from queue import Queue
from google.cloud import texttospeech
import threading
import time
import re
import numpy as np
import pandas as pd
import re, sys, os, csv, pickle

import itertools
import pprint
import re
import os
import json
from keras import regularizers, initializers, optimizers, callbacks
from keras.models import load_model
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.utils.np_utils import to_categorical
from keras.layers import Embedding
from keras.layers import Dense, Input, Flatten, Concatenate
from keras.layers import Conv1D, MaxPooling1D, Embedding, Dropout, LSTM, GRU, Bidirectional
from keras.models import Model
from keras import backend as K
from keras.engine.topology import Layer, InputSpec

from erlport.erlterms import Atom
from erlport.erlang import set_message_handler, cast


# The global constants below are designed for sentiment analysis

MAX_NB_WORDS = 40000  # max no. of words for tokenizer
MAX_SEQUENCE_LENGTH = 30  # max length of text (words) including padding
VALIDATION_SPLIT = 0.2
EMBEDDING_DIM = 200  # embedding dimensions for word vectors (word2vec/GloVe)

# Regex for matching speaking character
p = re.compile("([A-Z]+\.)")



manager = threading.Semaphore(0)


class Talk2Me:
        """ Main driver class, runs most of our other classes
        and provides setup for the producer consumer model

        """

    def __init__(self, actors=[], file=None, erlPid=None):
        """ Initializes Assisting Classes for Program Functionality

            Args:
                actors: A str list of actors in the play
                file: An open file where the file pointer is at the 
                    start of lines
                erlPid: Erlang PID for communication.

            Returns:
                None
        """

        self.actors = actors
        self.erlPid = erlPid
        self.queue = Queue()
        self.sentQ = Queue()
        self.parser = Parser(actors=self.actors, queue=self.queue, file=file,
                             sentQ=self.sentQ)
        self.scheduler = Scheduler(actors=self.actors, queue=self.queue,
                                   erlPid=erlPid)



class Parser:
    """ Parser extracts an Actor Name and associated line
    to send to scheduler.
    """

    def __init__(self, actors=[], queue=None, file=None, sentQ=None):
        """ Initializer for Parser

            Args:
                actors: A str list of actors in the play
                file: An open file where the file pointer is at the 
                    start of lines
                sentQ: A shared Queue between the scheduler and parser

            Returns:
                None

        """
        if actors is None:
            actors = []
        self.actors = actors  # List of all actors
        self.queue = queue  # Queue between Parser and Scheduler
        self.file = file  # Script 
        # Thread in which the parser runs - allows for concurrent parsing
        # and scheduling
        self.thread = threading.Thread(target=self.readFile,
                                       name=f"Parser {self.file}", args=[])
        self.thread.start()

    def readFile(self):
        """ Parses lines in file and
        Enqueues lines to scheduler. 
        """

        last = ""
        char = ""
        # Parse the file line by line
        for line in self.file:
            iterate = p.finditer(line)
            j = 0
            for i in iterate:
                j += 1
                start, end = i.span()
                if line[start:end - 1] in self.actors:
                    last = line[end:]
                    char = line[start:end - 1]
                    if start > 0:
                        last = line
                        start = 0
                    self.queue.put((char, line[end:]))


            if line[0] == " " and j == 0:
                # If Stage direction is given, line
                # is given to the NARRATOR
                self.queue.put(('NARRATOR', line))
                self.sentQ.put(('NARRATOR', line))
            elif j == 0 and line != "\n":
                last = last + line

                self.queue.put((char, line))




class Scheduler:
   
    """ This class gets actor:line pairs from the queue and distributes them
    to the appropriate actor
    """

    def __init__(self, actors=[], queue=None, erlPid=None):
        """ Initializer for the scheduler

            Args:
                actors: A str list of actors in the play
                file: An open file where the file pointer is at the 
                    start of lines
                erlPid: Erlang Pid to communicate with server

            Returns:
                None
        """

        global manager

        # Open our word tokenizer
        f = open('tokenizer.pickle', 'rb')
        self.tokenizer = pickle.load(f)
        K.clear_session()
        self.classes = ["neutral", "happy", "sad", "hate", "anger"]
        self.model_test = load_model('checkpoint-1.097.h5')
        
        self.model_test._make_predict_function()

        self.actors = actors
        self.semaphore = manager
        self.managerQ = Queue()

        # Thread for Synchronizer 
        self.managerThread = threading.Thread(target=self.cue, name="cue", args=[])
        self.erlPid = erlPid
        self.queue = queue
        self.thread = threading.Thread(target=self.run, name="Scheduler",
                                       args=[])
        self.thread.start()
        self.managerThread.start()

    def run(self):
        """ Main running loop for 
        scheduler. Start the scheduling
        process by sending lines out to 
        Erlang distribution
        """
        while True:
            time.sleep(0.1)
            char, line = self.queue.get()
            sent = self.get_sent(line)
            if char in self.actorDict:
                line = (line, sent[0])
                msg = (char, line)
                cast(self.erlPid, msg)
                self.managerQ.put(msg)


    def cue(self):
        """ Synchronizer for speech
        timing between actors.
        """
        while True:
            char, line = self.managerQ.get()
            print("Queueing")
            msg = (Atom(b'cue'), char)    
            cast(self.erlPid, msg)
            if char != "EFFECT":  
                print("Locking")
                self.semaphore.acquire()

    def get_sent(self, line):
        """ Given an input line runs sentiment
        analysis model to determine the relative
        Neutrality, Happiness, Angriness, Sadness
        and Hate of speech
        """
        
        parsedL = []
        parsedL.append(line)
        sequences_test = self.tokenizer.texts_to_sequences(parsedL)

        data_int_t = pad_sequences(sequences_test, padding='pre',
                                    maxlen=(MAX_SEQUENCE_LENGTH - 5))
        data_test = pad_sequences(data_int_t, padding='post',
                                    maxlen=MAX_SEQUENCE_LENGTH)

        y_prob = self.model_test.predict(data_test)
        return y_prob




def release(Actor):
    """ Releases global synchronizer
    lock given the condition that the 
    actor is not EFFECT
    """
    global manager
    Actor = ''.join(map(chr,Actor))

    if Actor != "EFFECT":
        manager.release()



def start(ErlangPid=None, PlayName=None):

    """Starts an Instance Talk2Me For a Play and Erlang PID

    Args:
        ErlangPid: A valid Erlang PID to send messages to.
        PlayName: File name of the associated play.

    Returns:
        None

    """

    PlayName = ''.join(map(chr,PlayName)) # Erlport Message Formatting
    PlayName = PlayName[:len(PlayName)-1]

    file = open(PlayName, 'r')

    actors = []
    line = ""
    actorRead = False
    # Preproccessing for Character Detection
    for line in file:
        if line == "END\n":
            break
        if len(line) > 11 and line[:12] == "PLAY_TITLE: ":
            print(f"Preparing to Read {line[12:]}")
        
        if actorRead == True:
            actors.append(line[:len(line)-1])

        if line == "CHARACTERS:\n":
            actorRead = True

    print(f"Total Detected Actors {actors}")
    a = Talk2Me(file=file, actors=actors, erlPid=ErlangPid)
