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

MAX_NB_WORDS = 40000 # max no. of words for tokenizer
MAX_SEQUENCE_LENGTH = 30 # max length of text (words) including padding
VALIDATION_SPLIT = 0.2
EMBEDDING_DIM = 200 # embedding dimensions for word vectors (word2vec/GloVe)




p = re.compile("([A-Z]+\.)")

class Talk2Me:

    def __init__(self, actors=[], file=None):

        self.actors    = actors
        self.queue     = Queue()
        self.sentQ     = Queue()
        self.parser    = Parser(actors=self.actors, queue=self.queue, file=file, sentQ=self.sentQ)
        self.scheduler = Scheduler(actors=self.actors, queue=self.queue)
        self.sentiment = Sentiment(self.sentQ)


    def run(self):
        pass

class Parser:


    def __init__(self, actors=[], queue=None, file=None, sentQ=None):
        self.actors = actors
        self.queue  = queue
        self.file   = file
        self.sentQ  = sentQ
        self.thread = threading.Thread(target=self.readFile, name=f"Parser {self.file}", args=[])
        self.thread.start()
    
    def readFile(self):

        last = ""
        char = ""
        for line in self.file:
            iterate = p.finditer(line)
            j = 0
            for i in iterate:
                j += 1
                start, end = i.span()
                if line[start:end-1] in self.actors:
                    last = line[end:]
                    char = line[start:end-1]
                    if start > 0:
                        last = line
                        start = 0
                    self.queue.put((char,line[end:]))
                    self.sentQ.put((char,line[end:]))
                    #print(f"{line[start:end]}")

            if line[0] == " " and j == 0:
                self.queue.put(('NARRATOR', line))
                self.sentQ.put(('NARRATOR', line))
            elif j == 0 and line != "\n":
                last = last + line

                self.queue.put((char,line))
                self.sentQ.put((char,line))
                
                




class Scheduler:


    def __init__(self, actors=[], queue=None):
        self.actors = actors
        self.actorQ = dict([(actor, Queue()) for actor in self.actors])
        self.actorObj = [Actor(name=self.actors[i], queue=self.actorQ[self.actors[i]]) for i in range(len(actors))]
        self.actorDict = {self.actors[i]: self.actorObj[i] for i in range(len(self.actors))}
        #print(self.actorQ)
        #self.actorThreads = [Thread()]
        
        
        self.queue = queue
        self.thread = threading.Thread(target=self.run, name="Scheduler", args=[])
        self.thread.start()


    def run(self):
        while True:
            time.sleep(0.1)
            char, line = self.queue.get()
            if char in self.actorDict:
                self.actorDict[char].readLine(line=line)
            #print(f"Scheduler cue'ing {char} with line {line}")

            

    def cue(self, actorId=None, line=None):
        pass




class Actor:

    def __init__(self, name="", voice=None, queue=None):
        self.name=name
        self.voice=voice
        self.queue = queue
        
    def __repr__(self):
        return f"actor {self.name}"

    def readLine(self, line=" "):
        #print(f"{self.name}: {line} in voice {self.voice}")
        pass

    def run(self):

        while True:

            line = self.queue.get()
            if line == None:
                break
            self.readLine(line=line)



class Sentiment:

    def __init__(self, queue):

        self.queue = queue
        self.thread = threading.Thread(target=self.run, name=f"Sentiment Run", args=[])
        self.thread.start()


    def run(self):
        with open('tokenizer.pickle', 'rb') as handle:
            tokenizer = pickle.load(handle)

    
        
        classes = ["neutral", "happy", "sad", "hate","anger"]
        model_test = load_model('checkpoint-1.097.h5')
    
        while True:
            line = self.queue.get()
            print(line)
            if line == None:
                break
            parsedL = []
            parsedL.append(line[1])
            sequences_test = tokenizer.texts_to_sequences(parsedL)
    #print(sequences_test)
            data_int_t = pad_sequences(sequences_test, padding='pre', maxlen=(MAX_SEQUENCE_LENGTH-5))
            data_test = pad_sequences(data_int_t, padding='post', maxlen=(MAX_SEQUENCE_LENGTH))
            y_prob = model_test.predict(data_test)
    
            print(y_prob)




file = open("Hamlet.txt", 'r')


actors = ["NARRATOR","KING", "QUEEN","HAMLET", "CLAUDIUS", "GHOST", "POLONIUS", "LAERTES", "OPHELIA", "HORATIO", "FORTINBRAS", "VOLTEMAND", "CORNELIUS", "ROSENCRANTZ", "GUILDENSTERN", "MARCELLUS", "BARNARDO", "FRANCISCO", "OSRIC", "REYNALDO", "FIRST CLOWN", "PRIEST", "LORDS", "FIRST AMBASSADOR"]
a = Talk2Me(file=file, actors=actors)