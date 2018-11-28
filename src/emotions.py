

import numpy as np
import pandas as pd
import re, sys, os, csv, pickle

from datetime import date
from datetime import time
from datetime import datetime

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



def get_sentiment(line):


    with open('tokenizer.pickle', 'rb') as handle:
        tokenizer = pickle.load(handle)

    #print(parsedTweets)
    parsedL = []
    parsedL.append(line)
    classes = ["neutral", "happy", "sad", "hate","anger"]
    model_test = load_model('checkpoint-1.097.h5')
    sequences_test = tokenizer.texts_to_sequences(parsedL)
    #print(sequences_test)
    data_int_t = pad_sequences(sequences_test, padding='pre', maxlen=(MAX_SEQUENCE_LENGTH-5))
    data_test = pad_sequences(data_int_t, padding='post', maxlen=(MAX_SEQUENCE_LENGTH))
    y_prob = model_test.predict(data_test)
    
    print(y_prob)




get_sentiment(sys.argv[1])