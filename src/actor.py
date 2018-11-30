#!/usr/bin/env python3

import sys
import time
import queue
import threading


speechQ = queue.Queue()

class Actor:

    def __init__(self, actorId=None):
        global speechQ
        self.speechQ = speechQ
        self.Id = actorId
        self.thread = threading.Thread(target=self.run, name="Actor", args=[])
        self.thread.start()

    def run(self):
        while True:
            line = speechQ.get()
            print(f"Actor {self.Id} has picked up line {line}")


def start(actorId):


    a = Actor(actorId=actorId)



def enqueue(line):

    speechQ.put(line)

