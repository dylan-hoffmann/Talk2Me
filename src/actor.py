#!/usr/bin/env python3

import sys
import time

class Actor:

    def __init__(self, actorId=None):
        self.Id = actorId
        self.run()

    def run(self):
        while True:
            time.sleep(1)
            print(f"Listening for Actor {self.Id}")




myActor = Actor(sys.argv[1])