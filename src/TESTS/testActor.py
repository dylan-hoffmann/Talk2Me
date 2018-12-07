import actor
import queue
import threading
from time import sleep
import random
from playsound import playsound

random.seed(None)

def addLines():
    while len(lineQ) != 0:
        line = lineQ.pop()
        actor.enqueue(line)
        sleep(random.random())

def sayLines():
    sleep(1)
    for i in range(lines):
        actor.speak()
        sleep(random.random())


actor.start("Hamlet", b'auB')
lineQ = []
with open("lines.txt") as f:
    for line in f:
        if len(line) > 1:
            lineQ.append(line)

lines = len(lineQ)
funcs = [threading.Thread(target=addLines, args=[], name="ADD"), threading.Thread(target=sayLines, args=[], name="SAY")]
for i in funcs:
    i.start()
for i in funcs:
    i.join()