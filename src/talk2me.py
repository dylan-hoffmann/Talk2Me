from queue import Queue
from google.cloud import texttospeech
import threading
import time
import re
import emotions


p = re.compile("([A-Z]+\.)")

class Talk2Me:

    def __init__(self, actors=[], file=None):

        self.actors    = actors
        self.queue     = Queue()
        self.parser    = Parser(actors=self.actors, queue=self.queue, file=file)
        self.scheduler = Scheduler(actors=self.actors, queue=self.queue)


    def run(self):
        pass

class Parser:


    def __init__(self, actors=[], queue=None, file=None):
        self.actors = actors
        self.queue  = queue
        self.file   = file
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
                    #print(f"{line[start:end]}")

            if line[0] == " " and j == 0:
                self.queue.put(('NARRATOR', line))
            elif j == 0 and line != "\n":
                last = last + line

                self.queue.put((char,line))
                
                




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
        print(f"{self.name}: {line} in voice {self.voice}")

    def run(self):

        while True:

            line = self.queue.get()
            if line == None:
                break
            self.readLine(line=line)


file = open("Hamlet.txt", 'r')


actors = ["NARRATOR","KING", "QUEEN","HAMLET", "CLAUDIUS", "GHOST", "POLONIUS", "LAERTES", "OPHELIA", "HORATIO", "FORTINBRAS", "VOLTEMAND", "CORNELIUS", "ROSENCRANTZ", "GUILDENSTERN", "MARCELLUS", "BARNARDO", "FRANCISCO", "OSRIC", "REYNALDO", "FIRST CLOWN", "PRIEST", "LORDS", "FIRST AMBASSADOR"]
a = Talk2Me(file=file, actors=actors)