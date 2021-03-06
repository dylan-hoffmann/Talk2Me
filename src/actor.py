import sys
import time
import queue
import threading
from playsound import playsound



speechQ = queue.Queue()
lineQ = queue.Queue()


# Possible list of available voices for actors
voiceMap = {'auA':'en-AU-Wavenet-A', 'auB':'en-AU-Wavenet-B', 'auC':'en-AU-Wavenet-C',
    'auD':'en-AU-Wavenet-D', 'gbA':'en-GB-Wavenet-A','gbB':'en-GB-Wavenet-B', 
    'gbC':'en-GB-Wavenet-C', 'gbD':'en-GB-Wavenet-D', 'usA':'en-US-Wavenet-A',
    'usB':'en-US-Wavenet-B', 'usC':'en-US-Wavenet-C', 'usD':'en-US-Wavenet-D',
    'usE':'en-US-Wavenet-E', 'usF':'en-US-Wavenet-F'}

class Actor:
    """ An actor for a given play that is designed
    to speak mp3 sound files of lines.
    """

    def __init__(self, actorId=None, voice=None):
        """ Initializer for an actor

            Args:
                actorId: The string name of the actor
                voice: The voice associated with the actor

        """
        global speechQ
        self.speechQ = speechQ
        self.Id = actorId
        self.voice = voice
        self.lineQ = lineQ
        self.thread = threading.Thread(target=self.run, name="Actor", args=[])
        self.thread.start()
        self.lineNum = 0

    def synthesize_ssml(self, text):
        """Synthesizes speech from the input string of ssml.

        Note: ssml must be well-formed according to:
            https://www.w3.org/TR/speech-synthesis/

        Example: <speak>Hello there.</speak>
        """
        from google.cloud import texttospeech
        client = texttospeech.TextToSpeechClient()
        ssml = "<speak>" + text + "</speak>"
        input_text = texttospeech.types.SynthesisInput(ssml=ssml)

        # Note: the voice can also be specified by name.
        # Names of voices can be retrieved with client.list_voices().
        voice = texttospeech.types.VoiceSelectionParams(
            language_code='en-US',
            ssml_gender=texttospeech.enums.SsmlVoiceGender.MALE,
            name=self.voice)

        # Set the output and speaking rate
        audio_config = texttospeech.types.AudioConfig(
            audio_encoding=texttospeech.enums.AudioEncoding.MP3,
            speaking_rate=0.8)

        response = client.synthesize_speech(input_text, voice, audio_config)

        # The response's audio_content is binary.
        file = "audio_line" + str(self.Id) + str(self.lineNum) + ".mp3"
        with open(file, 'wb') as out:
            out.write(response.audio_content)
            print('Audio content written to file', file)
        self.lineQ.put(file)
        self.lineNum += 1



    def parseText(self, text):
        """ Further parsing done to ensure
        proper breaks in speech given a line
        """
        line = ""
        sentiment = 1
        for word in text.split():
            print("word", word)
            if word[-1] == ".":
                word = word + "<break time=\"" + str(
                    700 * sentiment) + "ms\" />"
            elif word[-1] == ",":
                word = word + "<break time=\"450ms\" />"
            elif word[-1] == ":":
                word = word + "<break time=\"250ms\" />"
            word += " "
            line += word
        print("text", text, "line", line)
        return line

    def run(self):
        """ Listening process
        for an actor.
        """
        while True:
            line = speechQ.get()
            if line is None:
                break
            if type(line) is not str:
                line = ''.join(map(chr,line))
            print(f"Actor {self.Id} has picked up line {line}")
            line = self.parseText(line)
            self.synthesize_ssml(line)




def start(actorId, voice):
    """ Starts an instance of an actor

        Args: 
            actorId: A string associated name for the actor
            voice: Wavenet voice for the actor

    """
    
    if type(actorId) is not str:
        print("Not string")
        actorId = ''.join(map(chr,actorId)) # Erlang message formatting
    
    voice = ''.join(map(chr,voice))
    voice = voiceMap[voice]
    a = Actor(actorId=actorId, voice=voice)


def enqueue(line):
    """ Enqueues a line for an actor"""
    print(f"line is {line}")
    speechQ.put(line[0])

def speak():
    """ Cues an actor to speak the next
    line
    """
    print("Speaking")
    file = lineQ.get()
    playsound(file)
