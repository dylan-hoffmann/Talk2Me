######!/usr/bin/env python3

import sys
import time
import queue
import threading
from playsound import playsound



speechQ = queue.Queue()
lineQ = queue.Queue()


class Actor:

    def __init__(self, actorId=None):
        global speechQ
        self.speechQ = speechQ
        self.Id = actorId
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
            name='en-AU-Wavenet-B')

        # Set the output and speaking rate
        audio_config = texttospeech.types.AudioConfig(
            audio_encoding=texttospeech.enums.AudioEncoding.MP3,
            speaking_rate=0.8)

        response = client.synthesize_speech(input_text, voice, audio_config)

        # The response's audio_content is binary.
        file = "audio_line" + str(self.lineNum) + ".mp3"
        with open(file, 'wb') as out:
            out.write(response.audio_content)
            print('Audio content written to file', file)
        self.lineQ.put(file)
        self.lineNum += 1



    def parseText(self, text):
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
        while True:
            line = speechQ.get()
            if line is None:
                break
            print(f"Actor {self.Id} has picked up line {line}")
            line = self.parseText(line)
            self.synthesize_ssml(line)




def start(actorId):
    a = Actor(actorId=actorId)



def enqueue(line):
    speechQ.put(line)

def speak():
    file = lineQ.get()
    playsound(file)
