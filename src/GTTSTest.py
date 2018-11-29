from playsound import playsound
from playsound import playsound

#Iterate over the line and insert breaks at each .,: for realistic speech
def parseText(text):
    line = ""
    for word in text.split():
        print("word", word)
        if word[-1] == ".":
            word = word + "<break time=\"700ms\" />"
        elif word[-1] == ",":
            word = word + "<break time=\"450ms\" />"
        elif word[-1] == ":":
            word = word + "<break time=\"250ms\" />"
        word += " "
        line+= word
    print("text", text, "line", line)
    return line

def synthesize_ssml():
    """Synthesizes speech from the input string of ssml.

    Note: ssml must be well-formed according to:
        https://www.w3.org/TR/speech-synthesis/

    Example: <speak>Hello there.</speak>
    """
    from google.cloud import texttospeech
    client = texttospeech.TextToSpeechClient()
    ssml = "<speak>"
    line1 = "To be, or not to be, that is the question:"
    line2  = "Whether 'tis nobler in the mind to suffer"
    line3  = "The slings and arrows of outrageous fortune,"
    line4 = "Or to take arms against a sea of troubles"
    line5 = "And by opposing end them. To die—to sleep,"
    lines = [line1, line2, line3, line4, line5]
    for i in lines:
        ssml += parseText(i)
        ssml += "<break time=\"200ms\" />"
    ssml += "</speak>"
    print(ssml)
    input_text = texttospeech.types.SynthesisInput(ssml=ssml)

    # Note: the voice can also be specified by name.
    # Names of voices can be retrieved with client.list_voices().
    voice = texttospeech.types.VoiceSelectionParams(
        language_code='en-US',
        ssml_gender=texttospeech.enums.SsmlVoiceGender.FEMALE)

    audio_config = texttospeech.types.AudioConfig(
        audio_encoding=texttospeech.enums.AudioEncoding.MP3)

    response = client.synthesize_speech(input_text, voice, audio_config)

    # The response's audio_content is binary.
    with open('output.mp3', 'wb') as out:
        out.write(response.audio_content)
        print('Audio content written to file "output.mp3"')

    playsound("output.mp3")

synthesize_ssml()