from playsound import playsound

#Iterate over the line and insert breaks at each .,: for realistic speech
def parseText(text):
    for word in text:
        for char in word:
            if char == ".":
                pass
    ssml = "<speak>" + text + "</speak>"
    return ssml

def synthesize_ssml():
    """Synthesizes speech from the input string of ssml.

    Note: ssml must be well-formed according to:
        https://www.w3.org/TR/speech-synthesis/

    Example: <speak>Hello there.</speak>
    """
    from google.cloud import texttospeech
    client = texttospeech.TextToSpeechClient()

    text = "To be, or not to be, that is the question: " \
           "Whether 'tis nobler in the mind to suffer " \
           "The slings and arrows of outrageous fortune, " \
           "Or to take arms against a sea of troubles " \
           "And by opposing end them. To die—to sleep,"

    ssml = parseText(text)

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