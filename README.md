
# Talk2Me
Comp50CP Final Project
This is a distributed 'characted' audiobook/screenplay reader in which different
computers function as different actors in reading the play or book.

## Getting Started
Talk2Me requires specific versions of Python in order
to function. Specifically >=__Python3.6__ and < __Python3.7__, otherwise Google
TTS will not work (for python3.6) and for python3.7 tensorflow will not work. To get the requirements we have provided a requirements.txt file while you can install in the following way.  

```shell
pip3 install -r requirements.txt
```

Since the model used for training is slightly too large to
put onto github, you will need to download it [here](google.com) and place it into __src__.

From this point on we assume that you are using bash as
your shell. The following steps vary for other shells. 

### Known Installation issues:

When running the code if you get the following error:
```shell
Import Error: No module named AppKit
```
or
```shell
Import Error: No module named PyObjC
```

Try running 
```shell
pip install pyobjc
```

Another known issue is with playsound and gi. If the following
python error message occurs:
```python
 Traceback (most recent call last):
 File "<stdin>", line 1, in <module>
 ImportError: No module named gi
```

A known solution is:
```shell
sudo apt-get install python3-gi
```

### Erlport Integration

To get Erlport please refer to [here](http://erlport.org/) and
install as specified. Make sure you know the directory Erlport is installed in. From here you need to change two files. Within start_actor.py and start_talk.py are variables
call ERL_PORT_PATH which need to be set to Erlports path.


### Google API Key

This project relies on a valid google API key and thus before
running any code ensure that proper path variables are set. This step depends on the shell, for bash variants do:

```shell
export GOOGLE_APPLICATION_CREDENTIALS="/PATH/TO/Key.json"
```

For tcsh

```shell
setenv GOOGLE_APPLICATION_CREDENTIALS /PATH/TO/Key.json
```


## Setup and running

After getting all the pre-requisites we have server and client
code. However, prior to running any of that please ensure that all environments are set up as if they are not the code will not work. Furthermore we define a file format for the script reader to take in.

### File formatting

```
PLAY_TITLE: NAME
CHARACTERS:
ACTOR1
ACTOR2
ACTOR3
END

ACTOR1. My line here

ACTOR2. My other line here
```

### Server Setup

To set up the server

```shell
python3 start_talk.py NAME
```

where NAME is a valid Erlang name (not sname).

After all actors are set up in erlang we start python 
and input our plays name.

```erlang
1(name)> distibutor:start_python().
Input PlayName:
```

### Client/Actor Setup

After getting the credentials set we start actors through

```shell
python3 start_actor.py NAME ACTORNAME SERVERNAME VOICE
```

NAME is your name@ip
SERVERNAME is servername@ip

The available voices are:

```python
{
    'auA':'en-AU-Wavenet-A', 'auB':'en-AU-Wavenet-B', 'auC':'en-AU-Wavenet-C',
    'auD':'en-AU-Wavenet-D', 'gbA':'en-GB-Wavenet-A','gbB':'en-GB-Wavenet-B', 
    'gbC':'en-GB-Wavenet-C', 'gbD':'en-GB-Wavenet-D', 'usA':'en-US-Wavenet-A',
    'usB':'en-US-Wavenet-B', 'usC':'en-US-Wavenet-C', 'usD':'en-US-Wavenet-D',
    'usE':'en-US-Wavenet-E', 'usF':'en-US-Wavenet-F'}

```

and in the VOICE selection you can type the short names i,e
auA, usC etc...

## Known issues:

Erlang server connection depends on firewalls to be disabled,
occasionally there have been issues regarding distribution 
with names depending WIFI behavior.


## Technologies used:
### Python 3.7 (Main development language)
### Erlang (for ease of distribution)
### Erlport (for connecting the two)
### Google Cloud Text to Speech

