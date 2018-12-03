# Talk2Me
Comp50CP Final Project
This is a distributed 'characted' audiobook/screenplay reader in which different
computers function as different actors in reading the play or book.

## Getting Started
Talk2Me requires specific versions of Python in order
to function. Specifically >=__Python3.6__, otherwise Google
TTS will not work. To get the requirements we have provided
a requirements.txt file while you can install in the following way. 

```shell
pip3 install -r requirements.txt
```

Since the model used for training is slightly too large to
put onto github, you will need to download it [here](google.com) and place it into __src__.

From this point on we assume that you are using bash as
your shell. The following steps vary for other shells. 

### Erlport Integration

To get Erlport please refer to [here](http://erlport.org/) and
install as specified. Make sure you know the directory Erlport is installed in. From here you need to change two files. Within start_actor.py and start_talk.py are variables
call ERL_PORT_PATH which need to be set to Erlports path.

Technologies used:
Python 3.7 (Main development language)
Erlang (for ease of distribution)
Erlport (for connecting the two)
Google Cloud Text to Speech

