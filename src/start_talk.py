import os

os.system('export GOOGLE_APPLICATION_CREDENTIALS=\"./Talk2Me-bc8da1a1de18.json\"')

os.system('erl -sname mert -setcookie Cake -env ERL_LIBS ../../erlport -s distributor start_link')
