import os
import sys


ERL_LIBS = "../../erlport"

os.system('export GOOGLE_APPLICATION_CREDENTIALS="./Talk2Me-bc8da1a1de18.json"')
os.system(f'erl -name {sys.argv[1]} -setcookie Cake -env ERL_LIBS {ERL_LIBS} -s distributor start_link')
