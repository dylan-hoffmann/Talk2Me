import os
import sys

ERL_LIB_PATH = "../../erlport"

os.system('export GOOGLE_APPLICATION_CREDENTIALS="./Talk2Me-bc8da1a1de18.json"')

cmd = "erl -sname " + sys.argv[1] + " -setcookie Cake -env ERL_LIBS " + ERL_LIB_PATH +" -s distributor join " + sys.argv[2] + " " + sys.argv[3] + " " + sys.argv[4]
print(cmd)
os.system(cmd)
