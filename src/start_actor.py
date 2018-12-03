import os
import sys


os.system('export GOOGLE_APPLICATION_CREDENTIALS=\"./Talk2Me-bc8da1a1de18.json\"')
cmd = "erl -sname " + sys.argv[1] + " -setcookie cake -env ERL_LIBS ../../erlport -s distributor join \"" + sys.argv[2] + "\" mert@mert-VirtualBox " + sys.argv[3]
os.system(cmd)
