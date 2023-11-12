import subprocess 
import os
while 1:
    command=input().split()
    if command[0]=="cd":    
        os.chdir(command[1])
    else:
        #subprocess.run(command,shell=True)
        a=subprocess.check_output(command)
        print(a.decode())