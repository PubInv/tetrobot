import time
import serial
import os
import sys
import select

# configure the serial connections (the parameters differs on the device you are connecting to)
ser = serial.Serial(
    port='/dev/cu.2TETBOT-RNI-SPP',
#    port='/dev/cu.RNBT-D411-RNI-SPP',
    baudrate=9600,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_ONE,
    bytesize=serial.EIGHTBITS
)

# ser.open()
if(ser.isOpen() == False):
    ser.open()

print 'Enter your commands below.\r\n'

buff = ""

outbuff = ""

input=1
while 1 :

    # let's try to see which stream (stdin or bluetooth) we have input on
    while ser.inWaiting() > 0:
        cin = ser.read(1)
        outbuff += cin[0]
        if cin == "\n":
            print "|"+outbuff.rstrip()+"|"
            outbuff = ""

    while sys.stdin in select.select([sys.stdin], [], [], 0)[0]:
        line = sys.stdin.readline()
        if line:
            print "About to send: " + line + "\n"
            ser.write(line)
            ser.flush()
        else: # an empty line means stdin has been closed
            print('eof')
            exit(0)
    else:
        x = 4

    


        
