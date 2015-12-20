#include <EEPROM.h>
#include <stdlib.h>
#include <SoftwareSerial.h>
#include <math.h>
#include <assert.h>

int sign(int a) {
  return (a < 0) ? -1 : ((a == 0) ? 0 : 1); 
}

typedef struct {
  int forwardPin;
  int reversePin;
  int speedPin;
  int potPin;
  int nm;
  int minV = -99;
  int maxV = -99;
} actuator;

// typedef struct actuator Actuator;
const int NUM_ACTUATORS = 3;

// Chosen to deal with duty cycle
const int CRUISE_SPEED = 255;

actuator act[NUM_ACTUATORS];

// This is the only set of pints that seems to work!
// I am very confused by this.  Perhaps it is worth noting to a larger audience this problem ... why
// don't 14 and 15 work just as well?
const byte bluetoothTx = 12;
const byte bluetoothRx = 13;

SoftwareSerial bluetooth(bluetoothRx, bluetoothTx);


void setup()
{
  Serial.begin(9600);  // Begin the serial monitor at 9600bps
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB
  }
  Serial.println("Serial port ready!");

  bluetooth.begin(115200);  // The Bluetooth Mate defaults to 115200bps
  bluetooth.print("$");  // Print three times individually
  bluetooth.print("$");
  bluetooth.print("$");  // Enter command mode
  delay(100);  // Short delay, wait for the Mate to send back CMD
  bluetooth.println("U,9600,N");  // Temporarily Change the baudrate to 9600, no parity
  // 115200 can be too fast at times for NewSoftSerial to relay the data reliably
  bluetooth.begin(9600);
  
  act[0].forwardPin = 53;
  act[0].reversePin = 52;
  act[0].speedPin = 2;
  act[0].potPin = A0;
  act[0].nm = 'u';
  act[0].minV = 0;
  act[0].maxV = 1023;

  act[1].forwardPin = 51;
  act[1].reversePin = 50;
  act[1].speedPin = 3;
  act[1].potPin = A1;
  act[1].nm = 'v';
  act[1].minV = 0;
  act[1].maxV = 1023;
  
  act[2].forwardPin = 49;
  act[2].reversePin = 48;
  act[2].speedPin = 4;
  act[2].potPin = A2;
  act[2].nm = 'w';
  act[2].minV = 0;
  act[2].maxV = 1023;
  
  pinMode(A0, INPUT);
  pinMode(A1, INPUT);
  pinMode(A2, INPUT);
  
  for(int i = 0; i < 4 * 5; i++) {
      pinMode(34+i,OUTPUT);
  }
  
  delay(1000);
}

// 1 means extend, -1 means restract
void activate_actuators(int actuator,int direction,int strength)
{
  analogWrite(act[actuator].speedPin,strength);
    if (direction == 1) {
      digitalWrite(act[actuator].reversePin,LOW);
      digitalWrite(act[actuator].forwardPin,HIGH);     
    } else if (direction == -1) {
      digitalWrite(act[actuator].reversePin,HIGH);
      digitalWrite(act[actuator].forwardPin,LOW);     
  }
}

int findByName(char c) {
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    if (act[i].nm == c) {
      return i;
    }
  }
  Serial.println("NAME NOT FOUND");
  return -1;
}


void deactivate_actuator(int actuator) {
     digitalWrite(act[actuator].speedPin,LOW);
}

int sensePosition(int pin) {
  int val = analogRead(pin);
  return val;
}


void sensePositionVector(int n,int v[]) {
  for(int i = 0; i < n; i++) {
    v[i] = sensePosition(act[i].potPin);
  }
}

// This is probably obsolete
float dist3(int a[],int b[]) {
  float x = a[0];
  float y = a[1];
  float z = a[2];
  float q = b[0];
  float r = b[1];
  float s = b[2];
  return sqrt((x-q)*(x-q) + (y-r)*(y-r) + (z-s)*(z-s));
}

float dist(int n, int a[],int b[]) {
  float sum = 0.0;
  for(int i = 0; i < n; i++) {
    float ai = a[i];
    float bi = b[i];
    sum += ((ai - bi)*(ai - bi));
  }
 // Serial.println("SPUD");
 // Serial.println(dist3(a,b));
 // Serial.println(sqrt(sum));
  return sqrt(sum);
}


void move_vector(int n,int *vec) {
  const int tolerance = 50; // the maximum number of clicks in the "digital voltage space" of 0 - 1023 that we accept
  const float STUCK_DISTANCE = 1.0; // 3-Dimensional distance in the "digital voltage space" that we must move to not be "stuck"
  const int DELAY_TIME = 30; // Time to wait before making a move again
  const int MAX_STUCK = 4; // number of iterations to apply force before we give up as "stuck".
  
  int cval[n];
  int v[n];
  int dir[n];
  
  bool in_position = false;
  int stuck_cnt = 0;
     
   while ((!in_position) && (stuck_cnt < MAX_STUCK)) {  
     // Figure out which directions to move....
     sensePositionVector(n,cval);
     int max_diff = -1;
     int d[n];
     for(int i = 0; i < n; i++) {
       d[i] = vec[i] - cval[i];
       dir[i] = sign(d[i]);
       if (abs(d[i]) > max_diff)
         max_diff = abs(d[i]);
     }

     for(int i = 0; i < n; i++) {  
       // This should probably adjust speed for those that 
       // need to move less  
       float speed_ratio = (float) abs(d[i]) / (float) max_diff;
       activate_actuators(i,dir[i], (int) (speed_ratio * CRUISE_SPEED));
     }

// Wait a little bit for they physical move....
     delay(DELAY_TIME); 
     
     for(int i = 0; i < n; i++) {
       v[i] = cval[i];     
     }
     sensePositionVector(n,cval);
     
     // if we didn't move at all, increase stuck_cnt, so we don't
     // permanently spin our motors with no progress
     if (dist(n,v,cval) < STUCK_DISTANCE) {
       stuck_cnt++;
     } else {
       stuck_cnt = 0;
     } 
     
     in_position = true;
     for(int i = 0; i < n; i++) {
       if (abs(cval[i] - vec[i]) < tolerance) {
           deactivate_actuator(i);   
       } else {
           in_position = false;
       }
     }
   } 

  if (stuck_cnt >= MAX_STUCK) {
    Serial.println("GOT STUCK!");
  }

 for(int i = 0; i < n; i++) {
    deactivate_actuator(i);
 }
  Serial.println("Move done!");
}

void send_all_to(Stream* debug,int val) {
  int vec[NUM_ACTUATORS];
  // First, lift
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    vec[i] = val;
  }
  move_vector(NUM_ACTUATORS,vec);
}

void relax(Stream* debug) {
  send_all_to(debug,500);
}

void contract(Stream* debug) {
  send_all_to(debug,0);
}

void expand(Stream* debug) {
  send_all_to(debug,1023); 
}

void experiment(Stream* debug) {
  activate_actuators(2,-1,255);
}
// This would be far more elegant with streams...one in stream, one out stream --- don't know how to do this!
void main_controller(String str,Stream* debug) {
  switch(str[0]) {
    case 'j':
      relax(debug);
      debug->println("done with Relax.");
      break;
    case 'k':
      expand(debug);
      debug->println("done with Expand.");
      break;
    case 'l':
      contract(debug);
      debug->println("done with Contract.");
      break;
    case 'x':
      experiment(debug);
      debug->println("done with Experiment.");
      break;
  }
}

void OutputVector(int n,int v[]) {
   for(int i = 0; i < n; i++) {
     Serial.print(v[i]);
     Serial.print(" ");
   }
   Serial.println();
}

void loop()
{
   if(bluetooth.available()>0)  // If the bluetooth sent any characters
  {
    int x = bluetooth.available();
    String str = bluetooth.readStringUntil('\n');
    bluetooth.println(str);
    
    // Send any characters the bluetooth prints to the serial monitor
   main_controller(str,&bluetooth);
  }
  if(Serial.available()>0)  // If stuff was typed in the serial monitor
  {
    // Send any characters the Serial monitor prints to the bluetooth
    String s =  Serial.readStringUntil('\n');
    bluetooth.println(s);
  }
}
