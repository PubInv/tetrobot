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

// If using a a Mega, you must use a pin that supports a change interrupt (10-15, and others.)
// int bluetoothTx = 2;  // TX-O pin of bluetooth mate, Arduino D2
// int bluetoothRx = 3;  // RX-I pin of bluetooth mate, Arduino D3

// typedef struct actuator Actuator;
const int NUM_ACTUATORS = 2;

// Chosen to deal with duty cycle
const int CRUISE_SPEED = 255;

actuator act[NUM_ACTUATORS];

// SoftwareSerial bluetooth(bluetoothTx, bluetoothRx);


void setup()
{
    Serial.begin(9600);  // Begin the serial monitor at 9600bps
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB
  }
  Serial.println("Serial port ready!");
 
/* 
  Serial.begin(9600);  // Begin the serial monitor at 9600bps

  bluetooth.begin(115200);  // The Bluetooth Mate defaults to 115200bps
  bluetooth.print("$");  // Print three times individually
  bluetooth.print("$");
  bluetooth.print("$");  // Enter command mode
  delay(100);  // Short delay, wait for the Mate to send back CMD
  bluetooth.println("V");  // Temporarily Change the baudrate to 9600, no parity
  bluetooth.println("U,9600,N");  // Temporarily Change the baudrate to 9600, no parity
  // 115200 can be too fast at times for NewSoftSerial to relay the data reliably
  bluetooth.begin(9600);  // Start bluetooth serial at 9600
  
  */
  
  /*
  Note: This code is specific to the Arduino Mega.  We are using Serial1, which is 
  pins 18 and 19.  This seems to be more reliable for me to 
  
  */
  
  Serial1.begin(115200);  // The Bluetooth Mate defaults to 115200bps
  Serial1.print("$");  // Print three times individually
  Serial1.print("$");
  Serial1.print("$");  // Enter command mode
  delay(100);  // Short delay, wait for the Mate to send back CMD
  Serial1.println("U,9600,N");  // Temporarily Change the baudrate to 9600, no parity
  // 115200 can be too fast at times for NewSoftSerial to relay the data reliably
  Serial1.begin(9600);
  
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
  
  pinMode(A0, INPUT);
  pinMode(A1, INPUT);
  
  for(int i = 0; i < 4 * 5; i++) {
      pinMode(34+i,OUTPUT);
  }
  
 
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
  Serial.println("SPUD");
  Serial.println(dist3(a,b));
  Serial.println(sqrt(sum));
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

void relax() {
  int vec[NUM_ACTUATORS];
  // First, lift
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    vec[i] = 500;
  }
  move_vector(NUM_ACTUATORS,vec);
}
// This would be far more elegant with streams...one in stream, one out stream --- don't know how to do this!
void main_controller() {
  if (Serial1.available()) {
    Serial1.println("YES!");
    bool terminator_not_found = true;
    char inChar = Serial1.read();
    Serial1.println(inChar);
    // This stuff is for the MotorShield
    if (inChar == 'u' || inChar == 'v' || inChar == 'w' || inChar == 'x') {
      int n = Serial1.parseInt();
      int coil = findByName(inChar);
      Serial1.println("coil =");
      Serial1.println(coil);
      if (n == 1) {
        deactivate_actuator(coil);
      } else {
      int dir = (n == 0) ? -1 : 1;
      Serial.println("Direction:");
      if (dir == 1) Serial.println("extend");
      if (dir == -1) Serial.println("retract");
      activate_actuators(coil,dir,255);
      }
    }
    
    if (inChar == 'r') {
      int n = Serial1.parseInt();
      if (n < 0 || n > 3) {
        Serial.println("bad actuator");
      } 
      int val = sensePosition(act[n].potPin);
      Serial.print("value of ");
      Serial.print(act[n].potPin);
      Serial.print(" = ");
      Serial.println(val);
      Serial.println("XXX");
    }
    
  
    
    if (inChar == 'a') {
      char comma = Serial1.read();
      int vec[NUM_ACTUATORS];
      for(int i = 0; i < NUM_ACTUATORS; i++) {
             vec[i] = Serial1.parseInt();
             if (i < (NUM_ACTUATORS - 1)) {
               comma = Serial1.read();
             }
      }
      OutputVector(NUM_ACTUATORS,vec);
      move_vector(NUM_ACTUATORS,vec);
    }
   
    if (inChar == 'j') {
      relax();
    }
    
    
    if (inChar == '\n') {
      terminator_not_found = false;
    }
    if (terminator_not_found) {
    while (!Serial1.available()) ;
    char terminator = Serial1.read();
    if (terminator != '\n') {
      Serial.print(terminator);
      Serial.println(" : Didn't read terminator successfully.");
    }
    }
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
   if(Serial1.available()>0)  // If the bluetooth sent any characters
  {
    // Send any characters the bluetooth prints to the serial monitor
    main_controller();
  }
  if(Serial.available()>0)  // If stuff was typed in the serial monitor
  {
    // Send any characters the Serial monitor prints to the bluetooth
    char c = (char)Serial.read();
    Serial.println(c);
    Serial1.println(c);
  }
}
