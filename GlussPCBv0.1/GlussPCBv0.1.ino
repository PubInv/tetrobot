#include "S-Expr.h"
#include "Arduino.h"
#include <stdlib.h>
#include <SoftwareSerial.h>
#include <math.h>
#include <assert.h>
#include "FirgAct.h"



int sign(int a) {
  return (a < 0) ? -1 : ((a == 0) ? 0 : 1); 
}

// Overall maximum speed: This can be used to limit amperage
const float MAX_AMPS = 3.6;
const float STALL_CURRENT_AMPS = 0.6;

// typedef struct actuator Actuator;
const int NUM_ACTUATORS = 6;

const int MAX_SPEED = (MAX_AMPS/(STALL_CURRENT_AMPS*NUM_ACTUATORS)) * 255;

// Chosen to deal with duty cycle
const int CRUISE_SPEED = MAX_SPEED;

actuator act[NUM_ACTUATORS];

// This is the only set of pints that seems to work!
// I am very confused by this.
const byte bluetoothTx = 12;
const byte bluetoothRx = 13;

SoftwareSerial bluetooth(bluetoothRx, bluetoothTx);

const int DEBUG = 5;
const int INFORM = 4;
const int WARN = 3;
const int ERROR = 2;
const int PANIC = 1;

int DEBUG_LEVEL = WARN;

int num_responsive = 0;

int responsive[NUM_ACTUATORS];


// NOTE: It is important to have sufficient battery power. The actuators don't move
// if you don't have sufficient battery power. It is impossible to tell if things
// are actuators are responsive if you do not have enough battery power.




// 1 means extend, -1 means restract
void activate_actuators(int actuator,int direction,int strength)
{
  //  Serial.println("Activating!");
  //  Serial.println("Forward PIN");
  //  Serial.println(act[actuator].forwardPin);
  //  Serial.println("Reverse PIN");
  //  Serial.println(act[actuator].reversePin);
  //  
  //  Serial.println("Speed PIN");
  //  Serial.println(act[actuator].speedPin);
  //    Serial.println("strength");
  //      Serial.println(strength);
  
  analogWrite(act[actuator].speedPin,strength);
  if (direction == 1) {
    digitalWrite(act[actuator].reversePin,LOW);
    digitalWrite(act[actuator].forwardPin,HIGH);  
    //      Serial.println("actuator"); 
    //      Serial.println(actuator);  
    //      Serial.println("reversePIN LOW"); 
    //     Serial.println(act[actuator].reversePin);
    //           Serial.println("forwardPIN HIGH"); 
    //     Serial.println(act[actuator].forwardPin);
  } else if (direction == -1) {
    digitalWrite(act[actuator].reversePin,HIGH);
    digitalWrite(act[actuator].forwardPin,LOW);  
    //         Serial.println("actuator"); 
    //      Serial.println(actuator);  
    //      Serial.println("reversePIN HIGH"); 
    //     Serial.println(act[actuator].reversePin);
    //           Serial.println("forwardPIN LOW"); 
    //     Serial.println(act[actuator].forwardPin);   
  }
}
void sensePositionVector(int n,int v[]);
  
void report_status(Stream* stream) {
    int cval[NUM_ACTUATORS];
    sensePositionVector(NUM_ACTUATORS, cval);
    stream->print("(status (list ");
    for(int i = 0; i < NUM_ACTUATORS; i++) {
      stream->print("'( ");      
      stream->print(i);
      stream->print(" . ");
      if (act[i].responsive) {
	stream->print(cval[i]);
      } else {
	stream->print(" nil");
      }
      stream->print(") ");
    }
    stream->print("))");
    stream->println();
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
  digitalWrite(act[actuator].reversePin,LOW);
  digitalWrite(act[actuator].forwardPin,LOW);  
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

void relax(Stream *);

const int RESPONSE_THRESHOLD = 30;
const int RESPONSE_DELAY_TIME = 300;
void compute_responsiveness(Stream* debug) {

  int cval[NUM_ACTUATORS];
  int rval[NUM_ACTUATORS];  // retract val
  int eval[NUM_ACTUATORS];  // extent val
   
  relax(debug);
  sensePositionVector(NUM_ACTUATORS,cval);
  // Let's try to retract, and see if it moves. If it moves, either in retraction or contraction,
  // the we will mark it responsive. 
  for(int i = 0; i < NUM_ACTUATORS; i++) { 
    act[i].responsive = 0;                    // mark unresponsive
    activate_actuators(i,-1,CRUISE_SPEED);
  }
  delay(RESPONSE_DELAY_TIME);
  
  sensePositionVector(NUM_ACTUATORS,eval);
  
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    if (act[i].responsive == 0) {   
      // We demand a positive motion 
      act[i].responsive = (((eval[i] - cval[i]) <= -RESPONSE_THRESHOLD) ? 1 : 0);
      Serial.println("SPUDX");
      Serial.println(i);
      Serial.println(eval[i] - cval[i]);
      Serial.println(((eval[i] - cval[i]) <= -RESPONSE_THRESHOLD) ? 1 : 0);
    }
  }  
  
  for(int i = 0; i < NUM_ACTUATORS; i++) {    
    activate_actuators(i,1,CRUISE_SPEED);
  }
  delay(RESPONSE_DELAY_TIME);
  sensePositionVector(NUM_ACTUATORS,rval);
 
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    deactivate_actuator(i);
  }

  
  for(int i = 0; i < NUM_ACTUATORS; i++) {   
    if (act[i].responsive == 0) {    
      act[i].responsive = (((eval[i] - rval[i]) >= RESPONSE_THRESHOLD) ? 1 : 0);
      Serial.println("SPUDY");
      Serial.println(i);
      Serial.println(abs(eval[i] - rval[i]));
    }  
  }
  
}

// This is desirable because we want to make
// sure that any debug statements are prefixed
// with comment characters.
void log_comment(Stream* debug,String str) {
  debug->print(";; ");
  debug->println(str);  
}
void log_comment(Stream* debug,int i) {
   debug->print(";; ");
   debug->println(i);  
}
void log_comment(int level,Stream* debug,String str) {
  if (level <= DEBUG_LEVEL) {
    debug->print(";; ");
    debug->println(str);
  }
}
void log_comment(int level,Stream* debug,int i) {
  if (level < DEBUG_LEVEL) {
    debug->print(";; ");
    debug->println(i);
  }
}

void find_responsive(Stream* debug) {
  for(int i = 0; i < NUM_ACTUATORS; i++) { 
    act[i].responsive = 0;
  }
  compute_responsiveness(debug);
  int num_responsive = 0;
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    log_comment(DEBUG,debug,i);
    //    debug->println(i);
    log_comment(DEBUG,debug,act[i].responsive);
  }
  log_comment(debug,"spud xxx");
  
  for(int i = 0; i < NUM_ACTUATORS; i++) { 
    log_comment(DEBUG,debug,i);
    log_comment(DEBUG,debug,act[i].responsive);
    if (act[i].responsive == 1) {
      responsive[num_responsive++] = i;
    } else {
      log_comment(ERROR,debug,"ACTUATOR_UNRESPONSIVE:");
      log_comment(ERROR,debug,(char) act[i].nm);
      //   log_comment(debug,act[i].responsive);
    }
  }
  //  if (num_responsive != NUM_ACTUATORS) {
  //     log_comment(debug,"WE'VE GOT UNRESPONSIVE ACTUATORS");
  //     log_comment(debug,NUM_ACTUATORS - num_responsive);
  //  }
}


void move_vector(Stream* debug,int n,int *vec) {
  const int tolerance = 50; // the maximum number of clicks in the "digital voltage space" of 0 - 1023 that we accept
  const float STUCK_DISTANCE = 1.0; // 3-Dimensional distance in the "digital voltage space" that we must move to not be "stuck"
  const int DELAY_TIME = 30; // Time to wait before making a move again
  const int MAX_STUCK = 4; // number of iterations to apply force before we give up as "stuck".
  const int MAX_TIME_MS = 8000;
  const int MAX_TURNS = MAX_TIME_MS / DELAY_TIME;
  
  int cval[n];
  int v[n];
  int dir[n];
  
  bool in_position = false;
  int stuck_cnt = 0;
  int total_turns = 0;
     
  while ((!in_position) && (stuck_cnt < MAX_STUCK) && (total_turns < MAX_TURNS)) {  
    total_turns++;
    log_comment(DEBUG,debug,total_turns);
    // Figure out which directions to move....
    sensePositionVector(n,cval);
    int max_diff = -1;
    int d[n];
    for(int i = 0; i < n; i++) {
      if (act[i].responsive) { // here we don't try to look for those that are unresponsive!
	d[i] = vec[i] - cval[i];
	dir[i] = sign(d[i]);
	if (abs(d[i]) > max_diff)
	  max_diff = abs(d[i]);
      }
    }

    log_comment(DEBUG,debug,"aaa");
    for(int i = 0; i < n; i++) {  
      // This should probably adjust speed for those that 
      // need to move less  
      float speed_ratio = (float) abs(d[i]) / (float) max_diff;
      log_comment(DEBUG,debug,"activating");
      if (act[i].responsive) { // don't move the unresponsive ones!
	activate_actuators(i,dir[i], (int) (speed_ratio * CRUISE_SPEED));
      } else {
	log_comment(DEBUG,debug,"unresponsive:");
	log_comment(DEBUG,debug,i);
      }
    }

    log_comment(DEBUG,debug,"bbb");
    // Wait a little bit for they physical move....
    delay(DELAY_TIME); 
     
    for(int i = 0; i < n; i++) {
      v[i] = cval[i];     
    }
    sensePositionVector(n,cval);
     
    log_comment(DEBUG,debug,"sense done");
     
    // if we didn't move at all, increase stuck_cnt, so we don't
    // permanently spin our motors with no progress
    if (dist(n,v,cval) < STUCK_DISTANCE) {
      stuck_cnt++;
      log_comment(DEBUG,debug,"stuck!");
    } else {
      //     stuck_cnt = 0;
    } 
     
    in_position = true;
    for(int i = 0; i < n; i++) {
      if (abs(cval[i] - vec[i]) < tolerance) {
	deactivate_actuator(i);   
      } else {
	in_position = false;
      }
    }
    log_comment(DEBUG,debug,"end loop");
  } 
  if (total_turns >= MAX_TURNS) {
    log_comment(WARN,debug,"MOVE TIMED OUT!");
  }

  if (stuck_cnt >= MAX_STUCK) {
    log_comment(WARN,debug,"GOT STUCK!");
  }
  log_comment(DEBUG,debug,"about to deactivate");
  for(int i = 0; i < n; i++) {
    deactivate_actuator(i);
  }
  log_comment(INFORM,debug,"Move done!");
  report_status(debug);
}

void send_all_to(Stream* debug,int val) {
  int vec[NUM_ACTUATORS];
  // First, lift
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    vec[i] = val;
  }
  move_vector(debug,NUM_ACTUATORS,vec);
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
  activate_actuators(0,-1,MAX_SPEED);
}


void OutputVector(Stream* stream,int n,int v[]) {
  for(int i = 0; i < n; i++) {
    stream->print(v[i]);
    stream->print(" ");
  }
}

void OutputVectorSerial(int n,int v[]) {
  OutputVector(&Serial,n,v);
}



/*

Our coal here is implement a "driver" for the 6-controller PCB board
that I designed.  Ideally this driver willl be usable for any gneral
purpose need to control 6 motors.  However, I am personally focused on
controlling 6 Firgelli Actuators in my Gluss robot.  So at the time of
this writing this is probably slanted for this purpose, and in fact
the board interfaces better to the Firgelli actuator hardware device,
which uses potentiometers to return position, than to generaly rotary
motors, although if you ignore that and use only the voltage wires,
you can control DC rotary motors.

There are various functions which exist purely for testing:

"l" - contract as much as possible
"j" - relax (to middle position) as much as possible
"k" - expand as much as possible

"(m a0 a1 a2 a3 a4 a5)" -- Move to these positions.

Status commands:
"s" - return the positional vector of all actuators (this should be
expanded to return the functional status of all acutators as well, but
that is for the future)

"a" - attempt to determine which actuators are functional or not

 */

/*
This function accepts Strings of the form "(m D+ D+ D+ D+ D+ D+)".
(Where the number of digits is equal to the number of Acutators.)
Exactly like that, no extra sapces.  Yes, it is a lisp form, but don't
be fooled --- I have not yet implemented a full parser, and won't until
I have a use-case driving it.

 */
String get_function_symbol(String str) {
  if (str[0] != '(') {
    return "error";
  } else {
    int in = str.indexOf(' ');
    if (in != 2) {
      return "error";
    } else {
      return str.substring(1,2);
    }
  }
}
int nth_number(String str,int n) {
  int pos = 0;
  pos = str.indexOf(' ',pos)+1;
  for (int i = 0; i < n; i++) {
    pos = str.indexOf(' ',pos)+1;
  }
  int spos = (n != 5) ? str.indexOf(' ') : str.indexOf(')');
  int val = str.substring(pos,spos).toInt();
  return val;
}

void interpret_function_as_sepxr(Stream *debug,String str) {
  sexpr* s = parse(str);
  String fun = value_s(nth(s,0));
  log_comment(PANIC,debug,"fun from s-Expression =");
    log_comment(PANIC,debug,fun);
  if (fun.equals("m")) {
    int ps[NUM_ACTUATORS];
    for(int i = 0; i <  NUM_ACTUATORS; i++) {
      ps[i] = value_i(nth(s,(i+1)));
    }
    // Now we invoke the "m" function, or movement....
    move_vector(debug,NUM_ACTUATORS,ps);
  } else if (fun.equals("p")) {

    // We will similarly move here, but we are looking for lists
    // within the list, rather than a simple.

    
    // First, fill out the position vector...
    int ps[NUM_ACTUATORS];
    sensePositionVector(NUM_ACTUATORS,ps);

    // now read each sublist to change the positions....
    int len = s_length(s);
    for(int i = 0; i <  len; i++) {
      sexpr* sub = nth(s,i);
      int j = value_i(nth(sub,0));
      int v = value_i(nth(sub,1));		      
      ps[j] = v;
    }
    
    move_vector(debug,NUM_ACTUATORS,ps);
  } else {
    log_comment(PANIC,debug,"Don't know how to handle:");
    log_comment(PANIC,debug,str);
  }
}

void main_controller(Stream* debug,String str) {
  switch(str[0]) {
  case 'a':
    find_responsive(debug);
    log_comment(INFORM,debug,"done with Calculate.");
    break;
  case 'j':
    relax(debug);
    log_comment(INFORM,debug,"done with Relax.");
    break;
  case 'k':
    expand(debug);
    log_comment(INFORM,debug,"done with Expand.");
    break;
  case 'l':
    contract(debug);
    log_comment(INFORM,debug,"done with Contract.");
    break;
  case 's': // status
    log_comment(INFORM,debug,"About to get status");
    report_status(debug);
    log_comment(INFORM,debug,"Done with status.");
    break;
  case '(':
    interpret_function_as_sepxr(debug,str);
    break;
  }
}


int r_cnt = 0;
void loop()
{
  r_cnt++;
  if(bluetooth.available()>0)  // If the bluetooth sent any characters
    {
      int x = bluetooth.available();
      Serial.println("x = ");
      Serial.println(x);
      String str = bluetooth.readStringUntil('\n');
      log_comment(INFORM,&bluetooth,str);
    
      // Send any characters the bluetooth prints to the serial monitor
      main_controller(&bluetooth,str);
    }
  if(Serial.available()>0)  // If stuff was typed in the serial monitor
    {
      // Send any characters the Serial monitor prints to the bluetooth
      String s =  Serial.readStringUntil('\n');
      bluetooth.println(s);
    }
}
void SetUpActuator(int i,actuator* a);

void setup()
{
  // It is possible that this is a real problem for battery powered operation!!!!
  Serial.begin(9600);  // Begin the serial monitor at 9600bps
  Serial.println("Serial port ready!");
  

  bluetooth.begin(115200);  // The Bluetooth Mate defaults to 115200bps
  bluetooth.print("$");  // Print three times individually
  bluetooth.print("$");
  bluetooth.print("$");  // Enter command mode
  delay(100);  // Short delay, wait for the Mate to send back CMD
  bluetooth.println("U,9600,N");  // Temporarily Change the baudrate to 9600, no parity
  // 115200 can be too fast at times for NewSoftSerial to relay the data reliably
  bluetooth.begin(9600);
  
  Serial.println("XXX!");

  log_comment(PANIC,&bluetooth,"Alive and listening");
  
  // This is my original code.  With the new (0.2) board I intend to order,
  // this will not be correct.  However for now I am trying to write 
  // even a different piece of code to make the (poorly designed v0.1 board work.
  //  for(int i = 0; i < NUM_ACTUATORS; i++) {
  //    
  //    // NOTE: On my particular board, pin # 45 is stuck HIGH for some reason.  This is a sign that we have to try to make
  //    // our whole system more fault tolerant.  However, in the mean time,  I will skip over that pair!
  //    if (i < 4) {
  //        act[i].forwardPin = 53 - (2*i);
  //        act[i].reversePin = act[i].forwardPin - 1;
  //    } else {
  //        act[i].forwardPin = 53 - (2*(i+1));
  //        act[i].reversePin = act[i].forwardPin - 1;
  //    }
  //     // In the Arduino Mega, the Anaglog pins A0, A1, etc, are numbered 54,55, etc.
  //     act[i].speedPin = 2+i;
  //     act[i].potPin = 54+i;
  //     act[i].nm = 'a' + i;
  //     act[i].minV = 0;
  //     act[i].maxV = 1023;
  //     act[i].responsive = 1;
  //  }
  
  // Note: I really hosed this up on the v0.1 board.

  for(int i = 0; i < NUM_ACTUATORS; i++) {
    
    SetUpActuator(i,&(act[i]));
    
    // Write all the acutators high because that is the only way to test!!!
    
    
  }
  
  for(int i = 0; i < NUM_ACTUATORS; i++) {
    pinMode(54+i,INPUT);
  }
  
  // Make sure the PWM pins are set to output for controlling speed.
  for(int i = 2; i < 9; i++) {
    pinMode(i,OUTPUT);
  }
  
  for(int i = 0; i < 4 * 5; i++) {
    pinMode(34+i,OUTPUT);
  }
  
  Serial.println("Setup done!");
  //  find_responsive(&Serial);
  // Now lets just put something into place to allow us to remark them unresponsive...
  
}

void SetUpActuator(int i,actuator* a) {
  a->forwardPin = 53 - 2*i;
  a->reversePin = a->forwardPin - 1;
    
  // In the Arduino Mega, the Anaglog pins A0, A1, etc, are numbered 54,55, etc.
  a->speedPin = 2+i;
  a->potPin = 54+i;
  a->nm = 'a' + i;
  a->minV = 0;
  a->maxV = 1023;
  a->responsive = 1;    
}
