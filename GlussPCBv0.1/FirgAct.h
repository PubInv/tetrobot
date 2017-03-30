typedef struct {
  int forwardPin;
  int reversePin;
  int speedPin;
  int potPin;
  int nm;
  int minV;
  int maxV;
  int responsive; // This will be 1 if the items is found to be responsive. We start with an assumption of being unresponsive.
} actuator;
