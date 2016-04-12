# gluss

You probably want to visit the [public website](http://pubinv.github.io/gluss) first.

## Update as of Feb 16th

Today a major milestone was acheived!  My open-source [PCB board](https://oshpark.com/shared_projects/5xxBXrSD) available at OSH Park supported simultaneous motion of 6 actuators (at low speed).

This is a major breakthrough. This is in fact an Arduino Mega shield, and provides all of the robustness (physically and electrically) of a shield.  This is far superior to my previous appraoch you can see in my vidoes, which required multiple shields and multiple arduinos.  (I still need a Mega for each 6 actuators.)

The board also support BlueTooth, which means the robot should now be remotely controllable.

Next steps are:

* Test at higher speed (to make sure I don't burn up the Arduino Mega power trace
* Test Bluetooth range
* Build the second controller
* Mount the system on the robot
* Start working on code to allow a "gait" that walks.

## Previous

Nascent repo to hold information about Gluss robots in general (as opposed to GlussPusher, my attempt to make cheap linea
actuators.)

TODO:
* Develop the dual-tetrahedra based robot and make it walk.
* Complete the tetrahelix joint 3D printable design and create enough for us to build the Dual-Tet robot.
* Create a rolling octahedron just to prove that we can do it.
* Create a joint with larger extent to act as "Feet" for the Dual-Tet, which is barely flat enough. Use existing rotor
structure to do this.
* Purchase some large ball bearings in order to test possibility of magnetic joints
* Design Firgelli mounts to hold magnets and print enough to try it.
* order Arduino Mega and sufficient h-drivers to control the Dual-tet
* Get BlueTooth control working.
* Design "folding joint" to make an edge based on servos -- low priority.
