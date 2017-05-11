---
layout: default
title: Gluss Main Page
---


# Gluss = Slug + Truss + Glass

## Premise 

The Gluss Project seeks to build Gluss, a robotic material that moves like a slug but is strong like a truss. We are exploring using networks of linear actuators connected in triangular shapes to build strong space frames that can ooze into position. This a version of the TETROBOT concept from 20 years ago made accessible to hobbyist.

The Gluss Project is a part of <a href="https://github.com/PubInv/PIFAH">Public Invention</a>, which seeks to invent things directly beneficial to all humanity without seeking profit or patents. The project was started in August, 2015.

Here is a summary of our presentation at SxSW Create, 2017:

<iframe width="560" height="315" src="https://www.youtube.com/embed/kYpHKjkcXKA" frameborder="0" allowfullscreen></iframe>
	  
## Status 

Currently the Gluss Project is Robert L. Read, full time, and Joshua Hannan and a number of students working as part time volunteers, although we welcome additional assistance.
We have acheived a crawling robot made of three tetrahedra. Technically, this a "5TetGlussbot", but we are seeking a more personal name.
<iframe width="560" height="315" src="https://www.youtube.com/embed/zl0AEfxyVMw" frameborder="0" allowfullscreen></iframe>

You may wish to subscribe to our <a href="https://www.youtube.com/watch?v=1T8XSMhwKUE">YouTube Channel</a>, since because motion is so intrinsic to the Gluss Project, videos are good way to show what we are doing.

The Glussbot uses commercial linear acuators and the "Turret Joint", an invention of Kwon, Song and Kim, implemented by Rob using a 3D printer. It is controlled by two Arduino Mega with an open-source shield we designed ourselves to allow up to 6 motors to be controlled with a single Mega. An Emacs lisp program communicates via bluetooth with the two controller, giving us a command line interface to control the robot.

## A Techical Paper

We have drafted a [technical paper](https://github.com/PubInv/gluss/blob/gh-pages/doc/Gluss.pdf) describing the gluss concept and some mathematically results relating to the Turret Joint.
If you are familiar with robotics conference, please recommend a place for us to submit this.

## Motivation 

Imagine a strong, light, metamorphic robotic substance. The uses of such a substance are limited only by our ability to imagine them.

Imagine a bridge that crawls into place in a matter of hours. Imagine constructing a temporary highway overpass in 24 hours, or a pedestrian bridge for a festival in the morning that comes down in the evening.

<figure>
	<img width="80%" src="https://github.com/PubInv/gluss/raw/gh-pages/images/robotTruckChasm.png"  alt="GlussBot Spannig Chasm, by WantedHero on Fiverr"/>
  <figcaption>GlussBot Spanning Chasm, by <a href="https://www.fiverr.com/wantedhero">WantedHero</a> on Fiverr.</figcaption>
</figure>




Imagine a snake-like robot that can crawl into a collapsed building and hold the roof up while survivors are extracted. Imagine material that combines the functionality of forklift, a crane, a bulldozer and a backhoe, all in one machine. Imagine a very small gluss that crawl into and holds shut a wound, temporarily taking the place of missing tissue.



It is hard to imagine humanity exploring the stars in peace without gluss at its side.

We've made a couple of videos showing physcial models and discussing our motivation:

<span>
<iframe width="280" height="157" src="https://www.youtube.com/embed/WWXpSSwL88s" frameborder="0" allowfullscreen></iframe>
<iframe width="280" height="157" src="https://www.youtube.com/embed/lLmkdvR1NCo" frameborder="0" allowfullscreen></iframe>

</span>


## How You Can Get Involved

We want to help the world by harnessing the creativity of amateur and professional makers.

We need competent and energetic artists, writers, video editors, designers, and thinkers. Those functions are just as important as the skills more obviously particular to this project. Perhaps it goes without saying that we also need programmers, Arduino programmers, 3D Printers, mechanical engineers, and electrical engineers. Depending on what you want to research, we also need mathematicians, structural engineers, mechanical engineers,  and roboticists.

### Contact

Because this is a genuine research project and everyone has different talents and interests, the best way to get involved is to email <a href="mailto: read.robert@gmail.com">Rob &lt;read.robert@gmail.com&gt;</a> directly to discuss what you are most interested in.

### Support us on Social Media

If you aren't ready to contact us directly, you can support us via social media by:
<ul>
<li> Subscribing to and liking videos on our <a href="https://www.youtube.com/watch?v=1T8XSMhwKUE">YouTube Channel</a>.
</li>
<li> Sharing this website and mentioning the Gluss concept to your friends.
</li>
</ul>

### Build Your Own Glussbot

You may want to build your own Glussbot for a playful or serious purpose. All of files needed to do this are published in our various GitHub Repos. The total process is not yet well-documented. It would be a great service to the project if someone would make or assist us in making an Instructable about any phase of the project. The Bill of Materials is about $1300 for the 3-TetGlussbot. We will happily assist you in this to improve our documentation.

### Use our Open-Source 6-controller Arduino Mega Shield

As part of this work, we need a Bluetooth-enabled microcontroller that could control up to six 0.6-amp motors. Our approach was to design an <a href="https://oshpark.com/shared_projects/fijpozoB">open-source shield</a> for the Arduino Mega. Any one can freely reuse this design or can order the board from OshPark and then solder it together themselves. We are unaware of any similar 6-channel controller. Promoting our controller helps promote the Gluss Project.

<img width="30%" src="./images/MotorControllerInPlace.JPG"/>

### Some specific things we need...

<ul>
<li> We need someone to sketch drawings of the "moving bridge" concept and other uses of Gluss to fire the imagine of would-be participants.
</li>
<li> We need someone familiar with the robotics community to tell us where to publish an academic paper explaining the gluss concept.
</li>
<li> We need several instructables produced, about the Turret Joint, the Glussbot, and the Mega shield.
</li>
</ul>
	  
These are <a href="https://github.com/PubInv/gluss/issues">issues</a> in GitHub. The GitHub systems allows you to comment on them, ask questions, and even assign them to yourself.<br>

<img width="30%" src="./images/Issues.png"/>

## Subprojects Spawned from The Gluss Project

The project has done a number of things so far that might be useful to others in some way. Everything we do is freely resuable under various licenses.

<ul>
<li>
We are very excited to have produced a functioning controller in the form of a miniature controller: the <a href="https://pubinv.github.io/GlussCon/">GlussCon</a>.
This has been a big hit amoung users.  We believe this an innovative and powerful way to control the non-rectillinear geometry of the glussbot.
A major priority is for us to build the next prototype of the GlussCon, which functions well but is fragile and messy at present.
</li>
<li>
We have discoverd a  <a href="http://pubinv.github.io/tetrahelix/">tetrahelix continuum.</a> We are prepare an academic paper. The website allows interactive
design of a continuum of tetrahelices. In particular, this allows us to "untwist" the Boerdijk-Coxeter helix, turning our actual 7TetGlussbot robot into a hexapod
robot.
</li>
<li>
The <a href="http://pubinv.github.io/turret-joint/">Turret Joint</a> Open-SCAD files are <a href="http://www.thingiverse.com/thing:1043716">
completely parameterizable</a>
and would allow anyone to make their own Turret Joints.
This includes for static applications.
For example, the Turret Joint can be thought of as a static construction system that allows somewhat more angular freedom than existing static construction systems.<br>
<img width="30%" src="./images/OpenSCADScreen.png"/>	  
</li>
<li>
The <a href="https://github.com/PubInv/S-Expr">Arduino S-Expr</a> project and its <a href="https://github.com/PubInv/Arduino-S-Expr-Test">test project</a> may be useful to anyone who wants to control an Arduino from Lisp or prefer S-Expressions to the closely related JSON format.
</li>
<li>
The <a href="https://oshpark.com/shared_projects/fijpozoB">3x2 Motor Controller MegaShield, v0.2</a> may be useful to anyone who wants to control up to six DC motors (up to 1 amp each) at the same time.<br>
<img width="30%" src="./images/NakedBoard.png"/>	  
</li>
<li>
The <a href="http://pubinv.github.io/CoilChoice/">CoilChoice</a> project may be valuable to anyone desiging coils for the purpose of producing Magnetomotive Force (such as solenoids.) The CoilChoice project was created when we were still attempting to build our own actuator (the GlussPusher project.)<br>
<img width="30%" src="./images/CoilChoiceScreen.png"/>	  
</li>
</ul>

## The Intellectual Underpinnings of The Gluss Project

Like most ideas, the idea of gluss is a melding of several pre-existing ideas:
<ul>
<li>
The pioneering work begun 20 years ago by Prof. Sanderson, Greg Hamlin, and others in building the TETROBOT was a crucial first step.
The glussbot represent a continuation of that work after a pause or diversion. (I have not been given permission to use the TETROBOT name.
I mention it a lot to draw attention to the original research, but tend to use the word "glussbot" for
the robots built by Public Invention, which are much smaller and use a different joint.)
</li>
<li>
<a href="https://en.wikipedia.org/wiki/Buckminster_Fuller">Buckminster Fuller</a> advocated <a href="https://en.wikipedia.org/wiki/Synergetics_(Fuller)">thinking</a> about structures as fully triangulated rigid structures.
<img width="30%" src="https://upload.wikimedia.org/wikipedia/commons/b/b2/Synergetics_abmod.gif"/>
</li>
<li>
We seek to dissolve the distinction between a machine and structure. Every machine is a structure, and every structure moves, if only while being assembled.
</li>
<li>
The Song, Kwon, Kim <a href="https://patents.google.com/patent/US20010002964A1/en">spherical joint</a> allows angular displacement for many members coming into a point.
</li>
<li>
Three-D printing lets us construct this "<a href="http://www.thingiverse.com/thing:1043716">turret joint</a>" inexpensively.<br/>
<img width="30%" src="./images/TurretJointPieces.png"/>
</li>
<li>
Biomimetics argues for mimicing animals, but we believe it is best to mimic the lower animals, such as single-celled <a href="https://en.wikipedia.org/wiki/Amoeba">amoebae</a> or molluscs which move by the production of pseudopods:  <img width="30%" src="https://upload.wikimedia.org/wikipedia/commons/thumb/3/3b/Amebas.jpg/484px-Amebas.jpg"/>
</li>
<li>
No discussion of pseudopods is complete without a mention of <a href="https://en.wikipedia.org/wiki/Shoggoth">shoggoths</a>:<br/> <img width="30%" src="https://upload.wikimedia.org/wikipedia/commons/8/83/Shoggoth_by_pahko.jpg"/>
</li>
<li>
...although many readers may be more comfortable with the concept of <a href="https://en.wikipedia.org/wiki/Flubber_(film)">Flubber</a>!
</li>
</ul>

## Next Steps and Research Directions 

The Gluss Projects engenders many research opportunities.
	  
### Build a Simulator

We need a software-only simulator so that gaits and other forms of motion can be investigated and developed without having to actually have Glussbot. Ideally this would have a 3D rendering display and allow both programatic and interactive design of motion.

Note: The simulator is in an early, poorly documented and buggy stage, but is available.

### Build a physical model with sensors

It would be magical to control a Glussbot by holding in your hands a small model of the robot that you can move and reshape. Such a model would not need acutators, but each member would need to sense and report its length. Then a change with your hands would immediately create a change in the Glussbot, allowing you to control it elegantly.

### Develop 3D Printable Magnetic Joints Similar to the GeoMag System

Although the turret joint is a valuale embodiment, it may be possible to build a larger version of the magnet-and-ball-bearing system known as "Geo Mag". Nothing is stopping us from testing this but the time it takes to develop mounts that holds magnets and mount to the Firgelli actuators.

Note: This has been done.  The results were that it works---but it does not scale up well.
It probably does scale down well, however, but we are no longer researching that.

### Explore more numerous tetrahedral Gluss systems

Our immediate plans are to build a 5-tetrahedron Glussbot and explore if it can crawl faster than the 3-Tet system. However, we could build and control significantly larger systems.

Note: We currently have built a 7-tetradra Glussbot system.

### Build a very small Glussbot

It is interesting to investigate what is the smallest Glussbot system we can construct.

### Build a Glussbot out of Hydraulic Actuators

Some gluss constructed out of the enormously powerful hydraulic actauators used on backhoes and bulldozers would be a fascinating machine, if only as an art project. Possibly the simple attachmend of a bulldozer blade would create an interesting machine that would actually be useful.

### Research Mathematics of Gluss Dancing

The current control system is very primitive, although it is able to accomplish crawling. It is now clear that we need a way to express and think about more complex and smooth motion over time---the choreography of a gluss dance. This math would be applicable to other sorts of robots as well.

#### Introduction of a Mathematical Approach to the Control Problem.

A very preliminary set of notes on a mathematic approach to the  
<a href="https://pubinv.github.io/gluss/controltheory.html">Control Theory</a>.





