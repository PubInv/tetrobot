// algebra
//
// math.js has support for symbolic computation (CAS). It can parse
// expressions in an expression tree and do algebraic operations like
// simplification and derivation on this tree.

// load math.js (using node.js)
var math = require('mathjs');

var red_phase = math.bignumber(0);
var yellow_phase = math.bignumber(1);
var blue_phase = math.bignumber(2);

var BCrot = math.acos(math.divide(math.bignumber(-2),math.bignumber(3)));

function Cox(n) {
    console.log("cox",n);
    var pnt = [];
    var r = (3 * math.sqrt(3) / 10);
    var h = (1/math.sqrt(10));
    var theta = Math.acos(-2/3);
    pnt[0] = r * math.cos(n*theta);
    pnt[1] = r * math.sin(n*theta);
    pnt[2] = n*h;
    return pnt;
}

function CoxHelix(k,phase) {
    return Cox(k*3 +  math.number(phase));
}

function RedHelix(n) {
    var pnt = [];
    var r = (3 * math.sqrt(3) / 10);
    var h = (3/math.sqrt(10));
    var theta = Math.acos(-2/3);
    pnt[0] = r * math.cos(n * 3 * theta);
    pnt[1] = r * math.sin(n * 3 * theta);
    pnt[2] = n*h;
    return pnt;
    
}

// This seems to be working!! So now we have in fact defined a helix
// Note: I think this is correct, but the parameter is interpret as "node number".
// We would like a version of this parameterized by z position!
function ColorHelix(n,phase) {
    var pnt = [];
    var r = (3 * math.sqrt(3) / 10);
    var h = (1/math.sqrt(10));
    var theta = math.acos(-2/3);
    var slow = (3*theta - math.pi*2);
    var angle = slow*n+ phase*theta;
    pnt[0] = r * math.cos(angle);
    pnt[1] = r * math.sin(angle);
    pnt[2] = (n+phase/3) * 3 * h;
    return pnt;
}

// This is now working --- I need to comute an initial phase.
// NOTE: This seems to work for Lambda = 0.0 and lambda = 1.0, but
// not the intermediate values. I need to think carefully about it.


function ColorPHelix(n,phase,lambda) {
    var pnt = [];
    var r = (3 * math.sqrt(3) / 10);
    var rl = (r - 1/math.sqrt(3))*lambda + 1/math.sqrt(3);
    var h = (1/math.sqrt(10));
    var hl = (h - 1/3)*lambda + 1/3;
    var theta = Math.acos(-2/3);
    
    var thetal = theta*lambda 
    var itheta = (1.0 - lambda)*phase*2*math.pi/3;
    
    console.log("phase "+phase);
    console.log("thetal "+thetal);
    
    var slow = (3 * theta - math.pi*2);
    var angle = slow*n+ phase*theta;
    
    pnt[0] = rl * math.cos(angle);
    pnt[1] = rl * math.sin(angle);
    
    console.log("3*n Phase");
    console.log(((3*n)+phase)*hl);
    pnt[2] = (n+phase/3) * 3 * h;    
    return pnt;
    
}
function Helix(x,lambda,phase) {

//    var twist = 45*(math.pi / 180.0);
    var pnt = [];

    // radius must be 1/sqrt(3) when Lambda = 0, 3*sqrt(3)/10 when lambda = 1.0
    var sqrt3inverse = math.divide(math.bignumber(1),math.sqrt(math.bignumber(3)));
    
    var sqrt3over10time3 = math.divide(math.multiply(math.bignumber(3),math.sqrt(math.bignumber(3))),math.bignumber(10));
    console.log("sqrt3 inverse: " + sqrt3inverse)
    console.log("sqrt3inverse inverse: " + sqrt3over10time3)    
    
    var radius = math.add(math.multiply(math.subtract(sqrt3over10time3,sqrt3inverse),lambda),sqrt3inverse);
    console.log("radius: " + radius)    
    // the height factor determines the x displacements of nodes of the same color.
    // It is 1.0 when lambda is zero and 3/sqrt(10.0) when lambda = 1.
    var threeoversqrt10 = math.divide(math.bignumber(3),math.sqrt(math.bignumber(10))) 
    var h = math.add(math.multiply(
	math.subtract(threeoversqrt10,math.bignumber(1)),
	lambda),math.bignumber(1));
    pnt[0] = math.multiply(x,h);
    console.log("h: "+ h);
    var a = math.multiply(x,lambda);
//    var b = math.divide(BCrot,h);
    console.log("a: "+ a);
//    console.log("b: "+ b);
    //    var rotation = math.add(math.multiply(a,BCrot),phase);
    var rotation = math.multiply(a,BCrot);    
    console.log("rotation: "+ rotation);
    pnt[1] = math.multiply(radius,math.bignumber(math.cos(rotation)));

    pnt[2] = math.multiply(radius,math.bignumber(math.sin(rotation)));
    return pnt;
}

// simplify an expression
console.log('Let us compute 100 points in a tetrahelix...');
console.log("BCrot");
console.log(BCrot);
var reds = [];
var otherreds = [];
var blues = [];
var otheryells = [];
var yells = [];
var otherblues = [];
// var lambda = 1.0;
var lambda = 1.0;
var third = math.multiply(math.bignumber(120),(math.divide(math.bignumber(math.pi),math.bignumber(180.0))));    
for(var i = 0; i < 4; i++) {
//    var red = Helix(math.bignumber(i),lambda,math.multiply(third,red_phase));
//    var yell = Helix(math.bignumber((i+(1/3))),lambda,math.multiply(third,yellow_phase));
//    var blue = Helix(math.bignumber((i+(2/3))),lambda,math.multiply(third,blue_phase));
    var red = ColorHelix(i,red_phase);
    var otherr = ColorPHelix(i,math.number(red_phase),lambda);
    otherreds.push(otherr);
    var yell = ColorHelix(i,yellow_phase);
    var othery = ColorPHelix(i,math.number(yellow_phase),lambda);
    otheryells.push(othery);
    
    var blue = ColorHelix(i,blue_phase);
    var otherb = ColorPHelix(i,math.number(blue_phase),lambda);
    otherblues.push(otherb);
    
    reds.push(red);
    yells.push(yell);    
    blues.push(blue);
//    console.log(red,yell,blue);
}
console.log("reds");
console.log(reds);
console.log("otherreds");
console.log(otherreds);
console.log("yells");
console.log(yells);
console.log("otheryells");
console.log(otheryells);
console.log("blues");
console.log(blues);
console.log("otherblues");
console.log(otherblues);
for(var i = 0; i < 3; i++) {
    console.log("red");
//    console.log(math.distance([math.number(reds[i][0]),math.number(reds[i][1]),math.number(reds[i][2])],
//			      [math.number(reds[i+1][0]),math.number(reds[i+1][1]),math.number(reds[i+1][2])]));

    console.log("blue");
 //   console.log(math.distance([math.number(blues[i][0]),math.number(blues[i][1]),math.number(blues[i][2])],
//			      [math.number(blues[i+1][0]),math.number(blues[i+1][1]),math.number(blues[i+1][2])]));
   console.log(math.distance(blues[i],blues[i+1]));
    console.log("yellow");
    console.log(math.distance(yells[i],yells[i+1]));
 //   console.log(math.distance([math.number(yells[i][0]),math.number(yells[i][1]),math.number(yells[i][2])],
//			      [math.number(yells[i+1][0]),math.number(yells[i+1][1]),math.number(yells[i+1][2])]));
    
    console.log("orangeeven");
    console.log(math.distance(reds[i],yells[i]));

  //  console.log(math.distance([math.number(reds[i][0]),math.number(reds[i][1]),math.number(reds[i][2])],
//			      [math.number(yells[i][0]),math.number(yells[i][1]),math.number(yells[i][2])]));

  //  var red_dist = math.distance([math.number(reds[i][0]),math.number(reds[i][1]),math.number(reds[i][2])],
//			      [math.number(yells[i][0]),math.number(yells[i][1]),math.number(yells[i][2])])
    
    console.log("orangeodd");
    
    console.log(math.distance(yells[i],reds[i+1]));
  //  console.log(math.distance([math.number(yells[i][0]),math.number(yells[i][1]),math.number(yells[i][2])],
//			      [math.number(reds[i+1][0]),math.number(reds[i+1][1]),math.number(reds[i+1][2])]));

    console.log("purpleeven");
   console.log(math.distance(reds[i],blues[i]));
 //   console.log(math.distance([math.number(reds[i][0]),math.number(reds[i][1]),math.number(reds[i][2])],
//			      [math.number(blues[i][0]),math.number(blues[i][1]),math.number(blues[i][2])]));
    console.log("purpleodd");

 //   console.log(math.distance([math.number(blues[i][0]),math.number(blues[i][1]),math.number(blues[i][2])],
//			      [math.number(reds[i+1][0]),math.number(reds[i+1][1]),math.number(reds[i+1][2])]));

    console.log(math.distance(blues[i],reds[i+1]));

    console.log("greeneven");
    console.log(math.distance(yells[i],blues[i]));
 //   console.log(math.distance([math.number(yells[i][0]),math.number(yells[i][1]),math.number(yells[i][2])],
//			      [math.number(blues[i][0]),math.number(blues[i][1]),math.number(blues[i][2])]));
    
    console.log("greenodd");
    console.log(math.distance(blues[i],yells[i+1]));
//    console.log(math.distance([math.number(blues[i][0]),math.number(blues[i][1]),math.number(blues[i][2])],
//			      [math.number(yells[i+1][0]),math.number(yells[i+1][1]),math.number(yells[i+1][2])]));

 //   var green_odd_dist = math.distance([math.number(blues[i][0]),math.number(blues[i][1]),math.number(blues[i][2])],
//			      [math.number(yells[i+1][0]),math.number(yells[i+1][1]),math.number(yells[i+1][2])])

 //   console.log("longest by shortest");
    //  console.log(red_dist/ green_odd_dist);
    
}

