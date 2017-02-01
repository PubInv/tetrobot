// algebra
//
// math.js has support for symbolic computation (CAS). It can parse
// expressions in an expression tree and do algebraic operations like
// simplification and derivation on this tree.

// load math.js (using node.js)
var math = require('mathjs');

var red_phase = 0.0;
var yellow_phase = 1.0;
var blue_phase = 2.0;

var BCrot = math.acos(-2.0/3.0);

function H(n,lambda,color) {

    var twist = 45*(math.pi / 180.0);
    var third = 120*(math.pi / 180.0);
    var pnt = [];
    var p0 = 3.0 * (((math.sqrt(3.0)/10.0) - 1.0/math.sqrt(3.0))*lambda + 1.0/math.sqrt(3.0));
    var p1 = 3.0 * ((1.0/math.sqrt(10.0) - 1.0)*lambda+ 1.0);
    pnt[0] = p1*(n + color/3.0);
    console.log("color third");
    console.log(color*third);
    console.log("twist");
    console.log(twist);
    console.log("n * lambda");
    console.log(n * lambda);
    console.log("inner");
    // The problem seems to be that I am computing the
    // twists straight (without using BC Rot.  That makes sense
    // if we are computing 3 distinct helices, but that is
    // not what the other formulat is doing.
//    var inner = (n*lambda*BCrot/3.0) + color*third + twist;
    var inner = ([(n*3.0) + (color)]*lambda*BCrot) + twist;    
    console.log(inner);
    pnt[1] = p0*math.cos(inner);
    pnt[2] = p0*math.sin(inner);
    
    console.log("y");
    console.log(pnt[1]);
    console.log("z");    
    console.log(pnt[2]);
    console.log("x");        
    console.log(pnt[0]);
    return pnt;
}

// simplify an expression
console.log('Let us compute 100 points in a tetrahelix...');
console.log("BCrot");
console.log(BCrot);
var reds = [];
var blues = [];
var yells = [];
// var lambda = 1.0;
var lambda = 0.0;
for(var i = 0; i < 100; i++) {
    var red = H(i,lambda,red_phase);
    var yell = H(i,lambda,yellow_phase);
    var blue = H(i,lambda,blue_phase);
    reds.push(red);
    blues.push(blue);
    yells.push(yell);
    console.log(red,yell,blue);
}

console.log(reds);
for(var i = 0; i < 30; i++) {
    console.log("red");
    console.log(math.distance(reds[i],reds[i+1]));
    console.log("blue");
    console.log(math.distance(blues[i],blues[i+1]));
    console.log("yellow");
    console.log(math.distance(yells[i],yells[i+1]));
    
    console.log("orangeeven");
    console.log(math.distance(reds[i],yells[i]));
    console.log("orangeodd");
    console.log(math.distance(yells[i],reds[i+1]));

    console.log("purpleeven");
    console.log(math.distance(reds[i],blues[i]));
    console.log("purpleodd");
    console.log(math.distance(blues[i],reds[i+1]));

    console.log("greeneven");
    console.log(math.distance(yells[i],blues[i]));
    console.log("greenodd");
    console.log(math.distance(blues[i],yells[i+1]));
    
}

