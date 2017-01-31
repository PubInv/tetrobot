// algebra
//
// math.js has support for symbolic computation (CAS). It can parse
// expressions in an expression tree and do algebraic operations like
// simplification and derivation on this tree.

// load math.js (using node.js)
var math = require('mathjs');

var red_phase = 0;
var yellow_phase = 1;
var blue_phase = 2;

var BCrot = math.acos(-2/3);

function H(n,lambda,color) {
    var pnt = [];
    var p0 = ((3.0*math.sqrt(3)/10.0) - 1.0/math.sqrt(3))*lambda + 1.0/math.sqrt(3);
    var p1 = (3/math.sqrt(10) - 1.0)*lambda + 1;
    pnt[0] = p1*n + color/3.0;
    pnt[1] = p0*math.cos(n*lambda*BCrot/3 + color*(2*math.pi/3));
    pnt[2] = p0*math.sin(n*lambda*BCrot/3 + color*(2*math.pi/3));
    return pnt;
}

// simplify an expression
console.log('Let us compute 100 points in a tetrahelix...');
var reds = [];
var blues = [];
var yells = [];
var lambda = 1.0;
for(var i = 0; i < 100; i++) {
    var red = H(i,lambda,red_phase);
    var blue = H(i,lambda,blue_phase);
    var yell = H(i,lambda,yellow_phase);
    reds.push(red);
    blues.push(blue);
    yells.push(yell);
    console.log(red,yell,blue);
}

for(var i = 0; i < 10; i++) {
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

