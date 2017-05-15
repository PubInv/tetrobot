// algebra
//
// math.js has support for symbolic computation (CAS). It can parse
// expressions in an expression tree and do algebraic operations like
// simplification and derivation on this tree.

// load math.js (using node.js)
const math = require('mathjs');
const assert = require('assert');


var red_phase = 0;
var yellow_phase = 1;
var blue_phase = 2;

var BCtheta = math.acos(math.divide(math.bignumber(-2),math.bignumber(3)));
var BCrho = 3*BCtheta - 2 * Math.PI;
var BCh = (1/math.sqrt(10));
var BCd = (3/math.sqrt(10));
var BCr = (3 * math.sqrt(3) / 10);

// This from https://github.com/josdejong/mathjs/blob/develop/lib/utils/bignumber/nearlyEqual.js
function nearlyEqual(x, y, epsilon) {
  // if epsilon is null or undefined, test whether x and y are exactly equal
  if (epsilon == null) {
    return x === y;
  }


  // use "==" operator, handles infinities
  if (x == y) {
    return true;
  }

  // NaN
  if (isNaN(x) || isNaN(y)) {
    return false;
  }

    // check numbers are very close, needed when comparing numbers near zero
    var diff = math.abs(x-y);
    if (diff == 0) {
      return true;
    }
    else {
	if (diff < epsilon)
	    return true;
    }

  // Infinite and Number or negative Infinite and positive Infinite cases
  return false;
};

function PColorHelix(n,phase,lambda) {
    var pnt = [];
    var r = (3 * math.sqrt(3) / 10);
    var rl = (r - 1/math.sqrt(3))*lambda + 1/math.sqrt(3);
    var h = BCh;
    var hl = (h - 1/3)*lambda + 1/3;    
    var x = n+phase/3.0;
    var theta = math.acos(-2/3);
    var slow = (3*theta - math.pi*2);
    var third = 2*math.pi/3.0;
    // Note the subtle distinct here...
    // in fact:
    //   slow *(n+phase/3.0) + phase*third == slow * n + phase*theta!
    //
    var angle = slow * x * lambda+ phase*third;
    pnt[0] = rl * math.cos(angle);
    pnt[1] = rl * math.sin(angle);
    pnt[2] = (x) * 3 * hl;
    return pnt;
}

function find_drho_from_r(rho,r) {
    var sin_r_2 = math.sin(rho/2);
    var d = math.sqrt(1 - 4 * r * r * sin_r_2 * sin_r_2);
    return d;
}

function find_rrho_from_d(rho,d) {
    var sin_r_2 = math.sin(rho/2);
    console.log("sin_r_2",sin_r_2);
    console.log("math.sqrt(1 - d*d)",math.sqrt(1 - d*d));
    console.log("2 * sin_r_2", 2 * sin_r_2);
    var r = math.sqrt(1 - d*d) / (2 * sin_r_2);
    return r;
}


function H_general(n,c,rho,d,r) {
    var pnt = [];
    var kappa = n+ c/3.0;
    var rk = rho*kappa;
    var angle = rk + c*2*math.PI/3;
    pnt[0] = r*math.cos(angle);
    pnt[1] = r*math.sin(angle);
    pnt[2] = d*kappa;
    return pnt;
}

function H_bc(n,c) {
    var BCr = find_rrho_from_d(BCrho,BCd);
    return H_general(n,c,BCrho,BCd,BCr);
}


// WARNING: This is an error...it is not at all clear what this should be.
// The BC helix has a particular value, but unless we can compute
// a minimization error and hope that we can use interpolation thereof,
// then this will be difficult.
function H_bc_lambda(n,c,lambda) {
    // Note this is a particular scheme for parametrization.
    // Now we must compute r and h....
//    var h = BCh;    
    var dl = (BCd - 1)*lambda + 1;
    var rhol = BCrho * lambda;
    // having fixed dl and rhol, we must find r    
    var rl = (lambda == 0) ?  1/math.sqrt(3) : find_rrho_from_d(rhol,dl);
    console.log("rl");
    console.log(rl);
    return H_general(n,c,rhol,dl,rl);
}

function test_rail_angle_formula_against_BC() {
    var BCr_check = find_rrho_from_d(BCrho,BCd);
    var BCd_check = find_drho_from_r(BCrho,BCr);
    assert(nearlyEqual(BCr,BCr_check,0.0000001),
	   'BCr nearly equal fail '+BCr+', BCr_check == '+BCr_check);    
    assert(nearlyEqual(BCd,BCd_check,0.0000001),
	   'BCd == '+BCd+', BCd_check == '+BCd_check);
}

function test_H_general_against_BC() {
    var R0 = H_bc(0,0);
    var R1 = H_bc(1,0);
    var Y0 = H_bc(0,1);
    var B0 = H_bc(0,2);
    assert(nearlyEqual(math.distance(R0,R1),1,0.0000001),"\n R0 = " + R0 +"\n R1 = " + R1 +"\n distance =" + math.distance(R0,R1));
    assert(nearlyEqual(math.distance(R0,Y0),1,0.0000001),"\n R0 = " + R0 +"\n Y0 = " + Y0 +"\n distance =" + math.distance(R0,Y0));
    assert(nearlyEqual(math.distance(R0,B0),1,0.0000001),"\n R0 = " + R0 +"\n B0 = " + B0 +"\n distance =" + math.distance(R0,B0));

}

// simplify an expression
console.log('Let us compute 100 points in a tetrahelix...');
console.log("BCtheta");
console.log(BCtheta);

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

    //    var red = H_bc_lambda(i,red_phase,lambda);
    var red = H_bc(i,red_phase);    
    var otherr = PColorHelix(i,red_phase,lambda);
    otherreds.push(otherr);
    
    //    var yell = H_bc_lambda(i,yellow_phase,lambda);
    var yell = H_bc(i,yellow_phase);            
    var othery = PColorHelix(i,yellow_phase,lambda);
    otheryells.push(othery);
    
    //    var blue = H_bc_lambda(i,blue_phase,lambda);
    var blue = H_bc(i,blue_phase);        
    var otherb = PColorHelix(i,blue_phase,lambda);
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
/*    for(var j = 0; j < 1; j = j + 0.1) {
	var azero = NColorHelix(j,red_phase);
	var yzero = NColorHelix(j,yellow_phase);
	//	var bzero = NColorHelix(j,blue_phase);
	var bzero = NColorHelix(j,blue_phase);	
	console.log(azero,yzero,bzero);
	// These angle calculations appear to be wrong.
	var ar = Math.tan(azero[1]/azero[0]);	
	var ay = Math.tan(yzero[1]/yzero[0]);
	var ab = Math.tan(bzero[1]/bzero[0]);
	console.log(180*ar/math.pi,180*ay/math.pi,180*ab/math.pi);
    }
*/
    
}

test_rail_angle_formula_against_BC();
test_H_general_against_BC();
