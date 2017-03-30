

// uncomment as needed:

//flexbatterCR123A(n=2);
//rotate([0,0,0])translate([0,0,-9])flexbatter18650(n=1);
//translate([0,40,0])rotate([90,0,0])translate([0,0,-9])flexbatter18650(n=1);
//translate([0,80,0])rotate([180,0,0])translate([0,0,-9])flexbatter18650(n=1);
//translate([0,120,0])rotate([-90,0,0])translate([0,0,-9])flexbatter18650(n=1);
//translate([0,33,0])flexbatter18650(n=2);
// translate([0,90,0])

// This formula creates a bracket that can be used to hold 3 commercial battery holders
// mounted to the Firegelli shaft.
//difference() {
//difference() {
//scale([1.0,1.05,1.0])
//flexbatterWFirg18650(n=3);
//    translate([25,22,38])
//    color("red")
//    cube([80,63,70],center=true);  
//}
//color("green")
//    translate([-100,-20,-25])
//    cube(100,100,50);
//}

//translate([-90,33,0])flexbatter18650(n=4);
//translate([0,28,0])flexbatterAA(n=1);
//translate([0,50,0])flexbatterAAA(n=1);
//flexbatterC(n=1);
//flexbatterD(n=1);


// I'm now going to develop a completely new approach.
// I'm using a nickel-metal hyrdride battery pack. I want to mount it 
// the same way.  This should eventually be put into a different 
// file.

// Ropb adds this stuff do his onwn mounting!!
// The mount is aimed along the y instance
mount_length = 8.5;
mount_wall = 5;

module firg_mount() {  
    length = mount_length; // The Firgelli L16 are about 12.5 mm on a side
    wallside = mount_wall; // 
    outer = mount_length+2*wallside;
    color("red");
    difference() {
        cube([outer,outer,outer],center=true);
        cube([wallside+length,2*(wallside+length),wallside+length],center=true);
    }
}

module open_box(w,l,h,ww) {
    difference() {
        wo = w + 2*ww;
        lo = l + 2*ww;
        ho = h + 2*ww;
        color("blue")
        cube([wo,lo,ho],center=true);
        translate([0,0,ww])
        color("red")
        cube([w,l,h+ww*2],center=true);
    }
}
module battery_box() {
    bp_width = 30;
    bp_length = 53;
    bp_height = 72;
    width = 3;
    open_box(bp_width,bp_length,bp_height,width);
    mounts();
    
    separation = 30;
    length = mount_length; // The Firgelli L16 are about 12.5 mm on a side
    wallside = mount_wall; // 
    outer = mount_length+2*wallside; 
    
    translate([bp_width/2 + outer/2 + mount_wall/2,0,separation/2])
       rotate([90,0,0])
      firg_mount();
    
    translate([bp_width/2 + outer/2 + mount_wall/2,0,-separation/2])
       rotate([90,0,0])
      firg_mount();
}


battery_box();

