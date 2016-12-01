//----------------------------------------------------------------------------
// opts
// {
//  height: width, 
//  width: depth,
//  linesHeight: b,
//  linesWidth: c,
//  color: 0xcccccc
// }
//
//____________________________________________________________________________
function createAGrid(opts) {
  var config = opts || {
    height: 500,
    width: 500,
    linesHeight: 10,
    linesWidth: 10,
    color: 0xDD006C
  };

  var material = new THREE.LineBasicMaterial({
    color: config.color,
      opacity: 0.2,
      linewidth: 2
  });

  var gridObject = new THREE.Object3D(),
    gridGeo = new THREE.Geometry(),
    stepw = 2 * config.width / config.linesWidth,
    steph = 2 * config.height / config.linesHeight;

  //width
  for (var i = -config.width; i <= config.width; i += stepw) {
    gridGeo.vertices.push(new THREE.Vector3(-config.height, i, 0));
    gridGeo.vertices.push(new THREE.Vector3(config.height, i, 0));

  }
  //height
  for (var i = -config.height; i <= config.height; i += steph) {
    gridGeo.vertices.push(new THREE.Vector3(i, -config.width, 0));
    gridGeo.vertices.push(new THREE.Vector3(i, config.width, 0));
  }

  var line = new THREE.Line(gridGeo, material, THREE.LinePieces);
  gridObject.add(line);

  return gridObject;
}

function labelAxis(width, data, direction){

  var separator = 2*width/data.length,
			p = {
				x:0,
				y:0,
				z:0
			},
			dobj = new THREE.Object3D();

  for ( var i = 0; i < data.length; i ++ ) {
      var label = makeTextSprite(data[i],{fontsize: 50 });

		label.position.set(p.x,p.y,p.z);

		dobj.add( label );
		if (direction=="y"){
			p[direction]+=separator;
		}else{
			p[direction]-=separator;
		}

  }
  return dobj;
}


// This was written by Lee Stemkoski
// https://stemkoski.github.io/Three.js/Sprite-Text-Labels.html
function makeTextSprite( message, parameters )
{
	if ( parameters === undefined ) parameters = {};

	var fontface = parameters["fontface"] || "Helvetica";
        var fontsize = parameters["fontsize"] || 70;
	var canvas = document.createElement('canvas');
	var context = canvas.getContext('2d');
	context.font = fontsize + "px " + fontface;

	// get size data (height depends only on font size)
	var metrics = context.measureText( message );
	var textWidth = metrics.width;


	// text color
        context.fillStyle = "yellow";

	context.fillText( message, 0, fontsize);

	// canvas contents will be used for a texture
	var texture = new THREE.Texture(canvas)
			texture.minFilter = THREE.LinearFilter;
			texture.needsUpdate = true;

	var spriteMaterial = new THREE.SpriteMaterial({ map: texture, useScreenCoordinates: false});
	var sprite = new THREE.Sprite( spriteMaterial );
	return sprite;
}

function getMeterScale(sep,meters) {
    var scale = [];
    var n = meters/sep;
    for(var i = 0; i < n; i++) {
	scale.push(""+i*sep+"m");
    }
    return scale;
}

function gridInit(glScene,gDimensions){

    var tickSeparationMeters = 0.5;

    // Y is the vertical dimension!
    // x is width, z is distance away
    var data = {
  labels: {
      y: getMeterScale(tickSeparationMeters,gDimensions.h),
      x: getMeterScale(tickSeparationMeters,gDimensions.w),
    z: getMeterScale(tickSeparationMeters,gDimensions.d)
  }
};


      var graphDimensions = {
	  w:gDimensions.w,
	  d:gDimensions.d,
	  h:gDimensions.h
      };

	var boundingGrid = new THREE.Object3D(),
			depth = graphDimensions.d/2, //depth
			width = graphDimensions.w/2, //width
			height = graphDimensions.h/2, //height
			a =data.labels.y.length,
			b= data.labels.x.length,
			c= data.labels.z.length;

	//pink
	var newGridXY = createAGrid({
				height: width,
				width: height,
				linesHeight: b,
				linesWidth: a,
				color: 0xcc0000
			});
			//newGridXY.position.y = height;
    newGridXY.position.z = -depth;
    newGridXY.position.y = height;
			boundingGrid.add(newGridXY);

    //blue
	var newGridYZ = createAGrid({
				height: width,
				width: depth,
				linesHeight: b,
				linesWidth: c,
				color: 0x0000cc
			});
	 		newGridYZ.rotation.x = Math.PI/2;
//	 		newGridYZ.position.y = -height;
			boundingGrid.add(newGridYZ);

	//green
	var newGridXZ = createAGrid({
				height: depth,
				width: height,
				linesHeight:c,
				linesWidth: a,
				color: 0x00cc00
			});

    newGridXZ.position.x = width;
        newGridXZ.position.y = height;
			//newGridXZ.position.y = height;
	 		newGridXZ.rotation.y = Math.PI/2;
			boundingGrid.add(newGridXZ);

    glScene.add(boundingGrid);

    var margin = 0.4;
	var labelsW = labelAxis(width, data.labels.x,"x");
			labelsW.position.x = width;
			labelsW.position.y = 0;
    			labelsW.position.z = -margin + -depth;
			glScene.add(labelsW);

	var labelsH = labelAxis(height, data.labels.y,"y");
			labelsH.position.x = width;
                        labelsH.position.y = -margin/2;
			labelsH.position.z = -depth;
			glScene.add(labelsH);

	var labelsD = labelAxis(depth, data.labels.z, "z");
                        labelsD.position.x = width;
                        labelsD.position.y = 0;
			labelsD.position.z = -margin + depth;
			glScene.add(labelsD);
};

