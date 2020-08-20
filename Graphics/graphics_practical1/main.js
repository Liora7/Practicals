
// Add any resources you want to load here
// You will then be able to reference them in initialise_scene
// e.g. as "resources.vert_shader"
RESOURCES = [
  // format is:
  // ["name", "path-to-resource"]
  ["vert_shader", "shaders/default.vert"],
  ["frag_shader", "shaders/default.frag"],
  ["vert_shader2", "shaders/vertex_Shader.vert"],
  ["frag_shader2", "shaders/fragment_Shader.frag"],
  ["vert_shader3", "shaders/vertex_ShaderAdv.vert"],
  ["frag_shader3", "shaders/fragment_ShaderAdv.frag"]
]

/*

    Create the scene

*/

function initialise_scene(resources) {
  // You can use your loaded resources here; resources.vert_shader will
  // be the content of the vert_shader file listed in RESOURCES, for
  // example

  // Set up the key parts of your renderer: a camera, a scene and the renderer
  var scene = new THREE.Scene();
  var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );
  camera.position.z = 150;

  var renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize( window.innerWidth, window.innerHeight );
  document.body.appendChild( renderer.domElement );

  var controls = new THREE.OrbitControls(camera, renderer.domElement);


//define material
/*var material = new THREE.ShaderMaterial({
	vertexShader: resources.vert_shader2,
	fragmentShader: resources.frag_shader2
});*/

var material = new THREE.ShaderMaterial({
  uniforms: {
      texture: { type: "t", value: THREE.ImageUtils.loadTexture('textures/brick.png') }
  },
	vertexShader: resources.vert_shader3,
	fragmentShader: resources.frag_shader3
});


// create shape out of cubes

// first side
for (var i=0; i < 8; i++){
  // create cube
  var geometry = new THREE.BoxGeometry(20, 20, 20);
  var cube = new THREE.Mesh(geometry, material);
  cube.rotation.x =  0.45;
  cube.rotation.y =  0.25;
  cube.position.x = i * 5;
  cube.position.y = i * -5;
  scene.add(cube);
}

// second side
for (var i=0; i < 8; i++){
  // create cube
  var geometry = new THREE.BoxGeometry(20, 20, 20);
  var cube = new THREE.Mesh(geometry, material);
  cube.rotation.x =  0.45;
  cube.rotation.y =  0.25;
  cube.position.x = i * 5;
  cube.position.y = i * 5;
  scene.add(cube);
}

// third side
for (var i=0; i < 8; i++){
  // create cube
  var geometry = new THREE.BoxGeometry(20, 20, 20);
  var cube = new THREE.Mesh(geometry, material);
  cube.rotation.x =  0.45;
  cube.rotation.y =  0.25;
  cube.position.x = 40 + i * 5;
  cube.position.y = 40 - i * 5;
  scene.add(cube);
}

// fourth side
for (var i=0; i < 8; i++){
  // create cube
  var geometry = new THREE.BoxGeometry(20, 20, 20);
  var cube = new THREE.Mesh(geometry, material);
  cube.rotation.x =  0.45;
  cube.rotation.y =  0.25;
  cube.position.x = 40 + i * 5;
  cube.position.y = -40 + i * 5;
  scene.add(cube);
}


/* add light */
var light = new THREE.HemisphereLight(0xFFFF00);
/* position the light so it shines on the cube (x, y, z) */
light.position.set(10, 0, 25);
scene.add(light);

  // Your animation loop, which will run repeatedly and renders a new frame each time
  var animate = function () {
    requestAnimationFrame( animate );
    renderer.render( scene, camera );
  };

  animate();
}





/*  Asynchronously load resources

    You shouldn't need to change this - you can add
    more resources by changing RESOURCES above */

function load_resources() {
  var promises = []

  for(let r of RESOURCES) {
    promises.push(fetch(r[1])
    .then(res => res.text()))
  }

  return Promise.all(promises).then(function(res) {
    let resources = {}
    for(let i in RESOURCES) {
      resources[RESOURCES[i][0]] = res[i]
    }
    return resources
  })
}

// Load the resources and then create the scene when resources are loaded
load_resources().then(res => initialise_scene(res))
