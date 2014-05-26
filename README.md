Compiling
============

csc main.scm


Executing
============

./main

Files
============

main.scm            : The main file that loads the program
inputs.scm          : Handle inputs keyboard

shaders/            : Shaders used to render the scene (Vertex/Fragment)

common/             : Files that are used to build the scene

    kruskal.scm     : Function of the Kruskal's algorithm
    prim.scm        : Function of the Prim's algorithm
    disjoint-set.scm: Set of functions to handle trees
    list-utils.scm  : Set of functions to handle lists like merging list or removing elements
    shape.scm       : Contain functions to generate shapes (like icosahedron and the tessellation code)
    maze.scm        : More function used to create the maze
    opengl.scm      : Helpers for opengl
    opengl-monkey.scm: Replace some opengl function with more scheme friendly functions
    vector.scm      : Some functions that makes it easier to know the actual byte size of a vector


common/loaders/obj.scm : A file that has a function that loads aliaswavefront obj file

geo/setup.scm       : Code that setup the scene (generate shapes etc)
geo/renderer.scm    : Code that render the scene
