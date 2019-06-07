#########################
### Domain Parameters ###
#########################

# Table grid resolution
RESOLUTION=0.1

# End-Effector Frame
FRAME = "end_effector_grasp"

# Object placement tolerance
EPSILON = 1e-4



########################
### Imported Modules ###
########################

# Import our planning packages
import aminopy as aa
import tmsmtpy as tm

import TMSMT

# Import python math package
from math import pi

# The Common Lisp runtime is also available
import CL as cl


#############
## Helpers ##
#############

def map_locations(function, scene):
    for frame in tm.collect_frame_type(scene,"surface"):
        name = frame['name']
        for geom in frame['geometry']:
            shape = geom['shape']
            if aa.shape_is_box(shape):
                d = shape['dimension']
                x_max = d[0] / 2
                y_max = d[1] / 2
                x = 0
                i = 0
                while x <= x_max:
                    y = 0
                    j = 0
                    while y <= y_max:
                        function(name,i,j)
                        if i > 0: function(name,-i,j)
                        if j > 0: function(name,i,-j)
                        if i > 0 and j > 0: function(name,-i,-j)
                        y += RESOLUTION
                        j+=1
                    x +=  RESOLUTION
                    i+=1

############################
### Collision Constraint ###
############################

def collision_constraint(scene,op,objs):

    moveable = []
    def collect_moveable(frame_name):
        frame = scene[frame_name]
        if aa.frame_isa(frame,"moveable"):
            moveable.append(frame)
    map(collect_moveable, objs)

    conjunction = []

    def handle_moveable(frame):
        parent_name = frame.parent
        parent = scene[parent_name]
        if aa.frame_isa(parent, "surface"):
            conjunction.append(["AT", frame.name, parent_name])

    map(handle_moveable, moveable)

    return conjunction

##########################
## Scene State Function ##
##########################
def make_state(scene, configuration):
    '''Map the scene graph `scene' to a task state expression'''

    ## terms in the expression
    conjunction = []
    moveable_frames = tm.collect_frame_type(scene,"moveable")

    ## Add object locations
    handempty = [True]

    def add_on(child,parent):
        if parent == FRAME:
            conjunction.append(["HOLDING", child])
            conjunction.append(["NOT", ["HANDEMPTY"]])
            handempty[0] = False
        else:
            conjunction.append(["AT", child, parent])
            if ["OCCUPIED", parent] not in conjunction:
                conjunction.append(["OCCUPIED", parent])
                conjunction.append(["CONNECTED", parent])


    def handle_moveable(frame):
        name = frame.name
        parent_name = frame.parent
        add_on(name,parent_name)

    try:
        map(handle_moveable, moveable_frames)
    except NameError:
        moveable_frames = []    # No moveable frames

    if handempty[0]:
        conjunction.append(["HANDEMPTY"])

    return conjunction

def scene_state(scene,configuration):
    return make_state(scene, configuration)

def goal_state(scene,configuration):
    return make_state(scene, configuration)

############################
## Scene Objects Function ##
############################

def scene_objects(scene):
    '''Return the PDDL objects for `scene'.'''
    obj = []

    def type_names(thing):
        return [ f.name
                 for f in
                 tm.collect_frame_type(scene,thing) ]

    # Basic connection pieces
    wire = type_names("wire")
    wire.insert(0, "WIRE")

    # Switches
    switch = type_names("switch")
    switch.insert(0,"SWITCH")

    # Lamps
    lamp = type_names("lamp")
    lamp.insert(0, "LAMP")

    # Batteries
    battery = type_names("battery")
    battery.insert(0, "BATTERY")

    # Draw grid on surfaces
    location = type_names("location")
    location.insert(0, "LOCATION")


    return [wire, switch, lamp, battery, location]

############################
### Operator Definitions ###
############################


def motion_plan(op, frame, goal):
    scene = op.final_scene
    sub_scenegraph = aa.scene_chain(scene, "", frame)
    return tm.op_motion( op, sub_scenegraph, goal )

def place_tf(op, obj, dst_frame, g_tf_o ):
    mp =  motion_plan(op, obj, g_tf_o)
    return tm.op_reparent(mp, dst_frame, obj)

def place_height(scene,name):
    g = scene[name].collision
    s = g[0].shape
    if aa.shape_is_box(s):
        return s.dimension[2] / 2

def op_pick_up(scene, config, op):
    obj = op[1]
    dst = op[2]
    nop = tm.op_nop(scene,config)
    mp = motion_plan(nop, FRAME, tm.op_tf_abs(nop,obj))
    return tm.op_reparent(mp, FRAME, obj)

def op_put_down(scene, config, op):
    (a, obj, dst) = op
    nop = tm.op_nop(scene,config)
    g_tf_d = tm.op_tf_abs(nop,dst)
    return place_tf(nop, obj, dst, g_tf_d)


##########################
### Register functions ###
##########################

tm.bind_scene_state(scene_state)
tm.bind_goal_state(goal_state)
tm.bind_scene_objects(scene_objects)
tm.bind_refine_operator(op_pick_up, "PICK-UP")
tm.bind_refine_operator(op_put_down, "PUT-DOWN")

tm.bind_collision_constraint(collision_constraint)
