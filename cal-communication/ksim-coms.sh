#!/bin/bash

export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/

cd `dirname "$0"`
cd ../schunk
echo "cding into schunk to make compiled scenegraph"
aarxc lwa4d.urdf -n lwa4d -s lwa4d.robray
aarxc lwa4d.urdf -n lwa4d -o lwa4d.c
aarxc schunk_on_table.robray -n schunk_on_table -o schunk_on_table.c
aarxc schunk_with_table.robray -n schunk_with_table -o schunk_with_table.c
aarxc q0.robray -n q0 -o q0.c
aarxc goal.robray -n goal -o goal.c
make

export SNS_SCENE_NAME=schunk_on_table
export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_on_table.so

echo "cding into circuit domain to run SNS and planner"
cd ../circuit-domain

ach mk ref -1
ach mk state -1
ach mk points -1

echo "starting SNS"

sns=$(sns ls | grep DEAD)

if [ -z "$sns" ]; then
    echo "SNS already started."
else
    sns start
fi

sns run -d -r bg-ksim -- sns-ksim -y state -u ref
sns run -d -r sns-amino-ctrl -- sns-amino-ctrl -y state -u ref -p points


export TMSMT_COMMUNICATION=cal-coms::communication
export TMSMT_LISP_ARGS="--eval '(require :cal-coms)'"
export TMSMT_NO_CORE=t

if [ $1 ]; then
    export TMSMT_PLANNER=default-probabilistic-cpdl-plan
    tmsmt ~/Git/calamity/schunk/lwa4d_robot.urdf \
	  q0.robray \
	  allow-collision.robray \
	  circuits.pddl \
	  basic-test-probabilistic.pddl \
	  circuit-movement.py \
	  -g scene.robray \
	  -q q0.tmp \
	  --gui\
	  -v
else
    export TMSMT_PLANNER=default-cpdl-plan
    tmsmt ~/Git/calamity/schunk/lwa4d_robot.urdf \
	  q0.robray \
	  allow-collision.robray \
	  circuits.pddl \
	  basic-test.pddl \
	  circuit-movement.py \
	  -g scene.robray \
	  -q q0.tmp \
	  --write-facts=test-facts\
	  --gui\
	  -v
fi

sns kill sns-amino-ctrl
sns kill bg-ksim
# sns stop
