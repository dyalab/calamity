#!/bin/bash

cd `dirname "$0"`
../../schunk/create-scenegraphs.sh

export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/
export SNS_SCENE_NAME=circuit

cd ../../schunk
export SNS_SCENE_PLUGIN=`pwd`/.libs/q0.so

echo "cding into test to run SNS and planner"

ach mk ref -1
ach mk state -1
ach mk reparent -1
ach mk action -1

echo "starting SNS"

sns=$(sns ls | grep DEAD)

if [ -z "$sns" ]; then
    echo "SNS already started."
else
    sns start
fi

cd ../test
plan_file=`pwd`/coms-plan.tmp

sns run -d -r bg-ksim -- sns-ksim -y state -u ref -r reparent
sns run -d -r sns-amino-ctrl --\
    sns_tmsmt_coms -y state -u ref -p $plan_file -a action -r reparent

cd ../circuit-domain


export TMSMT_COMMUNICATION=cal-coms::write-file
export TMSMT_SCENE_UPDATE=cal-coms::read-new-scene
export TMSMT_INITIALIZE=cal-coms::initialize
export TMSMT_LISP_ARGS="--eval '(require :cal-coms)'"
export TMSMT_NO_CORE=t
export TMSMT_OPT="(:max-steps . 10)(:trace . nil)(:state-change-func . cal-coms::read-action-channel)"

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
	  -v\
	  -o ../test/plan.tmp
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
	  -v\
	  -o ../test/plan.tmp
fi

sns kill sns-amino-ctrl
sns kill bg-ksim

cd ../test
rm coms-plan.tmp
# sns stop
