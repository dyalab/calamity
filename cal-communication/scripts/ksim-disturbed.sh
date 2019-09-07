cd `dirname "$0"`
../../schunk/create-scenegraphs.sh

export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/
export SNS_SCENE_NAME=circuit

cd ../../schunk
export SNS_SCENE_PLUGIN=`pwd`/.libs/q0.so

echo "cding into test to run SNS and planner"

ach mk ref -1
ach mk state -1
ach mk change -1
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

rm $plan_file
rm state-file.tmp

sns run -d -r bg-ksim -- sns-ksim -y state -u ref -c change
sns run -d -r sns-amino-ctrl --\
    sns_tmsmt_coms -y state -u ref -p $plan_file -a action -c change

cd ..
parent_dir=`pwd`
export TMSMT_SCENE_FILES="\"${parent_dir}/schunk/lwa4d_robot.urdf\" \"${parent_dir}/circuit-domain/q0.robray\" \"${parent_dir}/circuit-domain/allow-collision.robray\""
export TMSMT_DISTURB_STEP=30
export TMSMT_STDEV=0.1
export TMSMT_MOVE=2
export TMSMT_ADD=1
export TMSMT_REM=1

cd cal-communication/

gnome-terminal -x sbcl --eval '(require :cal-coms)' --eval '(cal-coms::sim-disturber)' --non-interactive
cd ../circuit-domain


export TMSMT_COMMUNICATION=cal-coms::write-file
export TMSMT_SCENE_UPDATE=cal-coms::read-new-scene
export TMSMT_INITIALIZE=cal-coms::initialize
export TMSMT_PLANNER=default-probabilistic-cpdl-plan
export TMSMT_LISP_ARGS="--dynamic-space-size 2048 --eval '(require :cal-coms)' "
export TMSMT_NO_CORE=t
export TMSMT_OPT="(:max-steps . 10)(:trace . t)(:state-change-func . cal-coms::read-multiple)"

sleep 5
tmsmt 	  circuits.pddl \
	  basic-test-probabilistic.pddl \
	  circuit-movement.py \
	  -g scene.robray \
	  -q q0.tmp \
	  -v\
	  -o ../test/plan.tmp

sns kill sns-amino-ctrl
sns kill bg-ksim

cd ../test
rm coms-plan.tmp
rm state-file.tmp
