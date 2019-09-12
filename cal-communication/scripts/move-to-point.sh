#bin/bash

cd `dirname "$0"`

plan_file=`pwd`/plan.tmp

export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/
export SNS_SCENE_NAME=schunk_on_table

cd ../../schunk
export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_on_table.so

ach mk -1 action
ach mk -1 change
ach mk -1 state
ach mk -1 ref

sns_tmsmt_coms -y state -u ref -p $plan_file -a action -c change -vv
