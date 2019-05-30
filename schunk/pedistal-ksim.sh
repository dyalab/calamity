#!/bin/bash

ach mk state -1
ach mk ref -1
ach mk joystick -1

cd `dirname "$0"`
aarxc lwa4d.urdf -n lwa4d -o lwa4d.c
aarxc schunk_with_table.robray -n schunk_ped -o schunk_ped.c
aarxc schunk_on_table.robray -n schunk_table -o schunk_table.c
make

if [ ! -z "$1" ]
then
   echo "SNS_SCENE_NAME=$1"
   export SNS_SCENE_NAME=$1
   export SNS_SCENE_PLUGIN=`pwd`/.libs/$1.so
else
   export SNS_SCENE_NAME=schunk_ped
   export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_ped.so
fi

sns start
sns run -d -r joy -- sns-joyd-logitech-f310 -a 6
sns run -d -r bg-ksim -- sns-ksim -y state -u ref

sns-teleopd -j joystick \
             -y state \
             -u ref \
	     -v \
             -Q 4 -m "robot_1_joint,robot_2_joint,robot_3_joint,robot_4_joint" \
             -Q 5 -m "robot_5_joint,robot_6_joint,robot_7_joint" \
	     -e "robot_ee_joint"\
	     -w

sns stop
