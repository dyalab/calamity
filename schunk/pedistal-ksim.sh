#!/bin/bash

ach mk state -1
ach mk ref -1
ach mk joystick -1

cd `dirname "$0"`

# make sure the proper c files are created
aarxc lwa4d.urdf -n lwa4d -s lwa4d.robray
aarxc lwa4d.urdf -n lwa4d -o lwa4d.c
aarxc schunk_on_table.robray -n schunk_on_table -o schunk_on_table.c
aarxc schunk_with_table.robray -n schunk_with_table -o schunk_with_table.c
aarxc q0.robray -n q0 -o q0.c
aarxc goal.robray -n goal -o goal.c


if [ ! -z "$1" ]
then
    echo "SNS_SCENE_NAME=$1"
    export SNS_SCENE_NAME=$1
    export SNS_SCENE_PLUGIN=`pwd`/.libs/$1.so
else
    export SNS_SCENE_NAME=schunk_on_table
    export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_on_table.so
fi

make

sns start
sns run -d -r joy -- sns-joyd-logitech-f310 -a 6
sns run -d -r bg-ksim -- sns-ksim -y state -u ref

sns-teleopd -j joystick \
            -y state \
            -u ref \
            -Q 4 -m "robot_1_joint,robot_2_joint,robot_3_joint,robot_4_joint" \
            -Q 5 -m "robot_5_joint,robot_6_joint,robot_7_joint" \
	    -e "robot_ee_joint"\
	    -w

sns stop
