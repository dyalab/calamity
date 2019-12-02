#!/bin/bash

trap "kill 0" EXIT              # Kill background processes if bash window is killed

start=""
goal=""
action=""

cd `dirname "$0"`

startFile=`pwd`/start.tmp
planFile=`pwd`/plan.tmp

firstLine="m robot_7_joint robot_6_joint robot_5_joint"
firstLine="$firstLine robot_4_joint robot_3_joint robot_2_joint robot_1_joint"
simStateChan="state"
simRefChan="ref"
run(){
    echo "starting at: $start"

    touch $planFile
    rm $planFile
    touch $planFile

    echo $firstLine >> $planFile
    echo "p$start" >> $planFile
    echo "p$goal" >> $planFile


    export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/
    export SNS_SCENE_NAME=schunk_on_table

    cd ../../schunk
    export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_on_table.so

    ach mk -1 action
    ach mk -1 change
    ach mk -1 state
    ach mk -1 ref

    echo "Simulating movement . . ."

    sns run -d -r bg-ksim -- sns-ksim -y $simStateChan -u $simRefChan
    snsref $simRefChan -p -- $start

    sleep 1

    sns_tmsmt_coms -y $simStateChan -u $simRefChan -a action -c change -p $planFile -vv -o
    echo "Hit enter to quit"
    read $ans
    sns kill bg-ksim
}

acc_start=""
acc_goal=""
acc_action=""
while test "x$1" != x-- -a "x$#" != x0; do
    key="$1"
    case $key in
        -s)
            acc_start="t"
            acc_action=""
            acc_goal=""
            ;;
        -g)
            acc_start=""
            acc_action=""
            acc_goal="t"
            ;;
        -a)
            acc_start=""
            acc_action="t"
            acc_goal=""
            ;;
        -h)
            cat <<EOF
Usage: sim-to-point -s start point -g goal point
EOF
            exit 0
            ;;
        *)
            if [ -n "$acc_start" ]
            then
                start="$start $key"
            elif [ -n "$acc_goal" ]
            then
                goal="$goal $key"
            elif [ -n "acc_action" ]
            then
                action="$action $key"
            fi
    esac
    shift
done

run sim
