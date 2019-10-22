#!/bin/bash

trap "kill 0" EXIT 		# Kill background processes if bash window is killed

# com="$1"
# goalPoint="$@"
# realStateChan="realState"
# realRefChan="realRef"
# simStateChan="state"
# simRefChan="ref"

# firstLine="m robot_7_joint robot_6_joint robot_5_joint robot_4_joint robot_3_joint robot_2_joint robot_1_joint"


# cd `dirname "$0"`

# startFile=`pwd`/start.tmp
# planFile=`pwd`/plan.tmp

# touch $startFile
# rm $startFile
# touch $startFile


# snsdump $realStateChan motor_state -s >> $startFile
# sed 's/\t/ /g' $startFile >> $startFile

# startPoint=""
# for point in `tail -1 $startFile`; do
#     p=`echo $point | grep -o -E '[0-9.-]+'| head -1` # Grab the first number
#     startPoint="$startPoint $p"
# done

# echo "starting at: $startPoint"

# touch $planFile
# rm $planFile
# touch $planFile

# echo $firstLine >> $planFile
# echo "p $startPoint" >> $planFile
# echo "p $goalPoint" >> $planFile

# export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/
# export SNS_SCENE_NAME=schunk_on_table

# cd ../../schunk
# export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_on_table.so

# ach mk -1 action
# ach mk -1 change
# ach mk -1 state
# ach mk -1 ref

# echo "Simulating movement . . ."

# sns run -d -r bg-ksim -- sns-ksim -y $simStateChan -u $simRefChan
# snsref $simRefChan -p -- $startPoint
# read $ans
# sns_tmsmt_coms -y $simStateChan -u $simRefChan -a action -c change -p $planFile -vv -o
# read $ans
# sns kill bg-ksim

while test "x$1" != x-- -a "x$#" != x0; do
    key="$1"
    echo "var: $1"
    case $key in
	-s)
	    start=$2
	    ;;
	-g)
	    goal=$2
	    ;;
	-h)
	    cat <<EOF
Usage: sim-to-point -s start point -g goal point
EOF
	    exit 0
	    ;;
    esac
    shift
done
