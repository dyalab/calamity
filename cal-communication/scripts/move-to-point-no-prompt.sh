#!/bin/bash

trap "kill 0" EXIT              # Kill background processes if bash window is killed

com="can0"
goalPoint=$@
realStateChan="realState"
realRefChan="realRef"
simStateChan="state"
simRefChan="ref"

firstLine="m robot_7_joint robot_6_joint robot_5_joint robot_4_joint robot_3_joint robot_2_joint robot_1_joint"


cd `dirname "$0"`

startFile=`pwd`/start.tmp
planFile=`pwd`/plan.tmp

touch $startFile
rm $startFile
touch $startFile


snsdump $realStateChan motor_state -s >> $startFile
sed 's/\t/ /g' $startFile >> $startFile

startPoint=""
for point in `tail -1 $startFile`; do
    p=`echo $point | grep -o -E '[0-9.-]+'| head -1` # Grab the first number
    startPoint="$startPoint $p"
done

echo "Starting at: $startPoint"

touch $planFile
rm $planFile
touch $planFile

echo $firstLine >> $planFile
echo "p $startPoint" >> $planFile
echo "p $goalPoint" >> $planFile
echo "Goal point: $goalPoint"

export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/
export SNS_SCENE_NAME=schunk_on_table

cd ../../schunk
export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_on_table.so

ach mk -1 state_ft_sensor
ach mk -1 change
ach mk -1 state
ach mk -1 ref

sns_tmsmt_coms -y $realStateChan -u $realRefChan -a state_ft_sensor -c change -p $planFile -o -f 10
