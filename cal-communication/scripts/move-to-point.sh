goalPoint="$@"

firstLine="m robot_7_joint robot_6_joint robot_5_joint robot_4_joint robot_3_joint robot_2_joint robot_1_joint"


cd `dirname "$0"`

startFile=`pwd`/start.tmp
planFile=`pwd`/plan.tmp


gnome-terminal -x ./coms.sh 3 4 5 6 7 8 9

touch $startFile
rm $startFile
touch $startFile
snsdump realState motor_ref -s >> $startFile
sed 's/\t/ /g' $startFile >> $startFile

startPoint=`tail -1 $startFile`

touch $planFile
rm $planFile
touch $planFile

echo $firstLine >> $planFile
echo "p $startPoint" >> $planFile
echo "p $goalPoint" >> $planFile

export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/
export SNS_SCENE_NAME=schunk_on_table

cd ../../schunk
export SNS_SCENE_PLUGIN=`pwd`/.libs/schunk_on_table.so

ach mk -1 action
ach mk -1 change
ach mk -1 state
ach mk -1 ref

sns_tmsmt_coms -y state -u ref -a action -c change -vv -p $planFile
