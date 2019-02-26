# Runs sns-teleop with workspace control
# Author: Simon Reisig
# Last edited: 2/6/18

sns start
sns run -d -r joy -- sns-joyd -a 6
sns run -d -r bg-ksim -- sns-ksim -y state -u ref
read -p "Enter 'w' for workspace control, 'j' for jointspace control: " ctrl_type
if [ $ctrl_type = w ]
then
    sns-teleopd -j joystick \
		-y state \
		-u ref \
		-w "robot_1_joint,robot_2_joint,robot_3_joint,robot_4_joint robot_5_joint,robot_6_joint,robot_7_joint" \
		-e "robot_7_joint"
fi

if [ $ctrl_type = j ]
then
    sns-teleopd -j joystick \
             -y state \
             -u ref \
             -Q 4 -m "robot_1_joint,robot_2_joint,robot_3_joint,robot_4_joint" \
             -Q 5 -m "robot_5_joint,robot_6_joint,robot_7_joint"
fi
