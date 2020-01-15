#Sets up necessary environmental variables for runnning sns scripts
#Run via `source sns_tutorial_setup.sh`
#Author: Simon Reisig
#Last edited: 11/9/18


#Sets up environmental variables for specific robot
#You will need to edit the SNS_SCENE_PLUGIN filepaths depending on your system
read -p "Enter name of robot to set up for: " robot
if [ $robot = baxter ]
then
   export SNS_SCENE_NAME=baxter
   export SNS_SCENE_PLUGIN=~/Research/amino/demo/urdf/baxter/.libs/libamino_baxter.so
fi

if [ $robot = schunk ]
then
    export SNS_SCENE_NAME=lwa4d
    export SNS_SCENE_PLUGIN=$(cd ..; pwd)/schunk/.libs/libamino_schunk.so

    #Creates some ach channels specific for baxter.
    read -p "Create refs/states, define axes to remap for L/R/Head? (y/n): " make_states
    if [ $make_states = y ]
    then
	ach mk state_left
	ach mk state_right
	ach mk state_head
	ach mk ref_left
	ach mk ref_right
	ach mk ref_head

	export SNS_CHANNEL_MAP_state_left="left_s0,left_s1,left_e0,left_e1,left_w0,left_w1,left_w2"
	export SNS_CHANNEL_MAP_state_right="right_s0,right_s1,right_e0,right_e1,right_w0,right_w1,right_w2"
	export SNS_CHANNEL_MAP_state_head="head_pan"

	export SNS_CHANNEL_MAP_ref_left="$SNS_CHANNEL_MAP_state_left"
	export SNS_CHANNEL_MAP_ref_right="$SNS_CHANNEL_MAP_state_right"
	export SNS_CHANNEL_MAP_ref_head="$SNS_CHANNEL_MAP_state_head"
    fi
fi


#Creates generic ach channels
read -p "Create ach channels state, ref? (y/n): " state_ref
if [ $state_ref = y ]
then
    ach mk state
    ach mk ref
fi


#Creates some ach channels needed for joystick control
read -p "Make channels for joystick control (y/n)? " make_joystick
if [ $make_joystick = y ]
then
    ach mk joystick
fi
