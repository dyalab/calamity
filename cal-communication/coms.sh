#!/bin/bash
#Sets up necessary variables to run socanmatic to communicate with the
#Schunk arm.
#Author: Matthew Schack



run(){
    echo "Initalizing CAN device..."
    up=$(ip link | grep $1 | grep UP)
    if [ -z "$up" ]; then
	sudo ip link set dev $1 up type can bitrate 500000
	echo "CAN device created"
    else
	echo "CAN device already created"
    fi

    echo "Creating ach channels if they don't exist"

    ach mk ref -1
    ach mk state -1

    echo "starting SNS"

    sns=$(sns ls | grep DEAD)

    if [ -z "$sns" ]; then
	echo "SNS already started."
    else
	sns start
    fi

    echo "Running CAN402"

    dev=$1
    shift
    nodes=""

    for n in $@
    do
	nodes="$nodes -n $n"
    done

    exec can402 -f $dev -c ref -s state $nodes -vvvv
}



help(){
    	cat <<EOF
Usage: coms DEVICE NODES
Sets up and runs soCANmatic out of the device for the nodes.

Example:
coms can0 3            Runs soCANmatic to communicate to node 3 out of can0.
EOF
	exit 0
}




case "$1" in
    help|--help|-\?)
	help
	;;
    *)
	if [ -z "$1" ]; then
	    help
	else
	    run $@
	fi
	;;
esac
