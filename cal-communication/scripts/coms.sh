#!/bin/bash
#Sets up necessary variables to run socanmatic to communicate with the
#Schunk arm.
#Author: Matthew Schack


stateChan="realState"
refChan="realRef"

run(){

    echo "Initalizing CAN device..."
    up=$(ip link | grep $1 | grep UP)
    if [ -z "$up" ]; then
    	sudo ip link set $1 up type can bitrate 500000
    	echo "CAN device created"
    else
    	echo "CAN device already created"
    fi

    echo "Creating ach channels if they don't exist"

    ach mk $refChan -1
    ach mk $stateChan -1

    echo "starting SNS"

    sns=$(sns ls | grep -v DEAD)

    if [ -z "$sns" ]; then
	sns start
    else
	echo "SNS already started."
    fi

    echo "Running CAN402"

    dev=$1
    shift
    nodes=""

    for n in $@
    do
	nodes="$nodes -n $n"
    done
    # while [ ! -z "$nodes" ]
    # do
    # 	sleep 1
    # done

    exec can402 -f $dev -c $refChan -s $stateChan $nodes -R 1 -C 0



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
