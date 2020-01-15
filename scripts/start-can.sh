#!/bin/bash
./coms.sh can0 3 4 5 6 7 8 9 &
comsPID=$!
/opt/dyalab/robotiq-controllers/gripper_open_close
kill $comsPID
