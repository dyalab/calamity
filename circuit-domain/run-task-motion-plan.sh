export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/

cd `dirname "$0"`
tmsmt ../schunk/lwa4d_robot.urdf \
      q0.robray \
      allow-collision.robray \
      circuits.pddl \
      basic-test.pddl \
      circuit-movement.py \
      -g scene.robray \
      -q q0.tmp \
      --gui \
      -v \
      --write-facts=~/Git/calamity/circuit-domain/test-facts
