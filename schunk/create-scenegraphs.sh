export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/

cd `dirname "$0"`
echo "cding into schunk to make compiled scenegraph"

# Copy some needed files
cp ../circuit-domain/q0.robray ./
cp ../circuit-domain/scene.robray ./
cp ../circuit-domain/circuit.robray ./
cp ../circuit-domain/robotiq.robray ./
cp ../circuit-domain/goal.robray ./

# Compile .c files
aarxc lwa4d.urdf -n lwa4d -s lwa4d.robray
aarxc lwa4d.urdf -n lwa4d -o lwa4d.c
aarxc schunk_on_table.robray -n schunk_on_table -o schunk_on_table.c
aarxc schunk_with_table.robray -n schunk_with_table -o schunk_with_table.c
aarxc q0.robray lwa4d.urdf -n circuit -o q0.c
aarxc goal.robray lwa4d.urdf -n circuit -o goal.c

# Remove duplicate files
rm q0.robray
rm scene.robray
rm circuit.robray
rm robotiq.robray
rm goal.robray

make

# Remove .c files
rm *.c
rm *.c.h
