export ROS_PACKAGE_PATH=/home/mschack/Git/schunk_modular_robotics/

cd `dirname "$0"`
echo "cding into schunk to make compiled scenegraph"

# Compile .c files
aarxc lwa4d.urdf -n lwa4d -s lwa4d.robray
aarxc lwa4d.urdf -n lwa4d -o lwa4d.c
aarxc schunk_on_table.robray -n schunk_on_table -o schunk_on_table.c
aarxc schunk_with_table.robray -n schunk_with_table -o schunk_with_table.c

# Remove duplicate files
rm lwa4d.robray


make

# Remove .c files
rm *.c
rm *.c.h
