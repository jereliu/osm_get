#### 1. Preprocessing & Element Read-in ####

# store current working directory
cwd="/Users/Jeremiah/GitHub/osm_get/Script"
cd $cwd

# read file names with extension ".osm" under Data_osm
# then pipe into 'sed' to remove "./Data_osm/" and ".osm" from file names
# then assign this array into variable with name "file_list"

file_list=( $(find ./Data_osm -type f -regex ".*.osm" | sed -e 's/^\.\/Data_osm\///g; s/\.osm//g') )


#### 2. Formal Processing using OSM2World.sh ####

# switch to target directory
cd ../Library/OSM2World/

#conversion
for file_name in "${file_list[@]}"; 
	do 
	./osm2world.sh -i "${cwd}/Data_osm/${file_name}.osm" -o "${cwd}/Data_3d/${file_name}.obj";
	echo "================== ${file_name} Finished! =====================";
done


#switch back working directory
cd $cwd
