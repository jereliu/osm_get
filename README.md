# osm_get

beta v0.1

R/Bash Script for project **BostonPol/Elem** to:

1. obtain osm map data of specified radius/ bbox around a given coordinate [through OSM Extended API]
2. obtain 3d model (OBJ) from obtained osm data [bash script evoking commandline OSM2World].
3. Measure volumn of generated 3d model through convex hull estimation. 

## TO-DO

* Extract Data through OSM-API 
	- [x] How to extract osm data from API: [openstreetmap API](http://wiki.openstreetmap.org/wiki/Downloading_data#Choose_your_region)
	- [x] Write this function in R
* Obtain 3d model using OSM2World commandline
	- [x] Figure out how to use OSM2World commandline tool
	- [x] Carry out Extraction using .sh

	
* Extract building model out of a OBJ file
 	- [x] Write R script to subset buildings
 	- [x] Fix Vertex index
 	- [x] Fix Stragglers with irregular data 
 		+ (Note: problem was some building facets requires vertices thats not defined in "g BUILDINGS" code blocks).
	
* Figure out how to measure volumn of a OBJ file
	- [x] R-script to parse vertices contained by each building
	- [x] Calculate convex hull with volume measure using 'geometry' package.
