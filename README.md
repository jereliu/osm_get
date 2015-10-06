# osm_get

R Script to:

1. obtain osm map data of specified radius/ bbox around a given coordinate [immitate JOSM]
2. obtain 3d model (OBJ) from obtained osm data [through commandline OSM2World]
3. Measure volumn of generated 3d model  

## TO-DO

* Extract Data through OSM-API 
	- [x] How to extract osm data from API: [openstreetmap API](http://wiki.openstreetmap.org/wiki/Downloading_data#Choose_your_region)
	- [ ] Write this function in R
* Obtain 3d model using OSM2World commandline
	- [ ] Figure out how to use OSM2World commandline tool
	- [ ] Carry out Extraction
* Figure out how to measure volumn of a OBJ file
	- [ ] No idea yet....
