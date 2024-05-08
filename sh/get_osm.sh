#!/bin/bash

mkdir -p data/osm/
mkdir -p r5

# download raw data from geofabrik
wget -N http://download.geofabrik.de/north-america/us/arizona-240101.osm.pbf -O data/osm/arizona.osm.pbf
wget -N http://download.geofabrik.de/north-america/us/new-mexico-240101.osm.pbf -O data/osm/new-mexico.osm.pbf
wget -N https://download.geofabrik.de/north-america/us/utah-240101.osm.pbf -O data/osm/utah.osm.pbf
wget -N http://download.geofabrik.de/north-america/us/colorado-240101.osm.pbf -O data/osm/colorado.osm.pbf

# extract fourcorners region from other states
osmium extract --overwrite -p data/fourcbounding.geojson data/osm/colorado.osm.pbf -o data/osm/fccolorado.osm.pbf
osmium extract --overwrite -b -110.7,36.3,-107.8,38.6  data/osm/new-mexico.osm.pbf -o data/osm/fcnew-mexico.osm.pbf
osmium extract --overwrite -p data/fourcbounding.geojson data/osm/arizona.osm.pbf -o data/osm/fcarizona.osm.pbf

# merge into a single network
osmium merge --overwrite data/osm/utah.osm.pbf data/osm/fccolorado.osm.pbf data/osm/fcnew-mexico.osm.pbf data/osm/fcarizona.osm.pbf -o r5/merged.osm.pbf
