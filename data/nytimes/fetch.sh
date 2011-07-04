#!/bin/bash
FILEPATH='http://data.nytimes.com'

wget $FILEPATH/people.rdf
wget $FILEPATH/organizations.rdf
wget $FILEPATH/locations.rdf
wget $FILEPATH/descriptors.rdf
gzip *.rdf
