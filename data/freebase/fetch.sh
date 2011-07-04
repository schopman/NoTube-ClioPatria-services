#!/bin/bash
wget http://download.freebase.com/datadumps/latest/browse/film.tar.bz2
bzip2 -d film.tar.bz2
tar -xf film.tar
rm film.tar*

