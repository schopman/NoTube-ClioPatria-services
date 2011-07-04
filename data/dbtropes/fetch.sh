#!/bin/bash
FILE=dbtropes.zip
wget http://dbtropes.org/static/$FILE
unzip $FILE
gzip -c *.nt > dbtropes.nt.gz
rm $FILE *.nt
