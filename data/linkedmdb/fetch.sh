#!/bin/bash

# FILE='linkedmdb-latest-dump.tar.gz'
# wget http://queens.db.toronto.edu/~oktie/linkedmdb/$FILE
# tar --to-stdout -xf $FILE | gzip > linkedmdb.nt.gz
# rm $FILE
wget http://www.few.vu.nl/~schopman/notube/linkedmdb/linkedmdb.nt.gz

echo 'generating lite version -- for the no-nonsense index'
zgrep -v "http://data.linkedmdb.org/resource/interlink/" linkedmdb.nt.gz | zgrep -v "http://data.linkedmdb.org/resource/performance/" | gzip > linkedmdb_lite.nt.gz

echo 'generating separate set of triples containing names and titles -- for enrichments'
zgrep "_name>" linkedmdb.nt.gz | grep -v \"\\s*\" > tmp1
zgrep "http://purl.org/dc/terms/title" linkedmdb.nt.gz > tmp2
cat tmp1 tmp2 | gzip > lmdbNamesTitles.nt.gz
rm tmp1 tmp2
