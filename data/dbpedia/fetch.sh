#!/bin/bash
fixDBpediaFile ( ) {
	: ${1?}
	ZIPPED=$1.bz2
	wget http://downloads.dbpedia.org/3.6/$2/$ZIPPED
	bzip2 -cd $ZIPPED | gzip > $1.gz
	rm $ZIPPED
	echo "* fixed: $1"
}

fixDBpediaDataset ( ) {
	: ${1?}
	fixDBpediaFile $1 en
}

fixDBpediaAlignment ( ) {
	: ${1?}
	fixDBpediaFile $1 links
}


FILE=dbpedia_3.6.owl
ZIPPED=$FILE.bz2
wget http://downloads.dbpedia.org/3.6/$ZIPPED
bzip2 -cd $ZIPPED > $FILE
rm $ZIPPED

fixDBpediaAlignment freebase_links.nt

# mkdir categories
# cd categories
# fixDBpediaDataset skos_categories_en.nt
# fixDBpediaDataset article_categories_en.nt
# fixDBpediaDataset category_labels_en.nt
# cd ..

mkdir infobox
cd infobox
fixDBpediaDataset mappingbased_properties_en.nt
zgrep influenced mappingbased_properties_en.nt.gz | gzip > mappingbased_props_influences.nt.gz
cd ..
