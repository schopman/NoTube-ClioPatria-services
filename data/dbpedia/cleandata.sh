#!/bin/bash
removeNonRelTriples ( ) {
	: ${1?}
	ORIGINAL='ze_original_'$1
	echo 'removing non-relational triples from '$1
	mv $1 $ORIGINAL
	grep -v "\^\^" $ORIGINAL > $1
	mv $1 $1.tmp
	grep -v "@" $1.tmp > $1
	rm $1.tmp
}


cd infobox

FILE='instance_types_en.nt'
echo 'removing [X rdf:type owl:Thing] triples from '$FILE
grep -v '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Thing>' $FILE > $FILE.tmp
mv $FILE.tmp $FILE

removeNonRelTriples mappingbased_properties_en.nt
#removeNonRelTriples specific_mappingbased_properties_en.nt   # not used
