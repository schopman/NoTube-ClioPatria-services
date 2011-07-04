#!/bin/bash

FILE='mappingbased_properties_en.nt'
EXCLUDED='mappingbased_properties_en_excluded.nt'
# zgrep "\^\^<" $FILE.gz > $EXCLUDED
zgrep "@\w\w \." $FILE.gz >> $EXCLUDED
zgrep -v "\^\^<" $FILE.gz | grep -v "@\w\w \." > $FILE
rm $FILE.gz
gzip $FILE
gzip $EXCLUDED

#removeNonRelTriples specific_mappingbased_properties_en.nt   # not used
