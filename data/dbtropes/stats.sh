#!/bin/bash

FILE=dbtropes.nt.gz
# echo "== predicates =="
# gunzip -c $FILE | awk '{print $2}' | sort | uniq -c

# echo "== subjects =="
# gunzip -c $FILE | awk '{print $1}' | sort | uniq -c

# echo "== objects =="
gunzip -c $FILE | awk '{print $3}' | sort | uniq -c > objects.txt

