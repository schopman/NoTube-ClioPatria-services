#!/bin/bash

OUT='film_genres.nt'

awk 'BEGIN { FS = "\t" }
	{ if ($2 ~ /\//) {
		n=split($20, genre, ",");
		for (i=1;i<=n;i++) {
			sub(/\"/, "", genre[i]);
			printf "<http://rdf.freebase.com/ns%s> \t <http://rdf.freebase.com/ns/film.film.genre> \"%s\" .\n", $2, genre[i] 
		}
	  }
	}' film/film.tsv > $OUT

awk 'BEGIN { FS = "\t" }
	{ if ($2 ~ /\//) {
		sub(/\"/, "", $1); sub(/\"/, "", $1);
		printf "<http://rdf.freebase.com/ns%s> \t <http://www.w3.org/2000/01/rdf-schema#label> \"%s\" .\n", $2, $1
	  }
	}' film/film_genre.tsv >> $OUT

gzip $OUT
