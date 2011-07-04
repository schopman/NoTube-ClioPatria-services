#!/bin/bash
DIRNAME="project$RANDOM"
mkdir $DIRNAME
cd $DIRNAME

mkdir {apis,components}

TEMPL_DIR='../templates_new_project'
cp $TEMPL_DIR/gitignore.txt .gitignore
cp $TEMPL_DIR/p_run.pl .
cp $TEMPL_DIR/configure.sh .
cp $TEMPL_DIR/purge_cache.sh .
chmod u+x p_run.pl configure.sh

./configure.sh

echo "Created new project in $DIRNAME"
