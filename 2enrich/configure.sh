#!/bin/bash
CPDIR="../ClioPatria"
$CPDIR/configure
echo ":- [p_run]." >> run.pl
cp $CPDIR/config-available/{cache.pl,logging.pl} config-enabled
