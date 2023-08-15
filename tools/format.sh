#!/bin/sh
PATH="$HOME/bin:$PATH"

OPTIONS="--no-subprojects --no-compact --decimal_grouping=3 --preserve-blank-lines --insert-blank-lines --indent-named-statements -j4 -M79"

gnatpp $OPTIONS -Pdestiny_inventory_tool &
gnatpp $OPTIONS -Pgpr/bungie_api &
#gnatpp $OPTIONS -Pgpr/bindings &
wait
