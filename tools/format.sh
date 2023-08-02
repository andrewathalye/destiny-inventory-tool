#!/bin/sh
PATH="$PATH:$HOME/bin"
#gnatpp --no-subprojects --no-compact --decimal_grouping=3 --preserve-blank-lines --insert-blank-lines --indent-named-statements -Pdestiny_inventory_tool &
gnatpp --no-subprojects --no-compact --decimal_grouping=3 --preserve-blank-lines --insert-blank-lines --indent-named-statements -Pgpr/bungie_api &
#gnatpp --no-subprojects --no-compact --decimal_grouping=3 --preserve-blank-lines --insert-blank-lines --indent-named-statements -Pgpr/bindings &
wait
