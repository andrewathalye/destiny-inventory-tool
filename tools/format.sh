#!/bin/sh
PATH="$PATH:$HOME/bin"
EXCLUDE="api-inventories-character-add.adb
api-inventories-character-equip.adb"

OPTIONS="--no-subprojects --no-compact --decimal_grouping=3 --preserve-blank-lines --insert-blank-lines --indent-named-statements"

echo "$EXCLUDE" > ignore.tmp
gnatpp $OPTIONS -Pdestiny_inventory_tool &
gnatpp $OPTIONS -Pgpr/bungie_api --ignore=ignore.tmp &
#gnatpp $OPTIONS -Pgpr/bindings &
wait
rm ignore.tmp
