#!/bin/sh
gnatpp --no-subprojects -Pdestiny_inventory_tool --no-compact -c4 -c3 --decimal_grouping=3 --insert-blank-lines --preserve-blank-lines -M79 "$@"
