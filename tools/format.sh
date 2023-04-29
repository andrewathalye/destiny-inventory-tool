#!/bin/sh
gnatpp --no-subprojects --no-compact --decimal_grouping=3 --preserve-blank-lines --insert-blank-lines --indent-named-statements "$@"
