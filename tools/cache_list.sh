#!/bin/sh
# Makes a list of the files in the cache directory.
# Not super efficient, but works for now. Rewrite in Ada / C if necessary later.

for i in cache/*
do
	echo -n "$i: "
	cat "$i" | tr '\0' '\n' | head -n1
done;
