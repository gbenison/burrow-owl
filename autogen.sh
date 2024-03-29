#!/bin/sh

# Run this file so that a fresh checkout from
# source control can be configured & built.

for a in *;
do
   test -d $a || continue;
   test -f $a/autogen.sh || continue;
   (cd $a; ./autogen.sh;)
done

libtoolize;
aclocal;
automake --add-missing;
autoconf;

