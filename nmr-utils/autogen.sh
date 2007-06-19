#!/bin/sh

# Run this file so that a fresh checkout from
# source control can be configured & built.

aclocal;
automake --add-missing;
autoconf;

