#!/usr/bin/env bash

cat _num.scm | \
  sed "s/##/@@/g" | \
  sed "s/#!optional//g" | \
  sed "s/'#f64(/(flvector /g" > _num.no#.scm

# Check readability
racket -e "(file->list \"_num.no#.scm\")" > /dev/null

cat \
  <(echo ";; original: gambit/lib/_num.scm") \
  <(echo "#lang racket") \
  <(cat _num.no#.scm) > _num.rkt.orig

cat _num.rkt.orig | sed "s/^(/#;(/g" > _num.rkt.comment
