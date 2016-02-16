#!/usr/bin/env bash
cat _num.scm | sed "s/##/@@/g" | sed "s/#!optional//g" | sed "s/'#f64(/(flvector /g" > _num.no##.scm
racket run.rkt
cat <(echo "#lang racket") <(echo "(require \"_num#.rkt\")") <(cat _num.no##.scm | sed "s/^(/#;(/g") > _num.rkt.orig
