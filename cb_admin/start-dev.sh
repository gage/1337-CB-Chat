#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin \
    -pa ../CB/ebin \
    -pa ../CB/deps/*/ebin \
    -boss developing_app cb_admin \
    -boot start_sasl -config boss -s reloader -s boss \
    -sname wildbill
