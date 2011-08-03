#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin \
     -pa /Users/Gage/tmp/1337-CB-Chat/CB/ebin \
     -pa /Users/Gage/tmp/1337-CB-Chat/CB/deps/*/ebin \
     -boss developing_app chat \
     -boot start_sasl -config boss -s reloader -s boss \
     -sname wildbill
