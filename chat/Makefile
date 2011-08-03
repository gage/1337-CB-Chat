ERL=erl
APP=chat

all:
	$(ERL) -pa /Users/Gage/tmp/1337-CB-Chat/CB/ebin -eval 'boss_load:load_all_modules_and_emit_app_file($(APP), "ebin")' -s init stop -noshell

clean:
	rm -fv ebin/*.beam
	rm -fv ebin/$(APP).app
	
update_po:
	$(ERL) -pa ebin -pa ../ChicagoBoss/ebin -eval 'boss_load:load_models("ebin")' -eval 'boss_lang:update_po()' -s init stop -noshell

.PHONY: test
test:
	$(ERL) -pa /Users/Gage/tmp/1337-CB-Chat/CB/ebin -run boss_web_test -noshell
