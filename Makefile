REBAR     = ./rebar
COMBO_PLT = ./conmero_dialyzer_plt
APPS      = kernel stdlib conmero

.PHONY: rel

all: compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf deps

doc:
	@$(REBAR) skip_deps=true doc

console: compile
	@erl -sname conmero -pa ebin \
	 ./deps/*/ebin/ \
	 -boot start_sasl \
	 -s conmero

win_console: compile
	@werl -sname conmero -pa ebin \
	 ./deps/*/ebin/ \
	 -boot start_sasl \
	 -s conmero


check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		./ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		./ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ./ebin | \
		fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	@echo 
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo 
	sleep 5
	rm $(COMBO_PLT)
