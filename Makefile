

all: misultin backend websvc

check: dialyzer xref

xref:
	erl -pa lib/misultin-0.8/ebin -pa lib/wc_backend-0.1/ebin -pa lib/wc_websvc-0.1/ebin -eval 'A = xref:m(wc_backend), B = xref:m(wc_websvc),io:format("~p~n",[A++B]),init:stop().'

dialyzer:
	dialyzer --src -r ./lib

start: all run

release: all 
	erl -sname clocknode -pa ./lib/misultin-0.8/ebin -pa ./lib/wc_backend-0.1/ebin -pa ./lib/wc_websvc-0.1/ebin  -eval 'systools:make_script("./release/weasleyclock",[{outdir,"./release/"}]),systools:make_tar("./release/weasleyclock",[{erts,code:root_dir()},{outdir,"./release/"}]),init:stop().'

start_release:
	ERL_LIBS=`pwd`/lib/ erl -sname clocknode -boot release/weasleyclock -config release/sys -eval 'wc_backend_sup:start_child().'

misultin:
	@cd lib/misultin-0.8; \
	make all

backend:
	@cd lib/wc_backend-0.1; \
	rebar compile

websvc: 
	@cd lib/wc_websvc-0.1; \
	rebar compile

run: 
	erl -pa lib/misultin-0.8/ebin \
	-pa lib/wc_backend-0.1/ebin \
        -pa lib/wc_websvc-0.1/ebin \
	 -eval 'application:start(misultin), application:start(wc_websvc), application:start(wc_backend), wc_backend_sup:start_child().'
