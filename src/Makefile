EBIN = ../ebin
ERLC = erlc -I ../include -o $(EBIN)
VERBOSE = false

all: $(EBIN)/ephp_lex.beam $(EBIN)/ephp_parser.beam $(EBIN)/ephp_bytecoder.beam


$(EBIN)/ephp_lex.beam: ephp_lex.xrl
	erl -noshell -eval 'leex:file("ephp_lex"), halt().'
	$(ERLC) ephp_lex.erl

$(EBIN)/ephp_parser.beam: ephp_parser.yrl
	erl -noshell -eval 'yecc:file("ephp_parser", {verbose, $(VERBOSE)}), halt().'
	$(ERLC) ephp_parser.erl

$(EBIN)/ephp_bytecoder.beam: ephp_bytecoder.erl
	$(ERLC) ephp_bytecoder.erl

clean:
	rm -f ephp_lex.erl ephp_parser.erl $(EBIN)/*.beam
