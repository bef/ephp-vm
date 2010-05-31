#!/usr/bin/env escript

main(Args) ->
	%% include path
	code:add_patha(filename:join([filename:dirname(escript:script_name()), "..", "ebin"])),
	
	%% log errors to console
	error_logger:tty(true),
	
	ephpc(Args).

ephpc([Infile, Outfile]) ->
	compile([], Infile, Outfile);

ephpc([_,_|_]=Args) ->
	{Opts, Infile, Outfile} = parse_opts(Args, []),
	compile(Opts, Infile, Outfile);

ephpc(_) ->
	usage().

usage() ->
	io:format("** EPHP bytecode compiler v1 ** (c) BeF <bef@pentaphase.de> **~n"),
	io:format("usage: ephp [opts] <infile> <outfile>~n  opts: -base64 | -js    encode bytecode as base64 or json~n"),
	halt(1).

parse_opts([I, O], Opts) ->
	{Opts, I, O};
parse_opts([Opt|R], Opts) ->
	parse_opts(R, [Opt|Opts]).
	

compile(Opts, Infile, Outfile) ->
	%% read file
	Input = case file:read_file(Infile) of
		{ok, Binary} -> binary_to_list(Binary);
		{error, ReadErr} -> io:format("error: ~p~n", [ReadErr]), halt(2)
	end,

	%% compile
	S3 = try ephp_bytecoder:compile(Input) of
		BC -> BC
	catch
		Err -> io:format("error: ~p~n", [Err]),
		halt(2)
	end,

	%% encode base64?
	Output = encode(Opts, S3),
	
	%% output
	case Outfile of
		"--" ->
			io:format("~s", [Output]);
		_ ->
			case file:write_file(Outfile, Output) of
				ok -> ok;
				{error, WriteErr} -> io:format("error: ~p~n", [WriteErr]), halt(2)
			end
	end,
	ok.

encode(["-base64"|Opts], BC) ->
	encode(Opts, base64:encode_to_string(BC));
encode(["-js"|Opts], BC) ->
	encode(Opts, jsencode(BC));
encode([_|Opts], BC) ->
	encode(Opts, BC);
encode([], BC) -> BC.

jsencode(Str) ->
	lists:flatten(lists:map(
		fun(X) when X >= 32 andalso X < 127 -> X;
			(X) -> io_lib:fwrite("\\x~2.16.0B", [X])
		end, Str)).
