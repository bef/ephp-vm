#!/usr/bin/env escript

%% tiny test framework
suite(Name, Run, Testcases) -> {suite, Name, Run, Testcases}.

run_suite({suite, Name, F, Testcases}) ->
	io:format("### ~s~n", [Name]),
	lists:foreach(fun(Testcase) -> run_testcase(F, Testcase) end, Testcases),
	ok.

run_testcase(Run, {TestcaseName, Testcase}) ->
	try erlang:apply(Run, [Testcase]) of
		success -> io:format("[ok] ~s~n", [TestcaseName]);
		{Status, ErrorMsg} ->
			io:format("[~s] ~s~n --> ~s~n", [string:to_upper(atom_to_list(Status)), TestcaseName, ErrorMsg])
	catch
		Y -> io:format("[EXCEPTION] ~s~n --> ~p~n", [TestcaseName, Y])
	end.

%% run-test function for each suite
run_script_php(Input) ->
	S3 = ephp_bytecoder:compile(Input),
	S3enc = base64:encode_to_string(S3),
	os:cmd("php ../vm/vm.php " ++ S3enc).

jsencode(Str) ->
	lists:flatten(lists:map(
		fun(X) when X >= 32 andalso X < 127 -> X;
			(X) -> io_lib:fwrite("\\x~2.16.0B", [X])
		end, Str)).
run_script_v8(Input) ->
	S3 = ephp_bytecoder:compile(Input),
	S3enc = jsencode(S3),
	% Code = "vm = new __vm(); vm.echo = function(s) { print(s);}; window = new Object(); vm.run('" ++ S3enc ++ "');",
	Code = "vm.run('" ++ S3enc ++ "');",
	Output = os:cmd("v8-shell -e \"load('./testenv.js');" ++ Code ++ "\""),
	lists:concat(string:tokens(Output, "\n")).
	
run_langtest(Run, {Input, ExpectedOutput}) ->
	try Run(Input) of
		ExpectedOutput -> success;
		X -> {failed, lists:flatten(io_lib:format("output [~s]; expected output [~s]; input [~s]", [X, ExpectedOutput, Input]))}
	catch
		Err -> {error, lists:flatten(io_lib:format("~p", [Err]))}
	end.


run_lextest({Input, ExpectedOutput}) ->
	case ephp_lex:string(Input) of
		{ok, ExpectedOutput, _} -> success;
		{ok, Tokens, _} -> {failed, lists:flatten(io_lib:format("got ~p; expected ~p", [Tokens, ExpectedOutput]))};
		Err -> {error, lists:flatten(io_lib:format("~p", [Err]))}
	end.

run_parsertest({Input, ExpectedOutput}) ->
	{ok, Tokens, _} = ephp_lex:string(Input),
	
	case ephp_parser:parse(Tokens) of
		{ok, ExpectedOutput} -> success;
		{ok, R} -> {failed, lists:flatten(io_lib:format("got ~p; expected ~p", [R, ExpectedOutput]))};
		Err -> {error, lists:flatten(io_lib:format("~p", [Err]))}
	end.


main(Args) ->
	%% include path
	code:add_patha(filename:join([filename:dirname(escript:script_name()), "..", "ebin"])),
	
	%% log errors to console
	error_logger:tty(true),

	%% LEX tests
	Lex_kw = ["break", "case", "continue", "default", "do", "echo", "else", "elseif", "for", "function", "global", "goto", "if", "return", "switch", "while"],
	Lextests = [{X, [{list_to_atom("t_"++X), 1}]} || X <- Lex_kw] ++ [
		{"&&", [{t_boolean_and,1}]},
		{"||", [{t_boolean_or,1}]},
		{"--", [{t_dec,1}]},
		{"++", [{t_inc,1}]},
		{"==", [{t_is_equal,1}]},
		{">=", [{t_is_greater_or_equal,1}]},
		{"!=", [{t_is_not_equal,1}]},
		{"<>", [{t_is_not_equal,1}]},
		{"and", [{t_logical_and,1}]},
		{"or", [{t_logical_or,1}]},
		{"xor", [{t_logical_xor,1}]},
		{"<<", [{t_sl,1}]},
		{">>", [{t_sr,1}]},
		{"<", [{t_smaller,1}]},
		{">", [{t_greater,1}]},
		{"*", [{t_mul,1}]},
		{"/", [{t_div,1}]},
		{"-", [{t_minus,1}]},
		{"+", [{t_plus,1}]},
		{"%", [{t_mod,1}]},
		{".", [{t_concat,1}]},
		{"\~", [{t_bnot,1}]},
		{"&", [{t_band,1}]},
		{"|", [{t_bor,1}]},
		{"^", [{t_bxor,1}]},
		{"!", [{t_not,1}]},
		{"&=", [{t_and_equal,1}]},
		{".=", [{t_concat_equal,1}]},
		{"/=", [{t_div_equal,1}]},
		{"-=", [{t_minus_equal,1}]},
		{"*=", [{t_mul_equal,1}]},
		{"|=", [{t_or_equal,1}]},
		{"+=", [{t_plus_equal,1}]},
		{"<<=", [{t_sl_equal,1}]},
		{">>=", [{t_sr_equal,1}]},
		{"^=", [{t_xor_equal,1}]},
		{"'test'", [{t_constant_encapsed_string,1,"test"}]},
		{"\"test\"", [{t_constant_encapsed_string,1,"test"}]},
		{"0x23", [{t_number,1,35}]},
		{"023", [{t_number,1,19}]},
		{"23", [{t_number,1,23}]},
		{"$foo", [{t_variable,1,"foo"}]},
		{";", [{';',1}]},
		{"bla", [{t_symbol,1,"bla"}]},
		{"(", [{'(',1}]},
		{")", [{')',1}]},
		{",", [{',',1}]},
		{":", [{':',1}]},
		{"{", [{'{',1}]},
		{"}", [{'}',1}]},
		{"// foo", []},
		{"# foo", []},
		{"/* test */", []}
	],
	
	Lextestcases = [{X, {X, Y}} || {X, Y} <- Lextests],
	Lextest_suite = suite("LEX", fun(X) -> run_lextest(X) end, Lextestcases),

	%% parser test
	Parsertests = [
		{"23;", [{exp,{num,23}}]},
		{"{ }", [[]]},
		{"{ 23; }", [[{exp,{num,23}}]]},
		{"function foo($a) {}", [{fundef,{"foo",["a"],[]}}]},
		{"if (23) {} elseif (42) {} else {}", [{'if',{{num,23},[],[{elseif,{num,42},[]},{else,[]}]}}]},
		{"for (1; 2; 3) {}", [{for,{{{num,1},{num,2},{num,3}},[]}}]},
		{"while (1) {}", [{while,{{num,1},[]}}]},
		{"do {} while (23);", [{dowhile,{[],{num,23}}}]},
		{"echo 'foo';", [{echo,{string,"foo"}}]},
		{"return 23;", [{return,{num,23}}]},
		{"break;", [{break}]},
		{"continue;", [{continue}]},
		{"boo:", [{label,"boo"}]},
		{"goto boo;", [{goto,"boo"}]},
		{"switch ($a) { default: 23; }", [{switch,{{var,"a"},[{default,[{exp,{num,23}}]}]}}]}
	],
	Parsertestcases = [{X, {X, Y}} || {X, Y} <- Parsertests],
	Parsertest_suite = suite("Parser", fun(X) -> run_parsertest(X) end, Parsertestcases),

	%% language construct tests
	Langtestcases = [
		{"datatypes/integer", {"echo is_int(23);", "1"}},
		{"datatypes/string", {"echo is_string('test');", "1"}},
		{"expressions/+", {"echo 23+42;", "65"}},
		{"expressions/variable assignment", {"$a = 23; echo $a;", "23"}},
		{"conditions/simple if/true", {"echo 'a'; if (1) { echo 11; } echo 'b';", "a11b"}},
		{"conditions/simple if/false", {"echo 'a'; if (0) { echo 11; } echo 'b';", "ab"}},
		{"conditions/if-else/true", {"if (1) { echo 11; } else { echo 14;}", "11"}},
		{"conditions/if-else/false", {"if (0) { echo 11; } else { echo 14;}", "14"}},
		{"conditions/if-elseif", {"if (0) { echo 11; } elseif (1) { echo 14;} echo 'b';", "14b"}},
		{"loops/for", {"for ($i = 4; $i; --$i) echo 'x';", "xxxx"}},
		{"loops/while", {"$i = 4; while ($i) {echo 'x'; $i = $i - 1;}", "xxxx"}},
		{"loops/do-while", {"$a = 4; do { echo 'x'; --$a; } while ($a);", "xxxx"}},
		{"functions/definition", {"function foo() {}", ""}},
		{"functions/def. w/ args", {"function foo($a, $b) { return $a . $b; }", ""}},
		{"functions/call", {"function foo($a, $b) { return $a . $b; } echo 'a'; echo foo('b','c');", "abc"}},
		{"other language constructs/goto", {"echo 'a'; goto b; echo 'b'; b: echo 'c';", "ac"}}
	],
	Langtest_php_suite = suite("language test (PHP)", fun(X) -> run_langtest(fun(Y) -> run_script_php(Y) end, X) end, Langtestcases),
	Langtest_js_suite = suite("language test (JS)", fun(X) -> run_langtest(fun(Y) -> run_script_v8(Y) end, X) end, Langtestcases),

	%% all tests
	TestSuites = [
		{"lex", Lextest_suite},
		{"parser", Parsertest_suite},
		{"lang_php", Langtest_php_suite},
		{"lang_js", Langtest_js_suite}
	],
	
	%% cli parsing
	RunSuites = case Args of
		[] -> [X || {X, _} <- TestSuites];
		_ -> Args
	end,
	
	%% run tests
	lists:foreach(fun(RS) ->
		case lists:keysearch(RS, 1, TestSuites) of
			false -> io:format("unknown suite ~s~n", [RS]); 
			{_, {_, Suite}} -> run_suite(Suite)
		end
	end, RunSuites),
	
	ok.