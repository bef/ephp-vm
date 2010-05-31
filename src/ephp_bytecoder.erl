-module(ephp_bytecoder).
-export([compile/1, scan_and_parse/1, make_bc/1, stage1/1, stage2/1, stage3/1, asmenc/1, bcenc/1]).

compile(Input) ->
	make_bc(scan_and_parse(Input)).

scan_and_parse(Input) ->
	Tokens = case ephp_lex:string(Input) of
		{ok, T, _} -> T;
		Err -> throw({scanner, Err})
	end,
	
	case ephp_parser:parse(Tokens) of
		{ok, R} -> R;
		Err2 -> throw({parser, Err2})
	end.

make_bc(PT) -> %% PT = Parse Tree as returned by ephp_parser:parse/*
	S1 = stage1(PT),
	S2 = stage2(S1),
	stage3(S2).


%%
%% stage 1: parse tree -> asm
%%

stage1(L) ->
	lists:flatten(
		lists:map(fun(El) -> asmenc(El) end, L)
	).

stage1_resolve_break(L, BreakLabel, ContinueLabel) when is_list(L) -> %% resolve break and continue
	AsmEncBreak = fun({break}) -> [{labeladdr, BreakLabel}, jump];
		({continue}) -> [{labeladdr, ContinueLabel}, jump];
		(X) -> X end,
	[AsmEncBreak(S1) || S1 <- L].

asmenc(X) when is_list(X) -> %% statement list
	stage1(X);

asmenc({break}=X) -> X;
asmenc({continue}=X) -> X;

%% expression inside statement list
asmenc({exp, E}) ->
	asmenc(E) ++ [pop];

%% data types
asmenc({num, _}=X) -> [X];
asmenc({string, _}=X) -> [X];

%% int. functions
asmenc({echo, Exp}) ->
	asmenc(Exp) ++ [echo];

%% variables
asmenc({assign, {{var, A}, B}}) ->
	asmenc(B) ++ [{string, A}, assign];
asmenc({var, A}) ->
	[{string, A}, getvar];

%% functions
asmenc({fundef, {Name, Args, Body}}) ->
	L = new_jumplabel(),
	[{string, Name}] %% function name
	++ [{string, X} || X <- Args] %% arg names
	++ [{num, length(Args)}, %% arg count
		fundef,
		{labeladdr, L}, jump %% jump past function body
		]
	++ asmenc(Body) %% function body
	++ [{num, 0}, %% default return value
		swap, %% swap return value and return pointer
		return,
		{label, L} %% end of function label
	];

asmenc({return, Exp}) ->
	asmenc(Exp) ++ [swap, return];

asmenc({fcall, {Name, Args}}) ->
	[{string, Name}] %% function name
	++ lists:map(fun(El) -> asmenc(El) end, Args) %% args
	++ [{num, length(Args)}, %% argc
		fcall
	];

%% conditions/loops
asmenc({'goto', Label}) ->
	[{labeladdr, Label}, jump];
asmenc({'label', _}=X) -> X;

asmenc({'if', {Exp, Stat, Else}}) ->
	L1 = new_jumplabel(),
	L2 = new_jumplabel(),
	ElseIf = case Else of
		[] -> [];
		[{'else', ElseStat}] -> asmenc(ElseStat);
		[{'elseif', ElseifExp, ElseifStat}|R] -> asmenc({'if', {ElseifExp, ElseifStat, R}})
	end,
	asmenc(Exp)
	++ [{labeladdr, L1}, condition] %% conditional jump to L1
	++ ElseIf %% false condition
	++ [{labeladdr, L2}, jump, %% jump past true condition
		{label, L1}, asmenc(Stat), {label, L2}];

asmenc({'for', {{Exp1, Exp2, Exp3}, Stat}}) ->
	L1 = new_jumplabel(),
	L2 = new_jumplabel(),
	asmenc({'exp', Exp1}) %% init statement
	++ [{label, L1}] %% loop start
	++ asmenc(Exp2) ++ [{labeladdr, L2}, notcondition]
	++ stage1_resolve_break(asmenc(Stat), L2, L1) %% in-loop
	++ asmenc({'exp', Exp3}) %% loop statement
	++ [{labeladdr, L1}, jump, %% jump to loop start
		{label, L2}]; %% post-loop jump label

asmenc({'while', {Exp, Stat}}) ->
	asmenc({'for', {{[], Exp, []}, Stat}});

asmenc({'dowhile', {Stat, Exp}}) ->
	L1 = new_jumplabel(),
	L2 = new_jumplabel(),
	[{label, L1}]
	++ stage1_resolve_break(asmenc(Stat), L2, L1)
	++ asmenc(Exp)
	++ [{labeladdr, L1}, condition,
		{label, L2}];

asmenc({'switch', {Exp, Cases}}) ->
	Lend = new_jumplabel(),
	
	
	J = lists:map(
		fun({'case', {Exp1, Stats}}) ->
			L1 = new_jumplabel(),
			Ops = asmenc({eq, {Exp, Exp1}}) ++ [{labeladdr, L1}, condition],
			{Ops, L1, Stats};
		({'default', Stats}) ->
			L1 = new_jumplabel(),
			Ops = [{labeladdr, L1}, jump],
			{Ops, L1, Stats}
	end, Cases),
	{Opsn, Labelsn, Statsn} = lists:unzip3(J),

	Statsn2 = [stage1_resolve_break(asmenc(SL), Lend, []) || SL <- Statsn], %% asmenc stats and map break to jump
	
	lists:append(Opsn) %% jumptable
	++ [{labeladdr, Lend}, jump] %% no-match jump
	++ lists:append([[{label, Ln}] ++ Sn || {Ln, Sn} <- lists:zip(Labelsn, Statsn2)]) %% case labels and statements
	++ [{label, Lend}]; %% end of switch label


%% operators

asmenc({binop, {Op, {A, B}}}) ->
	asmenc(A) ++ asmenc(B) ++ [Op];
asmenc({concat, _}=X) -> asmenc({binop, X});
asmenc({add, _}=X) -> asmenc({binop, X});
asmenc({sub, _}=X) -> asmenc({binop, X});
asmenc({mul, _}=X) -> asmenc({binop, X});
asmenc({'div', _}=X) -> asmenc({binop, X});
asmenc({mod, _}=X) -> asmenc({binop, X});
asmenc({land, _}=X) -> asmenc({binop, X});
asmenc({lor, _}=X) -> asmenc({binop, X});
asmenc({lxor, _}=X) -> asmenc({binop, X});
asmenc({'band', _}=X) -> asmenc({binop, X});
asmenc({'bor', _}=X) -> asmenc({binop, X});
asmenc({'bsl', _}=X) -> asmenc({binop, X});
asmenc({'bsr', _}=X) -> asmenc({binop, X});
asmenc({'bxor', _}=X) -> asmenc({binop, X});
asmenc({booland, _}=X) -> asmenc({binop, X});
asmenc({boolor, _}=X) -> asmenc({binop, X});
asmenc({eq, _}=X) -> asmenc({binop, X});
asmenc({ge, _}=X) -> asmenc({binop, X});
asmenc({le, _}=X) -> asmenc({binop, X});
% asmenc({ident, _}=X) -> asmenc({binop, X});
% asmenc({notident, _}=X) -> asmenc({binop, X});
asmenc({neq, _}=X) -> asmenc({binop, X});
asmenc({lt, _}=X) -> asmenc({binop, X});
asmenc({gt, _}=X) -> asmenc({binop, X});

%% unary operators
asmenc({increment, {'var', A}}) ->
	asmenc({string, A}) ++ [increment];
asmenc({decrement, {'var', A}}) ->
	asmenc({string, A}) ++ [decrement];

asmenc({'bnot', A}) -> asmenc(A) ++ ['bnot'];
asmenc({'not', A}) -> asmenc(A) ++ ['not'];

asmenc(PT) ->
	throw({asmenc, PT}).


new_jumplabel() ->
	make_ref().


%%
%% stage 2: asm -> bytecode w/o jumps
%%
stage2(L) ->
	lists:flatten(
		lists:map(fun(El) -> bcenc(El) end, L)
	).

bcenc(nop) -> [0];

%% internal functions
bcenc(echo) -> [1];

%% variables
bcenc(assign) -> "a";
bcenc(getvar) -> "g";
bcenc(pop) -> "P";
bcenc(swap) -> "S";

%% functions
bcenc(fcall) -> "f";
bcenc(return) -> "r";
bcenc(fundef) -> "F";

%% conditions
bcenc(condition) -> "c";
bcenc(notcondition) -> "C";
bcenc(jump) -> "j";

%% types
bcenc({num, V}) ->
	"i" ++ enc_int(V);
bcenc({string, V}) ->
	"s" ++ enc_int(length(V)) ++ V;

%% operators
bcenc(add) -> "+";
bcenc(sub) -> "-";
bcenc(mul) -> "*";
bcenc('div') -> "/";
bcenc(mod) -> "%";
bcenc(concat) -> ".";
bcenc(land) -> "A";
bcenc(lor) -> "O";
bcenc(lxor) -> "X";
bcenc('band') -> "&";
bcenc('bor') -> "|";
bcenc('bxor') -> "^";
bcenc('bsl') -> "[";
bcenc('bsr') -> "]";
bcenc('booland') -> ",";
bcenc('boolor') -> ";";
bcenc('eq') -> "=";
bcenc('lt') -> "<";
bcenc('gt') -> ">";
bcenc('le') -> "{";
bcenc('ge') -> "}";
% bcenc('ident') -> "_";
% bcenc('notident') -> "@";
bcenc('neq') -> [$"];
bcenc(increment) -> "I";
bcenc(decrement) -> "D";
bcenc('bnot') -> [$~];
bcenc('not') -> "!";


%% internal ops for jump label resolution
bcenc({labeladdr,_}=V) -> V;
bcenc({label,_}=V) -> V;

%% error
bcenc(V) ->
	throw({bcenc, V}).


enc_int(I) ->
	{Sign, V} = case I < 0 of
		true -> {16#80, -I};
		false -> {0, I}
	end,
	{Out, Length} = enc_int(V, 0, []),
	[(Length bor Sign)|Out].
enc_int(V, L, Out) when V =:= 0 ->
	{Out, L};
enc_int(V, L, Out) ->
	enc_int(V bsr 8, L + 1, [(V band 16#ff)|Out]).


%%
%% stage 3: resolve labels and jumps in two steps:
%%    (a) find label addresses & remove labels
%%    (b) replace labeaddr with jump
%%

stage3(L) ->
	{Labels, L1} = stage3a(L, [], 0, []),
	stage3b(L1, Labels, 0, []).

stage3a([], Labels, _Addr, Acc) -> {Labels, lists:reverse(Acc)};
stage3a([{labeladdr, _}=C|R], Labels, Addr, Acc) ->
	stage3a(R, Labels, Addr+6, [C|Acc]); %% assume 6 bytes for relative jump address
stage3a([{label, Marker}|R], Labels, Addr, Acc) ->
	stage3a(R, [{Marker, Addr-1}|Labels], Addr, Acc);
stage3a([C|R], Labels, Addr, Acc) ->
	stage3a(R, Labels, Addr+1, [C|Acc]).

stage3b([], _Labels, _Addr, Acc) -> lists:reverse(Acc);
stage3b([{labeladdr, Marker}|R], Labels, Addr, Acc) ->
	{value, {_, LabelAddr}} = lists:keysearch(Marker, 1, Labels),
	RelativeJumpAddr = LabelAddr - (Addr + 6),
	% ---XJL--
	% 0123456789
	JumpAddrBC = bcenc({num, RelativeJumpAddr}),
	Nops = lists:duplicate(6 - length(JumpAddrBC), 0), %% nop padding
	stage3b(Nops ++ JumpAddrBC ++ R, Labels, Addr, Acc);
stage3b([C|R], Labels, Addr, Acc) ->
	stage3b(R, Labels, Addr+1, [C|Acc]).

