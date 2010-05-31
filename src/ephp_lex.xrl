Definitions.

WS  = [\000-\s]
D = [0-9]

Rules.

%% keywords
break|case|continue|default|do|echo|else|elseif|for|function|global|goto|if|return|switch|while :
	{token, {list_to_atom("t_" ++ TokenChars), TokenLine}}.


%% operators
&& : {token, {t_boolean_and, TokenLine}}.
\|\| : {token, {t_boolean_or, TokenLine}}.
-- : {token, {t_dec, TokenLine}}.
\+\+ : {token, {t_inc, TokenLine}}.
== : {token, {t_is_equal, TokenLine}}.
>= : {token, {t_is_greater_or_equal, TokenLine}}.
!=|<> : {token, {t_is_not_equal, TokenLine}}.
<= : {token, {t_is_smallor_or_equal, TokenLine}}.
and : {token, {t_logical_and, TokenLine}}.
or : {token, {t_logical_or, TokenLine}}.
xor : {token, {t_logical_xor, TokenLine}}.
<< : {token, {t_sl, TokenLine}}.
>> : {token, {t_sr, TokenLine}}.
< : {token, {t_smaller, TokenLine}}.
> : {token, {t_greater, TokenLine}}.
\* : {token, {t_mul, TokenLine}}.
/ : {token, {t_div, TokenLine}}.
- : {token, {t_minus, TokenLine}}.
\+ : {token, {t_plus, TokenLine}}.
\% : {token, {t_mod, TokenLine}}.
\. : {token, {t_concat, TokenLine}}.
~ : {token, {t_bnot, TokenLine}}.
& : {token, {t_band, TokenLine}}.
\| : {token, {t_bor, TokenLine}}.
\^ : {token, {t_bxor, TokenLine}}.
\! : {token, {t_not, TokenLine}}.

%% assignments
&= : {token, {t_and_equal, TokenLine}}.
\.= : {token, {t_concat_equal, TokenLine}}.
/= : {token, {t_div_equal, TokenLine}}.
-= : {token, {t_minus_equal, TokenLine}}.
\%= : {token, {t_mod_equal, TokenLine}}.
\*= : {token, {t_mul_equal, TokenLine}}.
\|= : {token, {t_or_equal, TokenLine}}.
\+= : {token, {t_plus_equal, TokenLine}}.
<<= : {token, {t_sl_equal, TokenLine}}.
>>= : {token, {t_sr_equal, TokenLine}}.
\^= : {token, {t_xor_equal, TokenLine}}.
= : {token, {t_equal, TokenLine}}.


%% strings
'.* : parse_string(TokenLine, TokenChars). % ' 
".* : parse_string(TokenLine, TokenChars). % " 

%% numbers
0(x|X)[0-9a-f]+ :
			{token, {t_number, TokenLine, erlang:list_to_integer(string:sub_string(TokenChars, 3), 16)}}.
0[0-7]+ :
			{token, {t_number, TokenLine, erlang:list_to_integer(TokenChars, 8)}}.
{D}+ :		{token, {t_number, TokenLine, list_to_integer(TokenChars)}}.

%% other tokens
\$[a-zA-Z0-9_]+ :
			{token, {t_variable, TokenLine, string:sub_string(TokenChars, 2)}}.
; :			{token, {';', TokenLine}}.
[a-zA-Z_][a-zA-Z_0-9]* :
			{token, {t_symbol, TokenLine, TokenChars}}.
\( :		{token, {'(', TokenLine}}.
\) :		{token, {')', TokenLine}}.
, :			{token, {',', TokenLine}}.
\: :		{token, {':', TokenLine}}.
\{ :		{token, {'{', TokenLine}}.
\} :		{token, {'}', TokenLine}}.

%% comments and whitespace
//.* : skip_token.
#.* : skip_token.
/\*([^*]|\*[^/])*\*/ : skip_token.
{WS}+ :		skip_token. %% whitespace


Erlang code.

parse_string(TokenLine, [First|TokenChars]) ->
	case process_chars(TokenChars, First, []) of
		error -> {error, "open string"};
		{S, PB} -> {token, {t_constant_encapsed_string, TokenLine, S}, PB}
	end.

process_chars([C|R], First, Acc) when C =:= First ->
	{lists:reverse(Acc), R};
process_chars([], _First, _Acc) ->
	error;
process_chars(Chars, First, Acc) ->
	{C, R} = process_char(Chars),
	process_chars(R, First, [C|Acc]).

process_char([$\\,$r,$\\,$n|R]) -> {$\n, R};
process_char([$\\,$a|R]) -> {$\a, R};
process_char([$\\,$b|R]) -> {$\b, R};
process_char([$\\,$f|R]) -> {$\f, R};
process_char([$\\,$n|R]) -> {$\n, R};
process_char([$\\,$r|R]) -> {$\r, R};
process_char([$\\,$t|R]) -> {$\t, R};
process_char([$\\,$v|R]) -> {$\v, R};
process_char([$\\,C|R]) -> {C, R};
process_char([C|R]) -> {C, R}.


