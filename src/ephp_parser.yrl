Nonterminals stats stat exp arglist varlist
	elseifs else elseif elsetail
	switchcases switchcase switchcasebody.
Terminals ';' t_number t_logical_or t_logical_and t_equal t_variable '(' ')' t_symbol ',' '{' '}' t_function t_if t_elseif t_else
	t_for t_while t_mul t_div t_mod t_plus t_minus t_inc t_dec t_echo t_constant_encapsed_string t_concat t_return t_do
	t_and_equal t_band t_logical_xor t_bor t_boolean_and t_boolean_or t_is_equal t_is_greater_or_equal
	t_is_not_equal t_is_smaller_or_equal t_sl t_sr t_smaller t_greater t_bxor t_bnot t_not
	t_concat_equal t_div_equal t_minus_equal t_mod_equal t_mul_equal t_or_equal t_plus_equal t_sl_equal t_sr_equal t_xor_equal
	t_break ':' t_goto t_switch t_default t_case t_continue.
Rootsymbol stats.

Unary 20 t_else t_elseif.
Right 50 t_equal t_and_equal t_concat_equal t_div_equal t_minus_equal t_mod_equal t_mul_equal t_or_equal t_plus_equal t_sl_equal t_sr_equal t_xor_equal.
Left 100 t_logical_or t_logical_xor t_boolean_or.
Left 200 t_logical_and t_boolean_and.
Left 250 t_smaller t_greater t_is_smaller_or_equal t_is_greater_or_equal.
Left 300 t_is_equal t_is_not_equal. % t_is_identical t_is_not_identical 
Right 400 t_concat.
Left 500 t_plus t_minus.
Left 600 t_mul t_div t_mod.
Left 630 t_bor t_bxor.
Left 650 t_band.
Left 670 t_sl t_sr.
Unary 700 t_inc t_dec.
Unary 800 t_bnot t_not.
% Unary 800 uminus.

stats -> stat : ['$1'].
stats -> stat stats : ['$1'|'$2'].

stat -> exp ';' : {exp, '$1'}.
stat -> '{' stats '}' : '$2'.
stat -> '{' '}' : [].
stat -> t_function t_symbol '(' varlist ')' stat : {fundef, {value('$2'), '$4', '$6'}}. %% function definition
stat -> t_if '(' exp ')' stat elsetail : {'if', {'$3', '$5', '$6'}}.
stat -> t_for '(' exp ';' exp ';' exp ')' stat : {'for', {{'$3', '$5', '$7'}, '$9'}}.
stat -> t_while '(' exp ')' stat : {'while', {'$3', '$5'}}.
stat -> t_do stat t_while '(' exp ')' ';': {'dowhile', {'$2', '$5'}}.
stat -> t_echo exp ';' : {echo, '$2'}.
% stat -> t_return ';': {return, null}.
stat -> t_return exp ';': {return, '$2'}.
stat -> t_break ';' : {break}.
stat -> t_continue ';' : {continue}.
stat -> t_symbol ':' : {'label', value('$1')}.
stat -> t_goto t_symbol ';' : {'goto', value('$2')}.
stat -> t_switch '(' exp ')' '{' switchcases '}' : {'switch', {'$3', '$6'}}.

elsetail -> elseifs else : '$1' ++ ['$2'].
elsetail -> elseifs : '$1'.
elsetail -> else : ['$1'].
elsetail -> '$empty' : [].

else -> t_else stat : {'else', '$2'}.
elseif -> t_elseif '(' exp')' stat : {'elseif', '$3', '$5'}.
elseifs -> elseif : ['$1'].
elseifs -> elseif elseifs : ['$1'|'$2'].

switchcases -> switchcase : ['$1'].
switchcases -> switchcase switchcases : ['$1'|'$2'].
switchcase -> t_case exp ':' switchcasebody : {'case', {'$2', '$4'}}.
switchcase -> t_default ':' switchcasebody : {'default', '$3'}.
switchcasebody -> '$empty' : [].
switchcasebody -> stats : '$1'.

arglist -> exp : ['$1'].
arglist -> exp ',' arglist : ['$1'|'$3'].
arglist -> '$empty' : [].

varlist -> t_variable : [value('$1')].
varlist -> t_variable ',' varlist : [value('$1')|'$3'].
varlist -> '$empty' : [].

exp -> '(' exp ')' : '$2'.
exp -> t_symbol '(' arglist ')' : {fcall, {value('$1'), '$3'}}. %% function call

%% assignment
exp -> t_variable t_equal exp : {assign, {{var, value('$1')}, '$3'}}.
exp -> t_variable t_and_equal exp : {assign, {{var, value('$1')}, {'band', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_concat_equal exp : {assign, {{var, value('$1')}, {'concat', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_div_equal exp : {assign, {{var, value('$1')}, {'div', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_minus_equal exp : {assign, {{var, value('$1')}, {'sub', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_mod_equal exp : {assign, {{var, value('$1')}, {'mod', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_mul_equal exp : {assign, {{var, value('$1')}, {'mul', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_or_equal exp : {assign, {{var, value('$1')}, {'bor', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_plus_equal exp : {assign, {{var, value('$1')}, {'add', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_sl_equal exp : {assign, {{var, value('$1')}, {'bsl', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_sr_equal exp : {assign, {{var, value('$1')}, {'bsr', {{var, value('$1')}, '$3'}}}}.
exp -> t_variable t_xor_equal exp : {assign, {{var, value('$1')}, {'bxor', {{var, value('$1')}, '$3'}}}}.

%% operators (binary)
exp -> exp t_logical_or exp : {land, {'$1', '$3'}}.
exp -> exp t_logical_and exp : {lor, {'$1', '$3'}}.
exp -> exp t_logical_xor exp : {lxor, {'$1', '$3'}}.
exp -> exp t_plus exp : {add, {'$1', '$3'}}.
exp -> exp t_minus exp : {sub, {'$1', '$3'}}.
exp -> exp t_mul exp : {mul, {'$1', '$3'}}.
exp -> exp t_div exp : {'div', {'$1', '$3'}}.
exp -> exp t_mod exp : {mod, {'$1', '$3'}}.
exp -> exp t_concat exp : {concat, {'$1', '$3'}}.
exp -> exp t_band exp : {'band', {'$1', '$3'}}.
exp -> exp t_bor exp : {'bor', {'$1', '$3'}}.
exp -> exp t_boolean_and exp : {'booland', {'$1', '$3'}}.
exp -> exp t_boolean_or exp : {'boolor', {'$1', '$3'}}.
exp -> exp t_is_equal exp : {'eq', {'$1', '$3'}}.
exp -> exp t_is_greater_or_equal exp : {'ge', {'$1', '$3'}}.
% exp -> exp t_is_identical exp : {'ident', {'$1', '$3'}}.
% exp -> exp t_is_not_identical exp : {'notident', {'$1', '$3'}}.
exp -> exp t_is_not_equal exp : {'neq', {'$1', '$3'}}.
exp -> exp t_is_smaller_or_equal exp : {'le', {'$1', '$3'}}.
exp -> exp t_sl exp : {'bsl', {'$1', '$3'}}.
exp -> exp t_sr exp : {'bsr', {'$1', '$3'}}.
exp -> exp t_smaller exp : {'lt', {'$1', '$3'}}.
exp -> exp t_greater exp : {'gt', {'$1', '$3'}}.
exp -> exp t_bxor exp : {'bxor', {'$1', '$3'}}.

%% operators (unary)
exp -> t_inc exp : {increment, '$2'}.
exp -> t_dec exp : {decrement, '$2'}.
% exp -> exp t_inc.
exp -> t_bnot exp : {'bnot', '$2'}.
exp -> t_not exp : {'not', '$2'}.
exp -> t_minus exp : {'sub', {{num, 0}, '$2'}}.

%% values
exp -> t_variable : {var, value('$1')}.
exp -> t_number : {num, value('$1')}.
exp -> t_symbol : {sym, value('$1')}.
exp -> t_constant_encapsed_string : {string, value('$1')}.

Erlang code.

% ts(T) -> element(1, T). %% terminal symbol
% lineno(T) -> element(2, T). %% line number
value(T) -> element(3, T). %% value (may not be there)
