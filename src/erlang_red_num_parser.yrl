Header
"%% @noformat"
"%%"
"%% 2 B A Number or ! 2 B A Number - that is the responsibility of this parser"
"%% The num field in Node-RED is conveniently broad and handles many types"
"%% but that's the underlying NodeJS number handling facility."
"%%".

Nonterminals
  root
  float
  number
.

Terminals
  '.'
  '-'
  'e'
  hexadecimal
  binary
  integer
.

Rootsymbol
  root
.

root -> number : '$1'.

%%
%% This float handling is all about creating valid float strings for
%% string:to_float(...) to parse - it means adding leading zeros, adding
%% minuses in the right places and adding dot-zeros in the right places.
%%   for example: string:to_float(".12") --> {error,no_float}
%%                string:to_float("1e1") --> {error,no_float}
%%                string:to_float("0e-1") --> {error,no_float}
%% so this code.
%% The various possibilites are found in test case
%%     https://flows.red-erik.org/f/fb50bac16667fc54
float -> integer '.' integer 'e' '-' integer : convert_float([e3('$1'), ".", e3('$3'), "e-", e3('$6')]).
float -> integer '.' integer 'e' integer     : convert_float([e3('$1'), ".", e3('$3'), "e", e3('$5')]).
float -> '.' integer 'e' '-' integer         : convert_float(["0.", e3('$2'), "e-", e3('$5')]).
float -> '.' integer 'e' integer             : convert_float(["0.", e3('$2'), "e", e3('$4')]).
float -> integer 'e' '-' integer             : convert_float([e3('$1'), ".0e-", e3('$4')]).
float -> integer 'e' integer                 : convert_float([e3('$1'), ".0e", e3('$3')]).
float -> integer '.' 'e' '-' integer         : convert_float([e3('$1'), ".0e-", e3('$5')]).
float -> integer '.' 'e' integer             : convert_float([e3('$1'), ".0e", e3('$4')]).
float -> integer '.' integer                 : convert_float([e3('$1'), ".", e3('$3')]).
float -> '.' integer                         : convert_float(["0.", e3('$2')]).

number -> '-' float : '$2' * -1.
number -> float : '$1'.

number -> '-' binary : convert_binary('$2') * -1.
number -> binary : convert_binary('$1').

number -> '-' hexadecimal : convert_hex('$2') * -1.
number -> hexadecimal : convert_hex('$1').

number -> '-' integer : element(1,string:to_integer(element(3,'$2'))) * -1.
number -> integer : element(1,string:to_integer(element(3,'$1'))).

%%
%%
Erlang code.

e3(T) ->
    element(3, T).

convert_float(Lst) ->
    element(1, string:to_float(list_to_binary(Lst))).

%%
convert_hex({_, _, [$0 | V]}) ->
    convert_hex_remove_x(V).

convert_hex_remove_x([$X | V]) ->
    hexstring_to_number(V, length(V) rem 2);
convert_hex_remove_x([$x | V]) ->
    hexstring_to_number(V, length(V) rem 2).

hexstring_to_number(V, 1) ->
    hexstring_to_number([$0 | V], 0);
hexstring_to_number(V, 0) ->
    binary:decode_unsigned(binary:decode_hex(list_to_binary(V))).

%%
convert_binary({_, _, [$0 | V]}) ->
    convert_hex_remove_b(V).

convert_hex_remove_b([$B | V]) ->
    binary_list_to_integer(lists:reverse(V), 0, 1);
convert_hex_remove_b([$b | V]) ->
    binary_list_to_integer(lists:reverse(V), 0, 1).

binary_list_to_integer([], Total, _) ->
    Total;
binary_list_to_integer([$1 | Rest], Total, CurrentValue) ->
    binary_list_to_integer(Rest, Total + CurrentValue, CurrentValue * 2);
binary_list_to_integer([$0 | Rest], Total, CurrentValue) ->
    binary_list_to_integer(Rest, Total, CurrentValue * 2).
