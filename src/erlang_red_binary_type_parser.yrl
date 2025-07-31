Header
"%%"
"%% Nice here. That is a quote from The Peppers."
"%%".

Nonterminals
  root
  float
  number
  list_of_stuff
.

Terminals
  ','
  '['
  ']'
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

root -> '[' ']' : [].
root -> '[' list_of_stuff ']' : lists:flatten('$2').

list_of_stuff -> number : ['$1'].
list_of_stuff -> number ',' : ['$1'].
list_of_stuff -> number ',' list_of_stuff : ['$1', '$3'].
%% trailing commas are ignored
list_of_stuff -> number ',' list_of_stuff ',' : ['$1', '$3'].

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
