-module(buffer_type_parser_test).

-include_lib("eunit/include/eunit.hrl").

foreach_parser_failure_test_() ->
    Tests = [
      {
       missing_closing_bracket,
       "[0xfeedbabe,"
      },
      {
       not_quite_empty_strings_quotes_data,
       "[\"-\",'+']"
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_binary_type_leex:string(SrcString),
                         R = erlang_red_binary_type_parser:parse(Tokens),
                         ?assertEqual(error, element(1,R))
                 end
     } || {TestCaseName, SrcString} <- Tests],

    {inparallel, TestList}.

foreach_parser_test_() ->
    Tests = [
      {
       empty_array,
       "[]",
       []
      },
      {
       simple_integer,
       "[1,2,3]",
       [1,2,3]
      },
      {
       hexadecimal_values_trailing_comma,
       "[0xfeedbabe,]",
       [4276992702]
      },
      {
       binary_values,
       "[0b1,0b0,0b11,0b10]",
       [1,0,3,2]
      },
      {
       binary_hex_float_int_values,
       "[0b1,0b0,0b11,0b10,0xff,-0x34,-12,33,2.123]",
       [1,0,3,2,255,-52,-12,33,2.123]
      },
      {
       negative_binary_hex_float_int_values,
       "[-0b1,-0b0,-0b11,-0b10,-0xff,-0x34]",
       [-1,0,-3,-2,-255,-52]
      },
      {
       string_quotes_data,
       "[\"-0b1\",'-0b0',\"-0b11\",-0b10,-0xff,\"-0x34\", \"-12.32\"]",
       [-1,0,-3,-2,-255,-52,-12.32]
      },
      {
       ignore_plus_symbolic,
       "[\"+0b1\",'+0b0',\"+0b11\",+0b10,+0xff,\"+0x34\", \"+12.32\"]",
       [1,0,3,2,255,52,12.32]
      },
      {
       empty_strings_quotes_data,
       "[\"\",'', '', '', \"\"]",
       [0,0,0,0,0]
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_binary_type_leex:string(SrcString),

                         {ok, Result} =
                             erlang_red_binary_type_parser:parse(Tokens),

                         ?assertEqual(ResultValue, Result)
                 end
                } || {TestCaseName, SrcString, ResultValue} <- Tests],

    {inparallel, TestList}.
