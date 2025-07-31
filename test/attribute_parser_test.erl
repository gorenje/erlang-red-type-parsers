%%% % @noformat

-module(attribute_parser_test).

-include_lib("eunit/include/eunit.hrl").

foreach_parser_test_() ->
    Tests = [
      {
       name_after_bracket,
       "key['ddd']fubar",
       [<<"key">>,ddd,<<"fubar">>]
      },
      {
       array_indicies_dot_after_square_bracket,
       "key['ddd']fubar['d'].123.12",
       [<<"key">>,ddd,<<"fubar">>,d,{idx,123},{idx,12}]
      },
      {
       array_indicies_have_to_work,
       "key.123.456",
       [<<"key">>,{idx,123},{idx,456}]
      },
      {
       array_indicies_have_to_work_also,
       "key[123].456",
       [<<"key">>,{idx,123},{idx,456}]
      },
      {
       array_hash_name_after_brackets,
       "payload.key[1]key2",
       [<<"payload">>,<<"key">>,{idx,1},<<"key2">>]
      },
      {
       array_hash_name_after_brackets_in_quotes,
       "payload.key[1]'key2'",
       [<<"payload">>,<<"key">>,{idx,1},key2]
      },
      {
       array_hash_name_after_brackets_in_dbl_quotes,
       "payload.key[1]\"key2\"",
       [<<"payload">>,<<"key">>,{idx,1},<<"key2">>]
      },
      {
       single_char_also_works,
       "k",
       [<<"k">>]
      },
      {
       single_capital_char_also_works,
       "K",
       [<<"K">>]
      },
      {
       multiple_mixed_case_char_also_works,
       "K.l.M",
       [<<"K">>,<<"l">>,<<"M">>]
      },
      {
       multiple_mixed_case_char_also_works_lower_first,
       "k.L.m",
       [<<"k">>,<<"L">>,<<"m">>]
      },
      {
       simple_atomised_string,
       "key.key1.key2",
       [<<"key">>, <<"key1">>, <<"key2">>]
      },
      {
       double_quotes_allowed,
       "\"key\"",
       [<<"key">>]
      },
      {
       single_quote_starts_allowed,
       "'key-1-2'",
       ['key-1-2']
      },
      {
       even_keys_are_single,
       "key",
       [<<"key">>]
      },
      {
       even_capital_keys_are_single,
       "Key",
       [<<"Key">>]
      },
      {
       mix_of_quites_and_dots,
       "Key['Key-2-'].Fubar.snafu[\"ddd\"]",
       [<<"Key">>, 'Key-2-', <<"Fubar">>, <<"snafu">>, <<"ddd">>]
      },
      {
       another_combination,
       "req.headers[\"x-proto-forward-for\"]",
       [<<"req">>, <<"headers">>, <<"x-proto-forward-for">>]
      },
      {
       quoted_brackets,
       "req['[fubar]'].snafu",
       [<<"req">>, '[fubar]', <<"snafu">>]
      },
      {
       quoted_with_double_quotes_brackets,
       "req[\"[fubar]\"].snafu",
       [<<"req">>,<<"[fubar]">>,<<"snafu">>]
      },
      {
       all_numbers,
       "\"123\"[\"123\"]['456'][\"789\"]",
       [<<"123">>,<<"123">>,'456',<<"789">>]
      },
      {
       all_array_access,
       "abc[\"abc\"]['def'][\"ghi\"]['H']['I']",
       [<<"abc">>,<<"abc">>,'def',<<"ghi">>,'H','I']
      },
      {
       collection_of_dots,
       "abcd.a121.b2.c4.d5.e6.f7.g8.h9.i10",
       [<<"abcd">>,<<"a121">>,<<"b2">>,<<"c4">>,<<"d5">>,
        <<"e6">>,<<"f7">>,<<"g8">>,<<"h9">>,<<"i10">>]
      },
      {
       mixing_as_much_as_possible,
       "abcd['a121'].b2['c4'].d5['e6'].f7['g8'].h9['i10']",
       [<<"abcd">>,a121,<<"b2">>,c4,<<"d5">>,e6,<<"f7">>,g8,<<"h9">>,i10]
      },
      {
       mixing_as_much_as_possible_double_quotes,
       "abcd[\"a121\"].b2['c4'].d5[\"e6\"].f7['g8'].h9[\"i10\"]",
       [<<"abcd">>,<<"a121">>,<<"b2">>,c4,<<"d5">>,<<"e6">>,<<"f7">>,
        g8,<<"h9">>,<<"i10">>]
      },
      {
       mixing_as_much_as_possible_double_quotes_starting_with_string,
       "\"fubar\".abcd[\"a121\"].b2['c4'].d5[\"e6\"].f7['g8'].h9[\"i10\"]",
       [<<"fubar">>,<<"abcd">>,<<"a121">>,<<"b2">>,c4,<<"d5">>,
        <<"e6">>,<<"f7">>,g8,<<"h9">>,<<"i10">>]
      },
      {
       can_start_with_string,
       "\"eee\".fff[\"ggg\"]",
       [<<"eee">>,<<"fff">>,<<"ggg">>]
      },
      {
       single_quote_start_is_also_possible,
       "'ddd'.ddd.eee",
       [ddd,<<"ddd">>,<<"eee">>]
      },
      {
       single_quote_start,
       "'ddd'[\"eee\"].de",
       [ddd,<<"eee">>,<<"de">>]
      },
      {
       single_quote_start_no_bracket,
       "'ddd'.de",
       [ddd,<<"de">>]
      },
      {
       square_brackets_all_the_way_down,
       "\"ddd\"[\"dd\"][\"ff\"][\"gg\"][\"hh\"]",
       [<<"ddd">>,<<"dd">>,<<"ff">>,<<"gg">>,<<"hh">>]
      }
    ],


    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_attr_leex:string(SrcString),

                         {ok, Result} = erlang_red_attr_parser:parse(Tokens),

                         ?assertEqual(ResultValue, Result)
                 end
                } || {TestCaseName, SrcString, ResultValue} <- Tests],

    {inparallel, TestList}.

foreach_parser_failure_test_() ->
    Tests = [
      {
       starting_with_bracket,
       "[\"key\"]"
      },
      {
       starting_with_bracket_single_quote,
       "['key']"
      },
      {
       dot_bracket_does_not_work,
       "key.['ddd']"
      },
      {
       double_dots,
       "key..ddd"
      },
      {
       triple_dots,
       "key...ddd"
      },
      {
       brackets_without_quotes_does_not_work,
       "key[ddd]fubar"
      },
      {
       double_brackets,
       "key[['ddd']]fubar"
      },
      {
       missing_brackets,
       "key['ddd'fubar]"
      },
      {
       no_bracket_after_dot,
       "\"eee\".[\"ddd\"]"
      },
      {
       no_bracket_after_dot_single_quote,
       "\"eee\".['ddd']"
      },
      {
       no_bracket_after_dot_single_quote,
       "'eee'.['ddd']"
      },
      {
       string_dot_string_not_possible,
       "\"sadasd\".\"asdsad\""
      },
      {
       string_string_not_possible,
       "\"sadasd\"\"asdsad\""
      },
      {
       single_quote_string_dot_string_not_possible,
       "'sadasd'.'asdsad'"
      },
      {
       single_quote_string_string_not_possible,
       "'sadasd''asdsad'"
      },
      {
       mixing_as_much_as_possible_but_not_incorrectly,
       "abcd['a121'].b2.['c4'].d5.['e6'].f7.['g8'].h9.['i10']"
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_attr_leex:string(SrcString),
                         R = erlang_red_attr_parser:parse(Tokens),
                         ?assertEqual(error, element(1,R))
                 end
     } || {TestCaseName, SrcString} <- Tests],

    {inparallel, TestList}.

foreach_leex_failure_test_() ->
    Tests = [
      {
       uncomplete_single_quote,
       "key['ddd]fubar"
      },
      {
       uncomplete_double_quote,
       "key[\"ddd]fubar"
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         R = erlang_red_attr_leex:string(SrcString),
                         ?assertEqual(error, element(1,R))
                 end
     } || {TestCaseName, SrcString} <- Tests],

    {inparallel, TestList}.
