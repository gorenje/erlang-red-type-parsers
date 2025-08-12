-module(num_parser_test).

-include_lib("eunit/include/eunit.hrl").

foreach_parser_failure_test_() ->
    Tests = [
        {
            parse_error,
            "-.e-1"
        }
    ],
    TestList = [
        {
            atom_to_binary(TestCaseName),
            timeout,
            5,
            fun() ->
                {ok, Tokens, _} =
                    erlang_red_num_leex:string(SrcString),

                Result = erlang_red_num_parser:parse(Tokens),

                ?assertEqual(error, element(1, Result))
            end
        }
     || {TestCaseName, SrcString} <- Tests
    ],

    {inparallel, TestList}.

foreach_parser_success_test_() ->
    Tests = [
        {
            minus_one_thousand,
            "-1.e003",
            -1.0e3
        },
        {
            minus_one_thousandst,
            "-1.e-003",
            -0.001
        },
        {
            most_painful_float,
            "-.12e-23",
            -1.2e-24
        },
        {
            zero_float,
            "0e0",
            0.0
        },
        {
            negative_zero_float,
            "-.0e-1",
            -0.0
        },
        {
            negative_zero_float_one,
            "-.0e-0",
            -0.0
        },
        {
            most_basic_float,
            "12.12",
            12.12
        },
        {
            floats_no_leading_zero,
            ".12e1",
            1.2
        },
        {
            simple_binary_longer_capital,
            "0B111010100001",
            3745
        },
        {
            simple_octal_value,
            "0o7",
            7
        },
        {
            octal_negative_value,
            "-0o7123_1211_3333_7777",
            -252005216796671
        },
        {
            simple_binary_longer,
            "0b1010100001",
            673
        },
        {
            simple_binary_with_underscores,
            "0b1010_1000_01_____",
            673
        },
        {
            simple_binary,
            "0b1",
            1
        },
        {
            simple_binary_zeros,
            "0B000000000000000000000000000000",
            0
        },
        {
            simple_binary_zero,
            "0B0",
            0
        },
        {
            simple_hexadecimal,
            "0x12",
            18
        },
        {
            long_hexadecimal_with_underscores,
            "0x1234_5678_9abc_def___",
            81985529216486895
        },
        {
            simple_capital_hexadecimal,
            "0X12",
            18
        },
        {
            simple_negative_hexadecimal,
            "-0x12",
            -18
        },
        {
            simple_negative_capital_hexadecimal,
            "-0X12",
            -18
        },
        {
            handle_uneven,
            "-0Xa",
            -10
        },
        {
            handle_uneven,
            "-0X0a",
            -10
        },
        {
            simple_negative_integer,
            "-132",
            -132
        },
        {
            simple_integer,
            "132",
            132
        }
    ],
    TestList = [
        {
            atom_to_binary(TestCaseName),
            timeout,
            5,
            fun() ->
                {ok, Tokens, _} =
                    erlang_red_num_leex:string(SrcString),

                {ok, Result} = erlang_red_num_parser:parse(Tokens),

                ?assertEqual(ResultValue, Result)
            end
        }
     || {TestCaseName, SrcString, ResultValue} <- Tests
    ],

    {inparallel, TestList}.

foreach_leex_failure_test_() ->
    Tests = [
        {
            hello_world_isnt_num,
            "Hwllo World"
        },
        {
            quotes_arent_numbers,
            "\"312\""
        },
        {
            single_quotes_arent_numbers,
            "'123'"
        },
        {
            g_isnt_hex,
            "0xfeedg0d"
        },
        {
            hex_isnt_hex,
            "0xhex"
        },
        {
            no_leading_underscores_binary,
            "0b_1010_10101"
        },
        {
            no_leading_underscores_hexadecimal,
            "0x_abcd_113_123"
        }
    ],

    TestList = [
        {
            atom_to_binary(TestCaseName),
            timeout,
            5,
            fun() ->
                R = erlang_red_num_leex:string(SrcString),
                ?assertEqual(error, element(1, R))
            end
        }
     || {TestCaseName, SrcString} <- Tests
    ],

    {inparallel, TestList}.

foreach_leex_illegals_caught_by_parser_test_() ->
    %% illegal numbers are also tokenised but they don't get past the yecc'er
    Tests = [
        {
            double_minus,
            "--1"
        },
        {
            double_dot_float,
            "12..1234"
        },
        {
            illegal_float,
            "-.e-1"
        }
    ],

    TestList = [
        {
            atom_to_binary(TestCaseName),
            timeout,
            5,
            fun() ->
                R = erlang_red_num_leex:string(SrcString),
                ?assertEqual(ok, element(1, R))
            end
        }
     || {TestCaseName, SrcString} <- Tests
    ],

    {inparallel, TestList}.

foreach_leex_success_test_() ->
    Tests = [
        {
            simplest_float,
            "0.1"
        },
        {
            simplest_integer,
            "1"
        },
        {
            simplest_binary,
            "0b0"
        },
        {
            simplest_hex,
            "0x1"
        },
        {
            simplest_neg_float,
            "-0.1"
        },
        {
            simplest_neg_integer,
            "-1"
        },
        {
            simplest_neg_binary,
            "-0b0"
        },
        {
            simplest_neg_hex,
            "-0x1"
        }
    ],

    TestList = [
        {
            atom_to_binary(TestCaseName),
            timeout,
            5,
            fun() ->
                R = erlang_red_num_leex:string(SrcString),
                ?assertEqual(ok, element(1, R))
            end
        }
     || {TestCaseName, SrcString} <- Tests
    ],

    {inparallel, TestList}.
