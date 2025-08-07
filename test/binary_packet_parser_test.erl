%%% % @noformat

-module(binary_packet_parser_test).

-include_lib("eunit/include/eunit.hrl").

collect_png_chunks(<<>>, Acc, _ChunkFunc) ->
    lists:reverse(Acc);
collect_png_chunks(Data, Acc, ChunkFunc) ->
    {Chunk, Rest} = ChunkFunc(Data),
    collect_png_chunks(Rest, [Chunk | Acc], ChunkFunc).

check_parsing_of_png_image_test() ->
    HeaderDef = "
       x8 => 0x89,
       x8 => 0x50,
       x8 => 0x4E,
       x8 => 0x47,
       x8 => 0x0D,
       x8 => 0x0A,
       x8 => 0x1A,
       x8 => 0x0A
    ",

    ChunkDef =
       "b32         => length,
        b8[4]       => type,
        b8[$length] => data,
        b32         => crc",

    {ok, HeaderFunc} = erl_packetparser:erlang_func_for_packetdef(HeaderDef),
    {ok, ChunkFunc} = erl_packetparser:erlang_func_for_packetdef(ChunkDef),

    {ok, PngData} =
        file:read_file(code:priv_dir(erlang_red_parsers) ++ "/test.png"),

    % skim off the header.
    {#{}, RestData} = HeaderFunc(PngData),

    % retrieve all chunks defined in the png
    Chunks = collect_png_chunks(RestData, [], ChunkFunc),
    Types = [T || #{ <<"type">> := T } <- Chunks ],

    ?assertEqual(["IHDR","iCCP","eXIf","iTXt","IDAT","IEND"], Types).

foreach_packet_parser_end_to_end_test_() ->
    Tests = [
      {
       single_byte_no_label,
       "b8",
       <<123>>,
       { #{}, <<>> }
      },
      {
       single_byte_big_endian,
       "b8 => f1",
       <<223>>,
       { #{ <<"f1">> => 223 }, <<>> }
      },
      {
       single_byte_big_endian_signed,
       "-b8 => f1",
       <<223>>,
       { #{ <<"f1">> => -33 }, <<>> }
      },
      {
       single_byte_little_endian,
       "l8 => f1",
       <<223>>,
       { #{ <<"f1">> => 223 }, <<>> }
      },
      {
       single_byte_little_endian_and_signed,
       "-l8 => f1",
       <<223>>,
       { #{ <<"f1">> => -33 }, <<>> }
      },
      {
       single_byte,
       "x1 => 1, b7 => len",
       <<130>>,
       { #{ <<"len">> => 2 }, <<>> }
      },
      {
       single_byte_off_boundary,
       "x1 => 1,
        b8 => len,
        x7",
       <<130, 123>>,
       { #{ <<"len">> => 4 }, <<>> }
      },
      {
       using_bit_boundary,
       "x1 => 1,
        b8 => len",
       <<130, 123>>,
       { #{ <<"len">> => 4 }, <<123:7>> }
      },
      {
       single_byte_off_boundary_with_structure,
       "x1 => 1,
        b8{ b4 => len1, b4 => len2 },
        x7",
       <<130, 123>>,
       { #{ <<"len1">> => 0, <<"len2">> => 4 }, <<>> }
      },
      {
       arrays_of_seven_bits,
       "b7[4] => values",
       <<130, 123, 164, 193>>,
       { #{ <<"values">> => [65,30,116,76] }, <<1:4>> }
      },
      {
       arrays_of_seven_bits_ignore_last_four_bits,
       "b7[4] => values, x4",
       <<130, 123, 164, 193>>,
       { #{ <<"values">> => [65,30,116,76] }, <<>> }
      },
      {
       structures_can_have_arrays,
       "b32{ b7[4] => values, x4 }",
       <<130, 123, 164, 193>>,
       { #{ <<"values">> => [65,30,116,76] }, <<>> }
      },
      {
       structures_can_have_arrays_reverse_naming,
       "b32{ values: b7[4], x4 }",
       <<130, 123, 164, 193>>,
       { #{ <<"values">> => [65,30,116,76] }, <<>> }
      },
      {
        little_endianness,
        "x8,
         b8 => value,
         l8 => value2,
         l16 => value3,
         l32{l8 => f1, l16 => f2, l4 => f3, l4 => f4},
         l8 => last",
        <<255,234,212,213,124,221,123,231,231,231>>,
       { #{ <<"f1">> => 221, <<"f2">> => 59259, <<"f3">> => 14,
            <<"f4">> => 7, <<"last">> => 231, <<"value">> => 234,
            <<"value2">> => 212, <<"value3">> => 31957 }, <<>> }
      },
      {
        big_endianness,
        "x8,
         l8 => value,
         b8 => value2,
         b16 => value3,
         b32{-l8 => f1, -l16 => f2, l4 => f3, l4 => f4},
         b8 => last",
        <<255,234,212,213,124,221,123,231,231,231>>,
       { #{ <<"f1">> => -35, <<"f2">> => -6277, <<"f3">> => 14,
            <<"f4">> => 7, <<"last">> => 231, <<"value">> => 234,
            <<"value2">> => 212, <<"value3">> => 54652 }, <<>> }
      }
     ],
    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Hash} =
                             erl_packetparser:execute(Stanza, BinaryData),
                         ?assertEqual(Result, Hash)
                 end
     } || {TestCaseName, Stanza, BinaryData, Result} <- Tests],

    {inparallel, TestList}.


foreach_packet_parser_successful_test_() ->
    Tests = [
      {
       parsing_png_datastream_header,
       "x8 => 0x89,
        x8 => 0x50,
        x8 => 0x4E,
        x8 => 0x47,
        x8 => 0x0D,
        x8 => 0x0A,
        x8 => 0x1A,
        x8 => 0x0A,
        b32         => length,
        b8[4]       => type,
        b8[$length] => data,
        b32         => crc
       ",
       "fun (Binary) ->
           <<137:8, 80:8, 78:8, 71:8, 13:8, 10:8, 26:8, 10:8,
              Vlength:32/integer-big-unsigned, Vtype:32/bits,
              Vdata:(8*Vlength)/bits, Vcrc:32/integer-big-unsigned,
                     UnmatchedBytes/bits>> = Binary,

           { #{ <<\"length\">> => Vlength,
               <<\"type\">> => [ X || <<X:8/integer-big-unsigned>> <= Vtype],
                <<\"data\">> => Vdata, <<\"crc\">> => Vcrc },
             UnmatchedBytes
           }
        end."
      },
      {
        trailing_commas_are_ok,
        "x1,",
        "fun (Binary) -> <<_VinternalField1:1/bits, UnmatchedBytes/bits>> = Binary, { #{ }, UnmatchedBytes } end."
      },
      {
        trailing_commas_are_ok_with_label,
        "x1 => 0x23,",
        "fun (Binary) -> <<35:1, UnmatchedBytes/bits>> = Binary, { #{ }, UnmatchedBytes } end."
      },
      {
       word_on_word_boundary_with_structure,
       "x1 => 1,
        b8{ b4 => len1, b4 => len2 },
        x7",
       "fun (Binary) ->
             <<1:1, VinternalField1:8/bits, _VinternalField2:7/bits,
                UnmatchedBytes/bits>> = Binary,

             <<Vlen1:4/integer-big-unsigned,
                      Vlen2:4/integer-big-unsigned>> = VinternalField1,

             { #{ <<\"len2\">> => Vlen2, <<\"len1\">> => Vlen1 }, UnmatchedBytes }
        end."
      },
      {
       word_on_word_boundary,
       "x1 => 1,
        b8 => len,
        x7",
       "fun (Binary) ->
            <<1:1, Vlen:8/integer-big-unsigned, _VinternalField1:7/bits,
                  UnmatchedBytes/bits>> = Binary,

            { #{ <<\"len\">> => Vlen }, UnmatchedBytes }
        end."
      },
      {
       match_on_one_bit,
       "x1 => 1,
        b7 => len",
       "fun (Binary) ->
            <<1:1, Vlen:7/integer-big-unsigned, UnmatchedBytes/bits>> = Binary,

           { #{ <<\"len\">> => Vlen }, UnmatchedBytes }
        end."
      },
      {
       ignore_can_be_given_expected_value,
       "x1 => 1,
        x4 => 12,
        36: x6,
        b7 => len",
       "fun (Binary) ->
          <<1:1, 12:4, 36:6, Vlen:7/integer-big-unsigned,
                UnmatchedBytes/bits>> = Binary,

          { #{ <<\"len\">> => Vlen }, UnmatchedBytes }
        end."
      },
      {
       hashmap_definition_ensure_names_can_be_specs,
       "x8z   => 12,
        b8    => b82,
        l24a  => l123,
        b8[4] => b89,
        b8    => b8f,
        b16f  => volt,
        b16   => temp,
        b8    => hum,
        b8    => crc,
        x8",
       "fun (Binary) ->
           <<12:8, Vb82:8/integer-big-unsigned, Vl123:24/integer-little-unsigned,
             Vb89:32/bits, Vb8f:8/integer-big-unsigned,
             Vvolt:16/integer-big-unsigned, Vtemp:16/integer-big-unsigned,
             Vhum:8/integer-big-unsigned, Vcrc:8/integer-big-unsigned,
             _VinternalField1:8/bits, UnmatchedBytes/bits>> = Binary,

           { #{ <<\"b82\">> => Vb82, <<\"l123\">> => Vl123,
                <<\"b89\">> => [ X || <<X:8/integer-big-unsigned>> <= Vb89],
                <<\"b8f\">> => Vb8f, <<\"volt\">> => Vvolt,
                <<\"temp\">> => Vtemp, <<\"hum\">> => Vhum,
                <<\"crc\">> => Vcrc }, UnmatchedBytes }
        end."
      },
      {
       hashmap_definition,
       "x8z,
        b8    => len,
        l24a  => id,
        b8[4] => tag,
        b8    => status,
        b16f  => volt,
        b16   => temp,
        b8    => hum,
        b8    => crc,
        x8",
       "fun (Binary) ->
           <<_VinternalField1:8/bits, Vlen:8/integer-big-unsigned,
             Vid:24/integer-little-unsigned, Vtag:32/bits,
             Vstatus:8/integer-big-unsigned, Vvolt:16/integer-big-unsigned,
             Vtemp:16/integer-big-unsigned, Vhum:8/integer-big-unsigned,
             Vcrc:8/integer-big-unsigned, _VinternalField2:8/bits,
             UnmatchedBytes/bits>> = Binary,

           { #{ <<\"len\">> => Vlen, <<\"id\">> => Vid,
                <<\"tag\">> => [ X || <<X:8/integer-big-unsigned>> <= Vtag],
                <<\"status\">> => Vstatus, <<\"volt\">> => Vvolt,
                <<\"temp\">> => Vtemp, <<\"hum\">> => Vhum,
                <<\"crc\">> => Vcrc },
             UnmatchedBytes }
        end."
      },
      {
       curly_brackets_specification,
       "x8,
        b8,
        l124,
        b8[4],
        b17{b3,x6,-b8}",
       "fun (Binary) ->
            <<_VinternalField1:8/bits, VinternalField2:8/integer-big-unsigned,
              VinternalField3:124/integer-little-unsigned,
              VinternalField4:32/bits, VinternalField5:17/bits,
              UnmatchedBytes/bits>> = Binary,

            <<VinternalField5:3/integer-big-unsigned, _VinternalField6:6/bits,
                VinternalField7:8/integer-big-signed>> = VinternalField5,

            { #{ }, UnmatchedBytes }
        end."
       },
      {
       curly_brackets_specification_with_field_names,
       "x8,
        b8,
        l124,
        b8[4],
        b17{b3 => value, x6, -b8 => count}",
       "fun (Binary) ->
            <<_VinternalField1:8/bits, VinternalField2:8/integer-big-unsigned,
              VinternalField3:124/integer-little-unsigned,
              VinternalField4:32/bits, VinternalField5:17/bits,
              UnmatchedBytes/bits>> = Binary,

            <<Vvalue:3/integer-big-unsigned, _VinternalField5:6/bits,
              Vcount:8/integer-big-signed>> = VinternalField5,

            { #{ <<\"count\">> => Vcount, <<\"value\">> => Vvalue },
               UnmatchedBytes }
        end."
       },
      {
       naming_using_colon,
       "x8,
        value: b8,
        value2: l124,
        b8[4] => value3,
        b17{b3 => value, x6, -b8 => count}",
       "fun (Binary) ->
            <<_VinternalField1:8/bits, Vvalue:8/integer-big-unsigned,
              Vvalue2:124/integer-little-unsigned, Vvalue3:32/bits,
              VinternalField2:17/bits, UnmatchedBytes/bits>> = Binary,

            <<Vvalue:3/integer-big-unsigned, _VinternalField2:6/bits,
              Vcount:8/integer-big-signed>> = VinternalField2,

            { #{ <<\"count\">> => Vcount, <<\"value\">> => Vvalue,
                 <<\"value3\">> => [ X || <<X:8/integer-big-unsigned>> <= Vvalue3],
              <<\"value2\">> => Vvalue2, <<\"value\">> => Vvalue },
             UnmatchedBytes }
        end."
       },
      {
       single_byte_unlabeled_byte,
       "b8",
       "fun (Binary) ->
            <<VinternalField1:8/integer-big-unsigned,
                UnmatchedBytes/bits>> = Binary,
            { #{ }, UnmatchedBytes }
        end."
       },
      {
        multiple_structure_definitions,
         "b8{ b1 => v1_1, b2 => v1_2, b5 => v1_3 },
          b16{ b4 => v2_1, b8 => v2_2, b4 => v3_3 },
          b8 => value,
          b8{ b1 => v3_1, x4, b3 => v3_2 }",
         "fun (Binary) ->
              <<VinternalField1:8/bits, VinternalField2:16/bits,
                Vvalue:8/integer-big-unsigned, VinternalField3:8/bits,
                UnmatchedBytes/bits>> = Binary,

              <<Vv1_1:1/integer-big-unsigned, Vv1_2:2/integer-big-unsigned,
                Vv1_3:5/integer-big-unsigned>> = VinternalField1,

              <<Vv2_1:4/integer-big-unsigned, Vv2_2:8/integer-big-unsigned,
                Vv3_3:4/integer-big-unsigned>> = VinternalField2,

              <<Vv3_1:1/integer-big-unsigned, _VinternalField3:4/bits,
                Vv3_2:3/integer-big-unsigned>> = VinternalField3,

             { #{ <<\"v3_2\">> => Vv3_2, <<\"v3_1\">> => Vv3_1,
                 <<\"value\">> => Vvalue, <<\"v1_3\">> => Vv1_3,
                 <<\"v1_2\">> => Vv1_2, <<\"v1_1\">> => Vv1_1,
                 <<\"v2_1\">> => Vv2_1, <<\"v2_2\">> => Vv2_2,
                 <<\"v3_3\">> => Vv3_3 },
              UnmatchedBytes }
          end."
       }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_packet_type_leex:string(SrcString),
                         {ok, Result} =
                             erlang_red_packet_type_parser:parse(Tokens),

                         ExpectedStringRmSpace = list_to_binary(
                                                 re:replace(Expec,
                                                            "\\s+", " ",
                                                            [global])
                                                ),

                         ResultStringRmSpace = list_to_binary(
                                                 re:replace(Result,
                                                            "\\s+", " ",
                                                            [global])
                                                ),

                         ?assertEqual(ExpectedStringRmSpace, ResultStringRmSpace)
                 end
     } || {TestCaseName, SrcString, Expec} <- Tests],

    {inparallel, TestList}.

foreach_packet_parser_failed_test_() ->
    Tests = [
      {
       empty_structure_not_allowed,
       "b8{} => var_name"
       },
      {
       recursive_structures_not_allowed,
       "b8{ b7{ b6 => var3 } => var } => var_name"
       }
    ],
    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_packet_type_leex:string(SrcString),
                         R =
                             erlang_red_packet_type_parser:parse(Tokens),
                         ?assertEqual(error, element(1,R))
                 end
     } || {TestCaseName, SrcString} <- Tests],

    {inparallel, TestList}.

foreach_packet_leex_successful_test_() ->
    Tests = [
      {
       hashmap_definition,
       "x8,
          b8 => len, l24a => id, b8[4] => tag,
            b8 => status, b16 => volt, b16 => temp, b8 => hum, b8 => crc, x8"
      },
      {
       curly_brackets_specification,
       "x8, b8, l124, b8[4], b17{b3,x6,-b8}"
       },
      {
       curly_brackets_specification,
       "x8, b8, l124, b8[4], b17{b3 => value,x6,-b8 => count}"
       },
      {
       naming_using_colon,
       "x8, value: b8, value2: l124, b8[4] => value3,
                            b17{b3 => value,x6,-b8 => count}"
       },
      {
       hashmap_definition_names_are_binary_specifications,
       "x8 => x8,
          b8 => b81, l24a => l123, b8[4] => b89,
            b8 => status, b16 => volt, b16 => temp, b8 => hum, b8 => crc, x8"
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         R = erlang_red_packet_type_leex:string(SrcString),
                         ?assertEqual(ok, element(1,R))
                 end
     } || {TestCaseName, SrcString} <- Tests],

    {inparallel, TestList}.
