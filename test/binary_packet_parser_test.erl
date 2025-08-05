-module(binary_packet_parser_test).

-include_lib("eunit/include/eunit.hrl").

foreach_packet_parser_end_to_end_test_() ->
    Tests = [
      {
       single_byte,
       "b8 => f1",
       <<123>>,
       #{ <<"f1">> => <<123>>, <<"unmatched">> => <<>> }
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
       hashmap_definition_ensure_names_can_be_specs,
       "x8z => x8z,
          b8 => b82,
            l24a => l123,
              b8[4] => b89,
               b8 => b8f,
                b16f => volt,
                  b16 => temp,
                    b8 => hum,
                       b8 => crc,
            x8",
       "fun (Binary) -> <<_Vx8z:8/bits, Vb82:8/bits, Vl123:24/bits, Vb89:32/bits, Vb8f:8/bits, Vvolt:16/bits, Vtemp:16/bits, Vhum:8/bits, Vcrc:8/bits, _Vfield1:8/bits, Unmatched/bytes>> = Binary, #{ <<\"crc\">> => Vcrc, <<\"hum\">> => Vhum, <<\"temp\">> => Vtemp, <<\"volt\">> => Vvolt, <<\"b8f\">> => Vb8f, <<\"b89\">> => Vb89, <<\"l123\">> => Vl123, <<\"b82\">> => Vb82, <<\"unmatched\">> => Unmatched } end."
      },
      {
       hashmap_definition,
       "x8z,
          b8 => len,
            l24a => id,
              b8[4] => tag,
               b8 => status,
                b16f => volt,
                  b16 => temp,
                    b8 => hum,
                       b8 => crc,
            x8",
       "fun (Binary) -> <<_Vfield1:8/bits, Vlen:8/bits, Vid:24/bits, Vtag:32/bits, Vstatus:8/bits, Vvolt:16/bits, Vtemp:16/bits, Vhum:8/bits, Vcrc:8/bits, _Vfield2:8/bits, Unmatched/bytes>> = Binary, #{ <<\"crc\">> => Vcrc, <<\"hum\">> => Vhum, <<\"temp\">> => Vtemp, <<\"volt\">> => Vvolt, <<\"status\">> => Vstatus, <<\"tag\">> => Vtag, <<\"id\">> => Vid, <<\"len\">> => Vlen, <<\"unmatched\">> => Unmatched } end."
      },
      {
       curly_brackets_specification,
       "x8, b8, l124, b8[4], b17{b3,x6,-b8}",
       "fun (Binary) -> <<_Vfield1:8/bits, Vfield2:8/bits, Vfield3:124/bits, Vfield4:32/bits, Vfield5:17/bits, Unmatched/bytes>> = Binary, #{ <<\"field5\">> => Vfield5, <<\"field4\">> => Vfield4, <<\"field3\">> => Vfield3, <<\"field2\">> => Vfield2, <<\"unmatched\">> => Unmatched } end."
       },
      {
       curly_brackets_specification_with_field_names,
       "x8, b8, l124, b8[4], b17{b3 => value,x6,-b8 => count}",
       "fun (Binary) -> <<_Vfield1:8/bits, Vfield2:8/bits, Vfield3:124/bits, Vfield4:32/bits, Vfield5:17/bits, Unmatched/bytes>> = Binary, #{ <<\"field5\">> => Vfield5, <<\"field4\">> => Vfield4, <<\"field3\">> => Vfield3, <<\"field2\">> => Vfield2, <<\"unmatched\">> => Unmatched } end."
       },
      {
       naming_using_colon,
       "x8, value: b8, value2: l124, b8[4] => value3,
                            b17{b3 => value,x6,-b8 => count}",
       "fun (Binary) -> <<_Vfield1:8/bits, Vvalue:8/bits, Vvalue2:124/bits, Vvalue3:32/bits, Vfield2:17/bits, Unmatched/bytes>> = Binary, #{ <<\"field2\">> => Vfield2, <<\"value3\">> => Vvalue3, <<\"value2\">> => Vvalue2, <<\"value\">> => Vvalue, <<\"unmatched\">> => Unmatched } end."
       },
      {
       single_byte,
       "b8",
       "fun (Binary) -> <<Vfield1:8/bits, Unmatched/bytes>> = Binary, #{ <<\"field1\">> => Vfield1, <<\"unmatched\">> => Unmatched } end."
       }
    ],
    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_packet_type_leex:string(SrcString),
                         {ok, Result} =
                             erlang_red_packet_type_parser:parse(Tokens),
                         ?assertEqual(list_to_binary(Expec), Result)
                 end
     } || {TestCaseName, SrcString, Expec} <- Tests],

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
