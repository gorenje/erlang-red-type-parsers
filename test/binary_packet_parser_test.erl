%%% % @noformat

-module(binary_packet_parser_test).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%%
%% Parse a PNG Image
%%
collect_png_chunks(<<>>, Acc, _ChunkFunc) ->
    lists:reverse(Acc);
collect_png_chunks(Data, Acc, ChunkFunc) ->
    {ok, Chunk, _Matched, Rest} = ChunkFunc(Data),
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
    {ok, #{}, _MatchedData, RestData} = HeaderFunc(PngData),

    % retrieve all chunks defined in the png
    Chunks = collect_png_chunks(RestData, [], ChunkFunc),
    Types = [T || #{ <<"type">> := T } <- Chunks ],

    ?assertEqual(["IHDR","iCCP","eXIf","iTXt","IDAT","IEND"], Types).

%% -----------------------------------------------------------------------------
%%
%% Parse a JPEG Image
%%
%% Specification taken from kaitai project:
%%     https://github.com/kaitai-io/kaitai_struct_formats/blob/34aa005a07c1fdf47ac385cca91ea590c4ec2ea1/image/jpeg.ksy

-define(SOS, 218). %% list_to_integer("DA",16)).
-define(SOI, 216). %% list_to_integer("D8",16)).
-define(EOI, 217). %% list_to_integer("D9",16)).
-define(APP0, 224). %% list_to_integer("E0",16)).
-define(APP1, 225). %% list_to_integer("E1",16)).
-define(SOF0, 192). %% list_to_integer("C0",16)).

split_on_zero(B) when is_binary(B) ->
    split_on_zero(B,0).
split_on_zero(B, N) when is_binary(B), is_integer(N) ->
    case B of
        <<B1:N/binary,0,B2/binary>> ->
            {B1,B2};
        _ when size(B) > N ->
            split_on_zero(B, N+1);
        _ -> B
    end.

sos_components({ok, Comp, _Matched, Rest}, _Func, 1 = Cnt) ->
    io:format("  ~b. Compponent: ~p~n", [Cnt,Comp]),
    Rest;
sos_components({ok, Comp, _Matched, Rest}, Func, Cnt) when Cnt > 1 ->
    io:format("  ~b. Compponent: ~p~n", [Cnt,Comp]),
    sos_components(Func(Rest),Func,Cnt-1).

jpg_check_segment_type(
  {ok, #{<<"type">> := Type}, _Matched, <<>>},
  _TypeFunc,
  _DataFunc
) ->
    io:format("Found Type: ~p - Data Ran Out~n", [Type]);

jpg_check_segment_type(
  {ok, #{<<"type">> := ?SOI}, _Matched, Rest},
  TypeFunc,
  DataFunc
) ->
    io:format("Found Type: 0x~s No Data~n", [integer_to_list(?SOI,16)]),
    jpg_check_segment_type(TypeFunc(Rest), TypeFunc, DataFunc);

jpg_check_segment_type(
  {ok, #{<<"type">> := ?EOI}, _Matched, Rest},
  TypeFunc,
  DataFunc
) ->
    io:format("Found Type: 0x~s No Data~n", [integer_to_list(?EOI,16)]),
    jpg_check_segment_type(TypeFunc(Rest), TypeFunc, DataFunc);

jpg_check_segment_type(
  {ok, #{<<"type">> := ?SOF0}, _Matched, Rest},
  TypeFunc,
  DataFunc
) ->
    { ok, #{<<"data">> := Data}, _Ignore, Rest2} = DataFunc(Rest),

    Sof0Format = "
      b8 => bits_per_sample,
      b16 => image_height,
      b16 => image_width,
      b8 => num_components
    ",

    {ok, Sof0FormatFunc} =
        erl_packetparser:erlang_func_for_packetdef(Sof0Format),
    {ok, Details, _, CompontentData} = Sof0FormatFunc(Data),

    io:format("Found Type: 0x~s Data: ~p D: ~p~n",
              [integer_to_list(?SOF0,16), Details, CompontentData]),

    jpg_check_segment_type(TypeFunc(Rest2), TypeFunc, DataFunc);


jpg_check_segment_type(
  {ok, #{<<"type">> := ?SOS}, _Matched, Rest},
  _TypeFunc,
  DataFunc
) ->
    { ok, #{<<"data">> := Data}, _Ignore, Rest2} = DataFunc(Rest),

    io:format("Found Type: 0x~s Data: ~p~n",
              [integer_to_list(?SOS,16), Data]),

    SosSegmentSpec = "
      b8 => num_components
    ",
    ComponentSpec = "
      b8 => type,
      b8 => value
    ",
    SpectralSpec = "
      b8 => start,
      b8 => end,
      b8 => approx
    ",

    {ok, SosSegFunc} =
        erl_packetparser:erlang_func_for_packetdef(SosSegmentSpec),
    {ok, ComponentFunc} =
        erl_packetparser:erlang_func_for_packetdef(ComponentSpec),
    {ok, SpectralSpecFunc} =
        erl_packetparser:erlang_func_for_packetdef(SpectralSpec),

    {ok, #{<<"num_components">> := Cnt}, _Ign3, Rest3} = SosSegFunc(Data),

    Rest4 = sos_components(ComponentFunc(Rest3), ComponentFunc, Cnt),
    io:format("    Rest ~p~n", [Rest4]),

    {ok, H, _, <<>>} = SpectralSpecFunc(Rest4),

    io:format("    Spectral ~p~n", [H]),

    io:format("    ImageData~n  ~p~n", [byte_size(Rest2)]),
    byte_size(Rest2);

jpg_check_segment_type(
  {ok, #{<<"type">> := ?APP0}, _Matched, Rest},
  TypeFunc,
  DataFunc
) ->
    { ok, #{<<"data">> := Data}, _Ignore, Rest2} = DataFunc(Rest),
    FormatApp0 = "
       b8[5] => magic,
       b8    => major_version,
       b8    => minor_version,
       b8    => density_units,
       b16   => density_x,
       b16   => density_y,
       b8    => thumbnail_x,
       b8    => thumbnail_y,
       b8[$thumbnail_x * $thumbnail_y * 3] => thumbnail_data
    ",
    {ok, App0Func} = erl_packetparser:erlang_func_for_packetdef(FormatApp0),
    {ok, App0Details, _, _} = App0Func(Data),

    io:format("Found Type: 0x~s Details: ~p~n",
              [integer_to_list(?APP0,16), App0Details]),
    jpg_check_segment_type(TypeFunc(Rest2), TypeFunc, DataFunc);

jpg_check_segment_type(
  {ok, #{<<"type">> := ?APP1}, _Matched, Rest},
  TypeFunc,
  DataFunc
) ->
    {ok, #{<<"data">> := Data}, _, Rest2} = DataFunc(Rest),

    {Magic, Rest3} = split_on_zero(Data),

    case Magic of
        <<"Exif">> ->
            FormatExif = "
              x8 => 0,
              l16 => endianness,
              b16 => version,
              b32{ b16 => num_of_fields, b16 => other_value }
            ",
            FormatExifField = "
              b16 => tag,
              b16 => field_type,
              b32 => length,
              b32 => data_or_ofs
            ",
            {ok, ExifHeaderFunc} =
                erl_packetparser:erlang_func_for_packetdef(FormatExif),
            {ok, ExifFieldFunc} =
                erl_packetparser:erlang_func_for_packetdef(FormatExifField),

            {ok, ExifHeaderDetails, _, Rest4} = ExifHeaderFunc(Rest3),
            {ok, FieldDetails, _, _} = ExifFieldFunc(Rest4),

            io:format("Exif Header : ~p~n  Field ~n  ~p~n  Rest ~n  ~p~n",
                      [ExifHeaderDetails, FieldDetails, Rest4]),
            ok;
        _ ->
            io:format( "Unknown Magic in APP1 : ~p Rest:~n ~p~n",
                       [Magic, Rest3])
    end,

    jpg_check_segment_type(TypeFunc(Rest2), TypeFunc, DataFunc);

jpg_check_segment_type(
  {ok, #{<<"type">> := Type}, _Matched, Rest},
  TypeFunc,
  DataFunc
) ->
    { ok, #{<<"data">> := Data}, _Ignore, Rest2} = DataFunc(Rest),
    io:format("Found Type: 0x~s Data: ~p~n", [integer_to_list(Type,16), Data]),
    jpg_check_segment_type(TypeFunc(Rest2), TypeFunc, DataFunc).


check_operation_by_parsing_jpg_test() ->
    {ok, JpgData} =
        file:read_file(code:priv_dir(erlang_red_parsers) ++ "/test.jpg"),

    SegmentType = "
       x8 => 0xff,
       b8 => type
    ",
    TypeWithLength = "
       b16 => length,
       b8[$length - 2] => data
    ",

    {ok, SegTypeFunc} = erl_packetparser:erlang_func_for_packetdef(SegmentType),
    {ok, DataFunc} = erl_packetparser:erlang_func_for_packetdef(TypeWithLength),

    ?assertEqual(3442,
                 jpg_check_segment_type(SegTypeFunc(JpgData),
                                        SegTypeFunc, DataFunc)).


%% -----------------------------------------------------------------------------
%% General Feature Testing
%%

foreach_packet_parser_end_to_end_test_() ->
    Tests = [
      %% {
      %%  zero_terminated_byte_string_not_supported,
      %%  "b8z => name",
      %%  <<69,120,105,102,0,0,77,77,0,42>>,
      %%  { #{ <<"name">> => <<69,120,105,102>>}, <<0,77,77,0,42>> }
      %% },
      {
       single_byte_no_label,
       "b8",
       <<123>>,
       { ok, #{}, <<123>>, <<>> }
      },
      {
       single_byte_big_endian,
       "b8 => f1",
       <<223>>,
       { ok, #{ <<"f1">> => 223 }, <<223>>, <<>> }
      },
      {
       single_byte_big_endian_signed,
       "-b8 => f1",
       <<223>>,
       { ok, #{ <<"f1">> => -33 }, <<223>>, <<>> }
      },
      {
       single_byte_little_endian,
       "l8 => f1",
       <<223>>,
       { ok, #{ <<"f1">> => 223 }, <<223>>, <<>> }
      },
      {
       single_byte_little_endian_and_signed,
       "-l8 => f1",
       <<223>>,
       { ok, #{ <<"f1">> => -33 }, <<223>>, <<>> }
      },
      {
       single_byte,
       "x1 => 1, b7 => len",
       <<130>>,
       { ok, #{ <<"len">> => 2 }, <<130>>, <<>> }
      },
      {
       single_byte_off_boundary,
       "x1 => 1,
        b8 => len,
        x7",
       <<130, 123>>,
       { ok, #{ <<"len">> => 4 }, <<130, 123>>, <<>> }
      },
      {
       using_bit_boundary,
       "x1 => 1,
        b8 => len",
       <<130, 123>>,
       { ok, #{ <<"len">> => 4 }, <<130, 0:1>>, <<123:7>> }
      },
      {
       single_byte_off_boundary_with_structure,
       "x1 => 1,
        b8{ b4 => len1, b4 => len2 },
        x7",
       <<130, 123>>,
       { ok, #{ <<"len1">> => 0, <<"len2">> => 4 }, <<130, 123>>, <<>> }
      },
      {
       arrays_of_seven_bits,
       "b7[4] => values",
       <<130, 123, 164, 193>>,
       { ok,
         #{ <<"values">> => [65,30,116,76] },
         <<130, 123, 164, 76:4>>,
         <<1:4>>
       }
      },
      {
       arrays_of_seven_bits_ignore_last_four_bits,
       "b7[4] => values, x4",
       <<130, 123, 164, 193>>,
       { ok, #{ <<"values">> => [65,30,116,76] }, <<130, 123, 164, 193>>, <<>> }
      },
      {
       structures_can_have_arrays,
       "b32{ b7[4] => values, x4 }",
       <<130, 123, 164, 193>>,
       { ok, #{ <<"values">> => [65,30,116,76] }, <<130, 123, 164, 193>>, <<>> }
      },
      {
       structures_can_have_arrays_reverse_naming,
       "b32{ values: b7[4], x4 }",
       <<130, 123, 164, 193>>,
       { ok, #{ <<"values">> => [65,30,116,76] }, <<130, 123, 164, 193>>, <<>> }
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
       { ok, #{ <<"f1">> => 221, <<"f2">> => 59259, <<"f3">> => 14,
            <<"f4">> => 7, <<"last">> => 231, <<"value">> => 234,
            <<"value2">> => 212, <<"value3">> => 31957 },
         <<255,234,212,213,124,221,123,231,231,231>>,
         <<>> }
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
       { ok, #{ <<"f1">> => -35, <<"f2">> => -6277, <<"f3">> => 14,
            <<"f4">> => 7, <<"last">> => 231, <<"value">> => 234,
            <<"value2">> => 212, <<"value3">> => 54652 },
         <<255,234,212,213,124,221,123,231,231,231>>,
         <<>> }
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
       support_simple_arithemtic_operations,
       "b16 => length,
        b8[$length - 2] => data
       ",
       "fun (Binary) ->
            <<Vlength:16/integer-big-unsigned, Vdata:(8*(Vlength - 2))/bits,
                   UnmatchedBits/bits>> = Binary,
            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                          UnmatchedBits/bits>> = Binary,
            {ok, #{ <<\"length\">> => Vlength, <<\"data\">> => Vdata },
                     MatchedBits, UnmatchedBits}
        end."
      },
      {
       support_basic_arithemtic_operations,
       "b16 => width,
        b16 => height,
        b8[$width * $height * 3] => data
       ",
       "fun (Binary) ->
          <<Vwidth:16/integer-big-unsigned, Vheight:16/integer-big-unsigned,
               Vdata:(8*(Vwidth * Vheight * 3))/bits,
          UnmatchedBits/bits>> = Binary,

          <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                               UnmatchedBits/bits>> = Binary,

          {ok, #{ <<\"width\">> => Vwidth, <<\"height\">> => Vheight,
                     <<\"data\">> => Vdata }, MatchedBits, UnmatchedBits}
        end."
      },
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
                 UnmatchedBits/bits>> = Binary,

             <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                         UnmatchedBits/bits>> = Binary,

             {ok, #{ <<\"length\">> => Vlength,
                 <<\"type\">> => [ X || <<X:8/integer-big-unsigned>> <= Vtype],
                 <<\"data\">> => Vdata, <<\"crc\">> => Vcrc },
                  MatchedBits, UnmatchedBits}
        end."
      },
      {
        trailing_commas_are_ok,
        "x1,",
        "fun (Binary) ->
            <<_VinternalField1:1/bits, UnmatchedBits/bits>> = Binary,

            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                  UnmatchedBits/bits>> = Binary,

            {ok, #{ }, MatchedBits, UnmatchedBits}
         end."
      },
      {
        trailing_commas_are_ok_with_label,
        "x1 => 0x23,",
        "fun (Binary) ->
              <<35:1, UnmatchedBits/bits>> = Binary,

              <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                  UnmatchedBits/bits>> = Binary,
              {ok, #{ }, MatchedBits, UnmatchedBits}
         end."
      },
      {
       word_on_word_boundary_with_structure,
       "x1 => 1,
        b8{ b4 => len1, b4 => len2 },
        x7",
       "fun (Binary) ->
             <<1:1, VinternalField1:8/bits, _VinternalField2:7/bits,
                 UnmatchedBits/bits>> = Binary,

             <<Vlen1:4/integer-big-unsigned,
                   Vlen2:4/integer-big-unsigned>> = VinternalField1,

             <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                  UnmatchedBits/bits>> = Binary,

             {ok, #{ <<\"len2\">> => Vlen2, <<\"len1\">> => Vlen1 },
                  MatchedBits, UnmatchedBits}
        end."
      },
      {
       word_on_word_boundary,
       "x1 => 1,
        b8 => len,
        x7",
       "fun (Binary) ->
           <<1:1, Vlen:8/integer-big-unsigned, _VinternalField1:7/bits,
                UnmatchedBits/bits>> = Binary,

           <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                     UnmatchedBits/bits>> = Binary,

           {ok, #{ <<\"len\">> => Vlen }, MatchedBits, UnmatchedBits}
        end."
      },
      {
       match_on_one_bit,
       "x1 => 1,
        b7 => len",
       "fun (Binary) ->
            <<1:1, Vlen:7/integer-big-unsigned, UnmatchedBits/bits>> = Binary,
            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                  UnmatchedBits/bits>> = Binary,
            {ok, #{ <<\"len\">> => Vlen }, MatchedBits, UnmatchedBits}
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
                  UnmatchedBits/bits>> = Binary,

            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                   UnmatchedBits/bits>> = Binary,

            {ok, #{ <<\"len\">> => Vlen }, MatchedBits, UnmatchedBits}
        end."
      },
      {
       hashmap_definition_ensure_names_can_be_specs,
       "x8    => 12,
        b8    => b82,
        l24   => l123,
        b8[4] => b89,
        b8    => b8f,
        b16f  => volt,
        b16   => temp,
        b8    => hum,
        b8    => crc,
        x8",
       "fun (Binary) ->
            <<12:8, Vb82:8/integer-big-unsigned,
              Vl123:24/integer-little-unsigned,
              Vb89:32/bits, Vb8f:8/integer-big-unsigned,
              Vvolt:16/float-big-unsigned, Vtemp:16/integer-big-unsigned,
              Vhum:8/integer-big-unsigned, Vcrc:8/integer-big-unsigned,
              _VinternalField1:8/bits, UnmatchedBits/bits>> = Binary,

           <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                    UnmatchedBits/bits>> = Binary,

           {ok, #{ <<\"b82\">> => Vb82, <<\"l123\">> => Vl123,
                   <<\"b89\">> => [ X || <<X:8/integer-big-unsigned>> <= Vb89],
                   <<\"b8f\">> => Vb8f, <<\"volt\">> => Vvolt,
                   <<\"temp\">> => Vtemp, <<\"hum\">> => Vhum,
                   <<\"crc\">> => Vcrc }, MatchedBits, UnmatchedBits}
        end."
      },
      {
       hashmap_definition,
       "x8,
        b8    => len,
        l24   => id,
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
               Vstatus:8/integer-big-unsigned, Vvolt:16/float-big-unsigned,
               Vtemp:16/integer-big-unsigned, Vhum:8/integer-big-unsigned,
               Vcrc:8/integer-big-unsigned, _VinternalField2:8/bits,
               UnmatchedBits/bits>> = Binary,

            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
               UnmatchedBits/bits>> = Binary,

            {ok, #{ <<\"len\">> => Vlen, <<\"id\">> => Vid,
                    <<\"tag\">> => [ X || <<X:8/integer-big-unsigned>> <= Vtag],
                    <<\"status\">> => Vstatus, <<\"volt\">> => Vvolt,
                    <<\"temp\">> => Vtemp, <<\"hum\">> => Vhum,
                    <<\"crc\">> => Vcrc }, MatchedBits, UnmatchedBits}
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
               UnmatchedBits/bits>> = Binary,

             <<VinternalField5:3/integer-big-unsigned, _VinternalField6:6/bits,
               VinternalField7:8/integer-big-signed>> = VinternalField5,

             <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                 UnmatchedBits/bits>> = Binary,

             {ok, #{ }, MatchedBits, UnmatchedBits}
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
              UnmatchedBits/bits>> = Binary,

            <<Vvalue:3/integer-big-unsigned, _VinternalField5:6/bits,
              Vcount:8/integer-big-signed>> = VinternalField5,

            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                UnmatchedBits/bits>> = Binary,

            {ok, #{ <<\"count\">> => Vcount, <<\"value\">> => Vvalue },
                    MatchedBits, UnmatchedBits}
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
              VinternalField2:17/bits, UnmatchedBits/bits>> = Binary,

            <<Vvalue:3/integer-big-unsigned, _VinternalField2:6/bits,
              Vcount:8/integer-big-signed>> = VinternalField2,

            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
               UnmatchedBits/bits>> = Binary,

            {ok, #{ <<\"count\">> => Vcount, <<\"value\">> => Vvalue,
                    <<\"value3\">> => [ X || <<X:8/integer-big-unsigned>> <= Vvalue3],
                    <<\"value2\">> => Vvalue2, <<\"value\">> => Vvalue },
              MatchedBits, UnmatchedBits}
        end."
       },
      {
       single_byte_unlabeled_byte,
       "b8",
       "fun (Binary) ->
            <<VinternalField1:8/integer-big-unsigned,
              UnmatchedBits/bits>> = Binary,
            <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
               UnmatchedBits/bits>> = Binary,
            {ok, #{ }, MatchedBits, UnmatchedBits}
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
                  UnmatchedBits/bits>> = Binary,

              <<Vv1_1:1/integer-big-unsigned, Vv1_2:2/integer-big-unsigned,
                Vv1_3:5/integer-big-unsigned>> = VinternalField1,

              <<Vv2_1:4/integer-big-unsigned, Vv2_2:8/integer-big-unsigned,
                 Vv3_3:4/integer-big-unsigned>> = VinternalField2,

              <<Vv3_1:1/integer-big-unsigned, _VinternalField3:4/bits,
                 Vv3_2:3/integer-big-unsigned>> = VinternalField3,

              <<MatchedBits:(bit_size(Binary) - bit_size(UnmatchedBits))/bits,
                 UnmatchedBits/bits>> = Binary,

              {ok, #{ <<\"v3_2\">> => Vv3_2, <<\"v3_1\">> => Vv3_1,
                      <<\"value\">> => Vvalue, <<\"v1_3\">> => Vv1_3,
                      <<\"v1_2\">> => Vv1_2, <<\"v1_1\">> => Vv1_1,
                      <<\"v2_1\">> => Vv2_1, <<\"v2_2\">> => Vv2_2,
                      <<\"v3_3\">> => Vv3_3 }, MatchedBits, UnmatchedBits}
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
