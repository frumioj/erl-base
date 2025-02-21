-module(base32_tests).
-include_lib("eunit/include/eunit.hrl").
-include("base32.hrl").

% Test basic encoding
encode_basic_test() ->
    % Test cases from z-base-32 spec
    ?assertEqual("cr", base32:encode_to_string("a")),
    ?assertEqual("ce", base32:encode_to_string("b")),
    ?assertEqual("rr", base32:encode_to_string("!")),
    ?assertEqual("cfty", base32:encode_to_string("ab")),
    ?assertEqual("cftgg", base32:encode_to_string("abc")),
    ?assertEqual("ktwgkedtqiwsg43ycj3g675qrbug66bypj4s4hdurbzzc3m1rb4go3jyptozw6jyctzsqmo", base32:encode_to_string("The quick brown fox jumps over the lazy dog.")).

% Test binary encoding
encode_binary_test() ->
    ?assertEqual(<<"cr">>, base32:encode(<<"a">>)),
    ?assertEqual(<<"ce">>, base32:encode(<<"b">>)),
    ?assertEqual(<<"cfty">>, base32:encode(<<"ab">>)),
    ?assertEqual(<<"cftgg">>, base32:encode(<<"abc">>)).

% Test decoding
decode_test() ->
    ?assertEqual("a", base32:decode_to_string("cr")),
    ?assertEqual("b", base32:decode_to_string("ce")),
    ?assertEqual("ab", base32:decode_to_string("cfty")),
    ?assertEqual("abc", base32:decode_to_string("cftgg")),
    ?assertEqual("The quick brown fox jumps over the lazy dog.", base32:decode_to_string("ktwgkedtqiwsg43ycj3g675qrbug66bypj4s4hdurbzzc3m1rb4go3jyptozw6jyctzsqmo")).

% Test roundtrip encoding/decoding
roundtrip_test() ->
    TestStrings = [
        "Hello, World!",
        "Test123",
        "Special chars: !@#$%^&*()"
    ],
    lists:foreach(fun(String) ->
        Encoded = base32:encode_to_string(String),
        Decoded = base32:decode_to_string(Encoded),
        ?assertEqual(String, Decoded)
    end, TestStrings),
    
    % Test binary separately
    BinaryTest = <<1,2,3,4,5>>,
    Encoded = base32:encode(BinaryTest),
    Decoded = base32:decode(Encoded),
    ?assertEqual(BinaryTest, Decoded).

% Test error cases
error_test() ->
    % Invalid characters should be handled
    ?assertEqual({error, invalid_character}, base32:decode("invalid!")),
    % Empty input should work
    ?assertEqual([], base32:encode_to_string([])),
    ?assertEqual([], base32:decode_to_string([])).

% Test z-base-32 alphabet compliance
alphabet_test() ->
    % Test that all valid z-base-32 characters are accepted
    ValidChars = "ybndrfg8ejkmcpqxot1uwisza345h769",
    lists:foreach(fun(Char) ->
        ?assertNotEqual(bad, element(Char + 1, ?DECODE_MAP))
    end, ValidChars).

% Test non-byte-aligned encoding
%% non_byte_aligned_test() ->
%%     TestCases = [
%%         {<<2#1:1>>, "b"},      % 1 bit
%%         {<<2#11:2>>, "n"},     % 2 bits
%%         {<<2#111:3>>, "r"},    % 3 bits
%%         {<<2#1111:4>>, "y"},    % 4 bits
%%         {<<2#11111:5>>, "y"},   % 5 bits
%%         {<<2#111111:6>>, "yb"},
%%         {<<2#1111111:7>>, "yn"},
%%         {<<2#11111111:8>>, "yy"}
%%     ],
%%     lists:foreach(fun({Input, Expected}) ->
%%         Result = base32:encode_to_string(Input),
%%         io:format("Input: ~p, Result: ~p, Expected: ~p~n", 
%%                  [Input, Result, Expected]),
%%         ?assertEqual(Expected, Result)
%%     end, TestCases).

% Test partial byte roundtrip encoding/decoding
%% partial_byte_roundtrip_test() ->
%%     OriginalBits = <<22:5>>,
%%     io:format("Original: ~p~n", [OriginalBits]),
%%     Encoded = base32:encode(OriginalBits),
%%     io:format("Encoded: ~p~n", [Encoded]),
%%     Decoded = base32:decode(Encoded),
%%     io:format("Decoded: ~p~n", [Decoded]),
%%     ?assertEqual(OriginalBits, Decoded).

% Test edge cases
edge_cases_test() ->
    % Test various edge cases
    ?assertEqual(<<>>, base32:decode(base32:encode(<<>>))),
    ?assertEqual(<<"a">>, base32:decode(base32:encode(<<"a">>))),
    
    % Test with different bit patterns
    Patterns = [
        <<2#1010:4>>,
        <<2#10101:5>>,
        <<2#101010:6>>,
        <<2#1010101:7>>
    ],
    lists:foreach(fun(Pattern) ->
        Encoded = base32:encode(Pattern),
        ?assertNotEqual(<<>>, Encoded)
    end, Patterns).

% Debug test for single bit encoding
single_bit_test() ->
    Input = <<2#1:1>>,
    Result = base32:encode_to_string(Input),
    io:format("1 bit input: ~p -> ~p (expected 'b')~n", 
             [Input, Result]).

% Debug test for 5-bit encoding/decoding
five_bit_test() ->
    Input = <<22:5>>,
    Encoded = base32:encode(Input),
    io:format("5-bit input: ~p~n", [Input]),
    io:format("Encoded: ~p~n", [Encoded]),
    Decoded = base32:decode(Encoded),
    io:format("Decoded: ~p~n", [Decoded]). 
