%%% @author  John Kemp <stable.pseudonym@gmail.com>
%%% @doc Implementation of z-base-32 in Erlang.
%%% @reference See <a href="http://zooko.com/repos/z-base-32/base32/DESIGN">Z-Base-32</a>. Find the code <a href="http://github.com/frumioj/erl-base">here</a>.
%%% @since 26 August 2009
%%%
%%% @copyright 2009 John Kemp, All rights reserved. Open source, BSD License
%%% @version 1.1
%%%

%%%
%%% Copyright (c) 2009 John Kemp
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 3. Neither the name of the copyright holder nor the names of contributors
%%%    may be used to endorse or promote products derived from this software
%%%    without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%

-module(base32).
-include("base32.hrl").  % rebar3 automatically adds include/ to the include path

%% API exports
-export([
    encode/1,
    encode_to_string/1,
    decode/1,
    decode_to_string/1
]).

%% Encoding functions
encode(Bin) when is_binary(Bin) ->
    encode_binary(Bin);
encode(List) when is_list(List) ->
    encode_binary(list_to_binary(List));
encode(BitString) when is_bitstring(BitString) ->
    encode_binary(BitString).

encode_to_string(Input) ->
    binary_to_list(encode(Input)).

%% Decoding functions
decode(Bin) when is_binary(Bin) ->
    try decode_binary(Bin)
    catch
        error:invalid_character -> {error, invalid_character}
    end;
decode(List) when is_list(List) ->
    try decode_binary(list_to_binary(List))
    catch
        error:invalid_character -> {error, invalid_character}
    end.

decode_to_string(Input) ->
    case decode(Input) of
        {error, Reason} -> {error, Reason};
        Bin when is_binary(Bin) -> binary_to_list(Bin)
    end.

%% Internal functions
encode_binary(<<>>) ->
    <<>>;
encode_binary(BitString) ->
    % Convert input bits into 5-bit chunks
    Bits = bit_size(BitString),
    case Bits of
        N when N =< 5 ->
            % For small bit strings (5 bits or less), left-pad with zeros
            <<Value:N, _/bitstring>> = BitString,
            % Shift left to match test expectations
            ShiftedValue = Value bsl (5-N),
            <<(b32e(ShiftedValue)):8>>;
        _ ->
            % For larger bit strings, process normally
            Padding = (5 - (Bits rem 5)) rem 5,
            PaddedBin = <<BitString/bitstring, 0:Padding>>,
            NumChunks = (Bits + Padding) div 5,
            encode_chunks(PaddedBin, NumChunks, <<>>)
    end.

encode_chunks(_Bin, 0, Acc) ->
    Acc;
encode_chunks(<<Chunk:5, Rest/bitstring>>, N, Acc) ->
    encode_chunks(Rest, N-1, <<Acc/binary, (b32e(Chunk)):8>>).

decode_binary(<<>>) ->
    <<>>;
decode_binary(Bin) ->
    decode_binary(Bin, <<>>).

decode_binary(<<>>, Acc) ->
    % Convert accumulated 5-bit values back to 8-bit bytes
    BitSize = bit_size(Acc),
    ByteSize = BitSize div 8,
    <<Result:ByteSize/binary, _/bitstring>> = Acc,
    Result;
decode_binary(<<Char:8, Rest/binary>>, Acc) ->
    % Ensure index is valid (0-255)
    Index = if 
        Char < 0 -> error(invalid_character);
        Char > 255 -> error(invalid_character);
        true -> Char + 1
    end,
    case element(Index, ?DECODE_MAP) of
        bad -> error(invalid_character);
        Value -> 
            NewAcc = <<Acc/bitstring, Value:5>>,
            decode_binary(Rest, NewAcc)
    end.

%% Helper functions
b32e(N) when N >= 0, N < 32 ->
    element(N + 1, ?ENCODE_MAP).