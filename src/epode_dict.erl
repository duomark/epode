%%%------------------------------------------------------------------------------
%%% @copyright (c) 2014, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com> [http://duomark.com/]
%%% @reference 2014 Development sponsored by TigerText, Inc. [http://tigertext.com/]
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Definition of epode dictionaries.
%%% @since 0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(epode_dict).
-author('Jay Nelson <jay@duomark.com>').

%% External interface
-export([new/1, is_dict/2, size/1]).

-include("epode.hrl").

%%%------------------------------------------------------------------------------
%% Construct and validate dictionary types
%%%------------------------------------------------------------------------------
-spec new      (epode_dict_type())                        -> epode_dict_instance().
-spec is_dict  (epode_dict_type(), epode_dict_instance()) -> boolean().

new(dict)    -> dict:new();
new(orddict) -> orddict:new();
new(vbisect) -> vbisect:from_orddict([]).


%% OTP doesn't provide is_dict types, so this is a low-level hack for convenience.
%% TODO: submit patches to vbisect and OTP to make is_dict/1 a standard API call.
is_dict(dict,        Dict) when is_tuple (Dict) -> dict =:= element(1, Dict);
is_dict(dict, _Not_A_Dict)                      -> false;

%% We can't scan the whole dictionary, or verify that it is sorted.
is_dict(orddict,                [])             -> true;
is_dict(orddict, [{_K,_V} | _Rest])             -> true;
is_dict(orddict,       _Not_A_Dict)             -> false;

%% Vbisects have a fast internal validation function.
is_dict(vbisect,  Dict)                         -> vbisect:is_vbisect(Dict).


%% Internally determine what type a dictionary is because of differing APIs.
dict_type(Dict) ->
    %% This should be a cond statement if we had one!
    case is_dict(dict, Dict) of
        true  -> dict;
        false -> case is_dict(orddict, Dict) of
                     true  -> orddict;
                     false -> case is_dict(vbisect, Dict) of
                                  true  -> vbisect;
                                  false -> not_a_dict
                              end
                 end
    end.


%%%------------------------------------------------------------------------------
%% API to access dictionary meta-attributes
%%%------------------------------------------------------------------------------
-spec size(epode_dict_instance()) -> non_neg_integer() | not_a_dict.

size(Dict) -> size(dict_type(Dict), Dict).

size(dict,        Dict) -> dict    :size(Dict);
size(orddict,     Dict) -> orddict :size(Dict);
size(vbisect,     Dict) -> vbisect :size(Dict);
size(not_a_dict, _Dict) -> not_a_dict.


%%%------------------------------------------------------------------------------
%% API to access dictionary properties...
%%%------------------------------------------------------------------------------
