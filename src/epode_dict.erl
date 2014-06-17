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
-export([
         new/2,             % Construct a new empty epode_dict
         from_list/3,       % Construct a new epode_dict from an attribute list
         is_dict/1,         % Check if data is an epode_dict
         size/1,            % Return the size of an epode_dict
         map/3,             % Convert attribute values only
         xlate/3            % Convert attributes and values
        ]).

-include("epode.hrl").


%%%------------------------------------------------------------------------------
%% Construct and validate dictionary types
%%%------------------------------------------------------------------------------

-type dict_list(Key_Type, Val_Type) :: [{Key_Type, Val_Type}].
-type binary_dict_list() :: dict_list(binary(), binary()).
-type atom_dict_list()   :: dict_list(atom(),   any()).
-type any_dict_list()    :: dict_list(any(),    any()).

-spec new       (epode_bdict_type(), epode_keyval_binary_type() ) -> epode_bin_dict();
                (epode_edict_type(), epode_keyval_atom_type()   ) -> epode_atom_dict();
                (epode_edict_type(), epode_keyval_any_type()    ) -> epode_any_dict().

-spec from_list (epode_bdict_type(),     epode_keyval_binary_type(), binary_dict_list() ) -> epode_bin_dict();
                (epode_atom_dict_type(), epode_keyval_atom_type(),   atom_dict_list()   ) -> epode_atom_dict();
                (epode_any_dict_type(),  epode_keyval_any_type(),    any_dict_list()    ) -> epode_any_dict().

-spec is_dict   (any()) -> boolean().


%% vbisect is the only dictionary that is restricted with key value types.
new(dict, Keyval_Type)    -> {dict,    Keyval_Type, dict   :new()};
new(orddict, Keyval_Type) -> {orddict, Keyval_Type, orddict:new()};
new(vbisect, pure_binary) -> {vbisect, pure_binary, vbisect:from_orddict([])}.

%% Construct a dictionary from a list of Attribute / Value pairs.
from_list(dict,    Keyval_Type, Attrs) -> {dict,    Keyval_Type, dict   :from_list(Attrs)};
from_list(orddict, Keyval_Type, Attrs) -> {orddict, Keyval_Type, orddict:from_list(Attrs)};
from_list(vbisect, pure_binary, Attrs) -> {vbisect, pure_binary, vbisect:from_list(Attrs)}.


%% Test if a dict is properly constructed.
is_dict(Dict) ->
    case dict_type(Dict) of
        not_a_dict -> false;
        _A_Dict    -> true
    end.


%% Internally determine what type a dictionary is because of differing APIs.
dict_type({dict,    _Any_Keyval_Type, Bare_Dict}) -> valid_dict(dict,    Bare_Dict);
dict_type({orddict, _Any_Keyval_Type, Bare_Dict}) -> valid_dict(orddict, Bare_Dict);
dict_type({vbisect, pure_binary,      Bare_Dict}) -> valid_dict(vbisect, Bare_Dict);
dict_type(_Incorrect_Format)                      -> not_a_dict.

valid_dict(Dict_Type, Bare_Dict) ->
    case is_dict(Dict_Type, Bare_Dict) of
        true  -> Dict_Type;
        false -> not_a_dict
    end.


%% OTP doesn't provide is_dict types, so this is a low-level hack for convenience.
%% TODO: submit patches to vbisect and OTP to make is_dict/1 a standard API call.
is_dict(dict,   Bare_Dict)
  when element(1, Bare_Dict) =:= dict  -> true;    % close enough, doesn't check tuple size
is_dict(dict,             _Not_A_Dict) -> false;

%% We can't scan the whole dictionary, or verify that it is sorted.
is_dict(orddict,                   []) -> true;
is_dict(orddict,    [{_K,_V} | _Rest]) -> true;
is_dict(orddict,          _Not_A_Dict) -> false;

%% Vbisects have a fast internal validation function.
is_dict(vbisect,            Bare_Dict) -> vbisect:is_vbisect(Bare_Dict).


%%%------------------------------------------------------------------------------
%% API to access dictionary meta-attributes
%%%------------------------------------------------------------------------------

-spec size(any()) -> non_neg_integer() | not_a_dict.

size({_Dict_Type, _Keyval_Type, Bare_Dict} = Dict) -> size(dict_type(Dict), Bare_Dict);
size(                                           _) -> not_a_dict.


size(dict,        Bare_Dict) -> dict   :size(Bare_Dict);
size(orddict,     Bare_Dict) -> orddict:size(Bare_Dict);
size(vbisect,     Bare_Dict) -> vbisect:size(Bare_Dict);
size(not_a_dict,  _Bad_Dict) -> not_a_dict.
    

%%%------------------------------------------------------------------------------
%% API for converting attributes and values
%%%------------------------------------------------------------------------------

%% Mapping has the potential to change all values.
-type map_fn()   :: fun((Key1::epode_attr(), Value1::epode_value())
                        -> Value2::epode_value()).

%% Translation has the potential to change all keys and values.
-type xlate_fn() :: fun((Key1::epode_attr(), Value1::epode_value())
                        -> {Key2::epode_attr(), Value2::epode_value()} | undefined).

-spec map       (map_fn(),   epode_dict(), epode_all_dict_type()) -> epode_dict().
-spec xlate     (xlate_fn(), epode_dict(), epode_all_dict_type()) -> epode_dict().

%% Convert just the values (keeping the old attributes) to generate a new dictionary.
map(Fun, {_Dict_Type, _Keyval_Type, Bare_Dict} = _Dict, New_Keyval_Type) ->
    case dict_type(Bare_Dict) of
        dict    -> {dict,    New_Keyval_Type, dict   :map(Fun, Bare_Dict)};
        orddict -> {orddict, New_Keyval_Type, orddict:map(Fun, Bare_Dict)};
        vbisect -> case New_Keyval_Type of
                       pure_binary -> {vbisect, pure_binary, vbisect:map(Fun, Bare_Dict)}
                   end
    end.


%% Convert both attributes and values to generate a new dictionary.
-define(MAP_XLATE(__Dict_Type, __New_Keyval_Type, __Fun, __Bare_Dict),
        __Module = __Dict_Type,
        {__Module, __New_Keyval_Type,
         __Module:from_list( xlate_attrs(__Fun, __New_Keyval_Type, __Module:to_list(__Bare_Dict)) )}).

xlate(Fun, {_Dict_Type, _Keyval_Type, Bare_Dict} = Dict, New_Keyval_Type) ->
    ?MAP_XLATE(dict_type(Dict), New_Keyval_Type, Fun, Bare_Dict).

xlate_attrs(Fun, New_Keyval_Type, Keyval_Pairs) ->
    lists:foldl(fun({Attr, Value}, New_Pair_List) ->
                        case {New_Keyval_Type, Fun(Attr, Value)} of

                            %% Both Attribute and Value must be binary...
                            {pure_binary, {_New_Attr, _New_Value} = New_Pair}
                              when is_binary(_New_Attr), is_binary(_New_Value) -> 
                                [New_Pair | New_Pair_List];

                            %% Attribute must be atom, Value can be anything...
                            {atom_attrs, {_New_Attr, _New_Value} = New_Pair}
                              when is_atom(_New_Attr) ->
                                [New_Pair | New_Pair_List];

                            %% Both Attribute and Value can be anything...
                            {any, {_New_Attr, _New_Value} = New_Pair} ->
                                [New_Pair | New_Pair_List];

                            %% Attribute is dropped from new dictionary results.
                            {New_Keyval_Type, undefined} ->
                                New_Pair_List
                        end
                end, [], Keyval_Pairs).


%%%------------------------------------------------------------------------------
%% API to access dictionary properties...
%%%------------------------------------------------------------------------------
