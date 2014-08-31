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

%% External interface for epode_dict construction and identity
-export([
         new/2,             % Construct a new empty epode_dict
         from_list/3,       % Construct a new epode_dict from an attribute list
         to_list/1,         % Create a list of {Key, Val} from an epode_dict
         is_dict/1          % Check if data is an epode_dict
        ]).

%% External interface for epode_dict attributes
-export([
         size/1,            % Return the size of an epode_dict
         fetch/2,           % Fetch a single key's value from epode_dict or crash
         fetch_keys/1,      % Fetch all the keys from an epode_dict (in same order as values/1)
         find/2,            % Fetch a single key's value from epode_dict or return error
         is_key/2,          % Test whether key exists in an epode_dict
         values/1           % Fetch all the values from an epode_dict (in same order as fetch_keys/1)
        ]).

%% External interface for epode_dict mapping, translation and filtering
-export([
         map/3,             % Convert values only to make a new epode_dict
         fold/3,            % Fold across keys and values to generate an arbitrary result
         xlate/4,           % Convert keys and values to make a new epode_dict
         filter/2           % Keep a subset of the keys and values in a new epode_dict
        ]).

-include("../include/epode.hrl").

-define(DICT_DISPATCH(__Fun_Name, __Dict),
        case dict_type(__Dict) of
            dict       -> dict    :__Fun_Name(bare_dict(Dict));
            orddict    -> orddict :__Fun_Name(bare_dict(Dict));
            vbisect    -> vbisect :__Fun_Name(bare_dict(Dict));
            not_a_dict -> not_a_dict
        end).

-define(DICT_DISPATCH(__Fun_Name, __Key, __Dict),
        case dict_type(__Dict) of
            dict       -> dict    :__Fun_Name(__Key, bare_dict(__Dict));
            orddict    -> orddict :__Fun_Name(__Key, bare_dict(__Dict));
            vbisect    -> vbisect :__Fun_Name(__Key, bare_dict(__Dict));
            not_a_dict -> not_a_dict
        end).

%%%------------------------------------------------------------------------------
%% Construct and validate dictionary types
%%%------------------------------------------------------------------------------

-type dict_list(Key_Type, Val_Type) :: [{Key_Type, Val_Type}].
-type binary_dict_list() :: dict_list(binary(), binary()).
-type atom_dict_list()   :: dict_list(atom(),   any()).
-type any_dict_list()    :: dict_list(any(),    any()).
-type dict_type_error()  :: {error, {invalid_types, {any(), any()}}}.

-spec new       (epode_bdict_type(), epode_keyval_binary_type() ) -> epode_bin_dict()  | dict_type_error();
                (epode_edict_type(), epode_keyval_atom_type()   ) -> epode_atom_dict() | dict_type_error();
                (epode_edict_type(), epode_keyval_any_type()    ) -> epode_any_dict()  | dict_type_error().

-spec from_list (epode_bdict_type(),     epode_keyval_binary_type(), binary_dict_list() ) -> epode_bin_dict()  | dict_type_error();
                (epode_atom_dict_type(), epode_keyval_atom_type(),   atom_dict_list()   ) -> epode_atom_dict() | dict_type_error();
                (epode_any_dict_type(),  epode_keyval_any_type(),    any_dict_list()    ) -> epode_any_dict()  | dict_type_error().

-spec to_list   (epode_bin_dict())  -> binary_dict_list();
                %% If overloaded types allowed: (epode_atom_dict()) -> atom_dict_list();
                (epode_any_dict())  -> any_dict_list().

-spec is_dict   (any()) -> boolean().

-define(IS_VALID_KEYVAL(__Type), __Type =:= pure_binary; __Type =:= atom_attrs; __Type =:= any).

%% vbisect is the only dictionary that is restricted with key value types.
new(dict,    Keyval_Type) when ?IS_VALID_KEYVAL(Keyval_Type) -> {dict,    Keyval_Type, dict    :new()};
new(orddict, Keyval_Type) when ?IS_VALID_KEYVAL(Keyval_Type) -> {orddict, Keyval_Type, orddict :new()};
new(vbisect, pure_binary)                                    -> {vbisect, pure_binary, vbisect :from_orddict([])};
new(Dict,    Keyval_Type)                                    -> {error, {invalid_types, {Dict, Keyval_Type}}}.

%% Construct a dictionary from a list of Attribute / Value pairs.
%% The source list is treated as a proplist so that the caller can push
%% contexts onto the front of the attribute list to efficiently provide
%% overrides for already specified attributes.
from_list(dict,    Keyval_Type,  Attrs) when ?IS_VALID_KEYVAL(Keyval_Type) -> {dict,    Keyval_Type, make_dict    (Attrs)};
from_list(orddict, Keyval_Type,  Attrs) when ?IS_VALID_KEYVAL(Keyval_Type) -> {orddict, Keyval_Type, make_orddict (Attrs)};
from_list(vbisect, pure_binary,  Attrs)                                    -> {vbisect, pure_binary, vbisect:from_orddict (make_orddict(Attrs))};
from_list(Dict,    Keyval_Type, _Attrs)                                    -> {error, {invalid_types, {Dict, Keyval_Type}}}.

to_list(Dict) -> ?DICT_DISPATCH(to_list, Dict).

%% These functions enforce proplist style shadowing of values rather
%% than using the built-in from_list where last clobbers earlier values.
-define(MAKE_FOLD(__Otp_Dict_Type, __Attrs),
        lists:foldl(fun({Key, Val}, Acc_Dict) ->
                            case __Otp_Dict_Type:is_key(Key, Acc_Dict) of
                                false -> __Otp_Dict_Type:store(Key, Val, Acc_Dict);
                                true  -> Acc_Dict
                            end
                    end, __Otp_Dict_Type:new(), __Attrs)).

make_dict    (Attrs) -> ?MAKE_FOLD(dict,    Attrs).
make_orddict (Attrs) -> ?MAKE_FOLD(orddict, Attrs).

%% Test if a dict is properly constructed.
is_dict(Dict) ->
    case dict_type(Dict) of
        not_a_dict -> false;
        _A_Dict    -> true
    end.


%% Internally determine what type a dictionary is because of differing APIs.
dict_type({dict,    Keyval_Type, Bare_Dict}) when ?IS_VALID_KEYVAL(Keyval_Type) -> valid_dict(dict,    Bare_Dict);
dict_type({orddict, Keyval_Type, Bare_Dict}) when ?IS_VALID_KEYVAL(Keyval_Type) -> valid_dict(orddict, Bare_Dict);
dict_type({vbisect, pure_binary, Bare_Dict})                                    -> valid_dict(vbisect, Bare_Dict);
dict_type(_Incorrect_Format)                                                    -> not_a_dict.

valid_dict(Dict_Type, Bare_Dict) ->
    case is_dict(Dict_Type, Bare_Dict) of
        true  -> Dict_Type;
        false -> not_a_dict
    end.

%% These functions are used internally, after verifying dict_type/2 is valid.
keyval_type ({_Dict_Type,  Keyval_Type, _Bare_Dict}) -> Keyval_Type.
bare_dict   ({_Dict_Type, _Keyval_Type,  Bare_Dict}) -> Bare_Dict.


%% OTP doesn't provide is_dict types, so this is a low-level hack for convenience.
%% TODO: submit patches to vbisect and OTP to make is_dict/1 a standard API call.
is_dict(dict,               Bare_Dict)
  when is_tuple(Bare_Dict),
       element(1, Bare_Dict) =:= dict  -> true;    % close enough, doesn't check tuple size
is_dict(dict,             _Not_A_Dict) -> false;

%% We can't scan the whole dictionary, or verify that it is sorted.
is_dict(orddict,                   []) -> true;
is_dict(orddict,    [{_K,_V} | _Rest]) -> true;
is_dict(orddict,          _Not_A_Dict) -> false;

%% Vbisects have a fast internal validation function.
is_dict(vbisect,            Bare_Dict) -> vbisect:is_vbisect(Bare_Dict).


%%%------------------------------------------------------------------------------
%% API for converting attributes and values
%%%------------------------------------------------------------------------------

%% Filter removes attributes by visiting every element.
-type filter_fn()   :: fun((Key::epode_attr(), Value::epode_value()) -> true | false).

%% Mapping has the potential to change all values.
-type map_error()   :: {error, {invalid_map_result, {any(), any()}}}.
-type map_fn()      :: fun((Key1::epode_attr(), Value1::epode_value())
                         -> Value2::epode_value()).

%% Translation has the potential to change all keys and values.
-type xlate_error() :: {error, {invalid_xlate_result, {any(), any(), any()}}}.
-type xlate_fn()    :: fun((Key1::epode_attr(), Value1::epode_value())
                           -> {Key2::epode_attr(), Value2::epode_value()} | undefined).

%% Fold visits all key and value pairs.
-type fold_error()  :: {error, {invalid_fold_result, any()}}.
-type fold_fn()     :: fun((Key::epode_attr(), Value::epode_value(), AccIn::any())
                           -> AcctOut::any()).

-spec filter    (filter_fn(), epode_dict())                                               -> epode_dict() | not_a_dict.
-spec map       (map_fn(),    epode_dict(), epode_all_dict_type())                        -> epode_dict() | not_a_dict | map_error().
-spec xlate     (xlate_fn(),  epode_dict(), epode_all_dict_type(), epode_all_dict_type()) -> epode_dict() | not_a_dict | xlate_error().
-spec fold      (fold_fn(),   AccIn::any(),  epode_dict())                                -> AccOut::any  | not_a_dict | fold_error().
             
%% Convert just the values (keeping the old attributes) to generate a new dictionary of the same style.
map(User_Map_Fn, Dict, New_Keyval_Type) when ?IS_VALID_KEYVAL(New_Keyval_Type) ->
    Map_Fn = possibly_wrap_map_fn(User_Map_Fn, New_Keyval_Type),
    case {dict_type(Dict), New_Keyval_Type} of
        {not_a_dict,              _} -> not_a_dict;
        {dict,                    _} -> {dict,    New_Keyval_Type, dict    :map(Map_Fn, bare_dict(Dict))};
        {orddict,                 _} -> {orddict, New_Keyval_Type, orddict :map(Map_Fn, bare_dict(Dict))};
        {vbisect,       pure_binary} -> {vbisect, New_Keyval_Type, vbisect :map(Map_Fn, bare_dict(Dict))};
        {Dict_Type, New_Keyval_Type} -> {error, {invalid_map_result, {Dict_Type, New_Keyval_Type}}}
    end;
map(_Map_Fn, Dict,  New_Keyval_Type) -> {error, {invalid_map_result, {dict_type(Dict), New_Keyval_Type}}}.

possibly_wrap_map_fn(Map_Fn, pure_binary) -> map_to_pure_binary_fn(Map_Fn);
possibly_wrap_map_fn(Map_Fn, _Not_Binary) -> Map_Fn.

map_to_pure_binary_fn(User_Fn) ->
    fun(Key, Val) ->
            case User_Fn(Key, Val) of
                New_Value when is_binary(New_Value) -> New_Value
            end
    end.

%% Fold across the keyval pairs.
fold(Fun, Acc0, Dict) ->
    case dict_type(Dict) of
        not_a_dict -> not_a_dict;
        dict       -> dict    :fold  (Fun, Acc0, bare_dict(Dict));
        orddict    -> orddict :fold  (Fun, Acc0, bare_dict(Dict));
        vbisect    -> vbisect :foldl (Fun, Acc0, bare_dict(Dict))
    end.


%% Convert both attributes and values to generate a new dictionary.
-define(MAP_XLATE(__Old_Dict_Type, __New_Dict_Type, __New_Keyval_Type, __Fun, __Bare_Dict),
        {__New_Dict_Type, __New_Keyval_Type,
         __New_Dict_Type:from_list( xlate_attrs(__Fun, __New_Keyval_Type,
                                                __Old_Dict_Type:to_list(__Bare_Dict)) )}).

xlate(User_Xlate_Fn, Dict, New_Dict_Type, New_Keyval_Type) when ?IS_VALID_KEYVAL(New_Keyval_Type) ->
    case dict_type(Dict) of
        not_a_dict    -> not_a_dict;
        Old_Dict_Type -> xlate_valid(Old_Dict_Type, New_Dict_Type, New_Keyval_Type, User_Xlate_Fn, bare_dict(Dict))
    end;
xlate(_Fn, Dict, New_Dict_Type, New_Keyval_Type) ->
    {error, {invalid_xlate_result, {dict_type(Dict), New_Dict_Type, New_Keyval_Type}}}.

xlate_valid(Old_Dict_Type, vbisect, pure_binary, User_Xlate_Fn, Bare_Dict) ->
    ?MAP_XLATE(Old_Dict_Type, vbisect, pure_binary, User_Xlate_Fn, Bare_Dict);
xlate_valid(Old_Dict_Type, New_Dict_Type, New_Keyval_Type, User_Xlate_Fn, Bare_Dict)
  when New_Dict_Type =/= vbisect ->
    ?MAP_XLATE(Old_Dict_Type, New_Dict_Type, New_Keyval_Type, User_Xlate_Fn, Bare_Dict);
xlate_valid(Old_Dict_Type, New_Dict_Type, New_Keyval_Type, _User_Xlate_Fn, _Bare_Dict) ->
    {error, {invalid_xlate_result, {Old_Dict_Type, New_Dict_Type, New_Keyval_Type}}}.


%% TODO: This function needs to systematically handle generating duplicate attributes.
xlate_attrs(Xlate_Fn, New_Keyval_Type, Keyval_Pairs) ->
    lists:foldl(fun({Attr, Value}, New_Pair_List) ->
                        case {New_Keyval_Type, Xlate_Fn(Attr, Value)} of

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

filter(Filter_Fn, Dict) ->
    case dict_type(Dict) of
        not_a_dict -> not_a_dict;
        Dict_Type  -> xlate_valid(Dict_Type, Dict_Type, keyval_type(Dict),
                                  fun(Key, Val) ->
                                          case Filter_Fn(Key, Val) of
                                              true  -> {Key, Val};
                                              false -> undefined
                                          end
                                  end, bare_dict(Dict))
    end.


%%%------------------------------------------------------------------------------
%% API to access dictionary properties...
%%%------------------------------------------------------------------------------

-spec size(any()) -> non_neg_integer() | not_a_dict.

-spec fetch_keys(epode_bdict()) -> [binary()];
                (epode_edict()) -> [atom() | any()].

-spec find(binary(), epode_bdict()) -> {ok, binary()} | error;
          (any(),    epode_edict()) -> {ok, any()}    | error.

-spec fetch(binary(), epode_bdict()) -> binary() | no_return;
           (any(),    epode_edict()) -> any()    | no_return.

-spec is_key(any(), epode_dict()) -> boolean() | not_a_dict.

-spec values(epode_bdict()) -> [binary()];
            (epode_edict()) -> [any()].

size       (Dict)  -> ?DICT_DISPATCH(size,       Dict).
fetch_keys (Dict)  -> ?DICT_DISPATCH(fetch_keys, Dict).
values     (Dict)  ->
    case dict_type(Dict) of
        dict       -> BD = bare_dict(Dict), Keys = dict    :fetch_keys(BD), [dict    :fetch(Key, BD) || Key <- Keys];
        orddict    -> BD = bare_dict(Dict), Keys = orddict :fetch_keys(BD), [orddict :fetch(Key, BD) || Key <- Keys];
        vbisect    -> BD = bare_dict(Dict), vbisect:values(BD);
        not_a_dict -> not_a_dict
    end.                              

find   (Key, Dict) -> ?DICT_DISPATCH(find,   Key, Dict).
fetch  (Key, Dict) -> ?DICT_DISPATCH(fetch,  Key, Dict).
is_key (Key, Dict) -> ?DICT_DISPATCH(is_key, Key, Dict).
