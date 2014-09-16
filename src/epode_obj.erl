%%%------------------------------------------------------------------------------
%%% @copyright (c) 2014, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com> [http://duomark.com/]
%%% @reference 2014 Development sponsored by TigerText, Inc. [http://tigertext.com/]
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Definition of epode objects based on internal epode dictionaries.
%%% @since 0.1.1
%%% @end
%%%------------------------------------------------------------------------------
-module(epode_obj).
-author('Jay Nelson <jay@duomark.com>').

%% External interface for epode_dict construction and identity
-export([
         is_epode_obj/2,
         is_epode_obj/1,
         new/4,
         coalesce_attrs/3,
         merge_attrs/2,
         filter_projection/2,
         filter_projection/3,
         find_projection/2,
         xlate/5
        ]).

-include("epode.hrl").

-define(EPODE_OBJ, epode_obj).

%% An epode "object" is an epode_dict() tagged with a user defined "object type"
-type obj_type()  :: atom().
-type epode_obj() :: {?EPODE_OBJ, obj_type(), epode_bdict() | epode_edict()}.
-type epode_obj(Obj_Type, Epode_Dict) :: {?EPODE_OBJ, Obj_Type, Epode_Dict}.
-export_type([obj_type/0, epode_obj/2]).


%%%---------------------------------------------------
%%% External API
%%%---------------------------------------------------
-spec is_epode_obj(any(), obj_type()) -> binary().
is_epode_obj({?EPODE_OBJ, Obj_Type, _Epode_Dict} = Epode, Obj_Type) ->
    is_epode_obj(Epode).

-spec is_epode_obj(any()) -> binary().
is_epode_obj({?EPODE_OBJ, Obj_Type, Epode_Dict})
 when is_atom(Obj_Type) ->
    epode_dict:is_dict(Epode_Dict).


-spec new(obj_type(), epode_all_dict_type(), epode_keyval_type(), any_dict_list()) -> epode_obj().
new(Obj_Type, Dict_Type, Keyval_Type, Attrs)
  when is_atom(Obj_Type) ->
    Epode_Dict = epode_dict:from_list(Dict_Type, Keyval_Type, Attrs),
    {?EPODE_OBJ, Obj_Type, Epode_Dict}.

-spec coalesce_attrs(Obj1::epode_obj(), Obj2::epode_obj(), obj_type()) -> Obj3::epode_obj().
coalesce_attrs({?EPODE_OBJ, _Obj_Type1, Epode_Dict1},
               {?EPODE_OBJ, _Obj_Type2, Epode_Dict2}, New_Obj_Type)
  when Epode_Dict1 =/= Epode_Dict2 ->
    First_Wins_Fun = fun(_Key, Value1, _Value2) -> Value1 end,
    {?EPODE_OBJ, New_Obj_Type, epode_dict:merge(First_Wins_Fun, Epode_Dict1, Epode_Dict2)}.

-spec merge_attrs(Obj1::epode_obj(), Obj2::epode_obj()) -> Obj3::epode_obj().
merge_attrs({?EPODE_OBJ, Obj_Type, Epode_Dict1},
            {?EPODE_OBJ, Obj_Type, Epode_Dict2})
  when Epode_Dict1 =/= Epode_Dict2 ->
    First_Wins_Fun = fun(_Key, Value1, _Value2) -> Value1 end,
    {?EPODE_OBJ, Obj_Type, epode_dict:merge(First_Wins_Fun, Epode_Dict1, Epode_Dict2)}.

-spec filter_projection([Attrs::any()], Obj1::epode_obj()) -> Obj2::epode_obj().
filter_projection(Remove_Keys, {?EPODE_OBJ, Obj_Type, Epode_Dict})
  when is_list(Remove_Keys) ->
    {?EPODE_OBJ, Obj_Type, remove_keys(Remove_Keys, Epode_Dict)}.

-spec filter_projection([Attrs::any()], Obj1::epode_obj(), obj_type()) -> Obj2::epode_obj().
filter_projection(Remove_Keys, {?EPODE_OBJ, Obj_Type, Epode_Dict}, New_Obj_Type)
  when is_list(Remove_Keys), Obj_Type =/= New_Obj_Type ->
    {?EPODE_OBJ, New_Obj_Type, remove_keys(Remove_Keys, Epode_Dict)}.

remove_keys(Remove_Keys, Epode_Dict) ->
    Remove_Fun = fun(Key, _Val) -> not lists:is_member(Key, Remove_Keys) end,
    epode_dict:filter(Remove_Fun, Epode_Dict).
    
%% Get a specific list of attribute values in requested order without keys.
-spec find_projection([Attrs::any()], epode_obj()) -> [Values::any()].
find_projection(Keep_Keys, {?EPODE_OBJ, _Obj_Type, Epode_Dict})
  when is_list(Keep_Keys) ->
    [Value || Key <- Keep_Keys, ((Value = epode_dict:find(Key, Epode_Dict)) =/= error)].

-spec xlate(epode_dict:xlate_fun(), epode_obj(), obj_type(), epode_all_dict_type(), epode_keyval_any_type())
           -> epode_obj().
xlate(Xlate_Fun, {?EPODE_OBJ, Obj_Type, Epode_Dict}, New_Obj_Type, New_Dict_Type, New_Keyval_Type)
  when Obj_Type =/= New_Obj_Type ->
    {?EPODE_OBJ, New_Obj_Type, epode_dict:xlate(Xlate_Fun, Epode_Dict, New_Dict_Type, New_Keyval_Type)}.
