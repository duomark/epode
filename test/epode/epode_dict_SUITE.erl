%%%------------------------------------------------------------------------------
%%% @copyright (c) 2014, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com> [http://duomark.com/]
%%% @reference 2014 Development sponsored by TigerText, Inc. [http://tigertext.com/]
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Test the epode_dict abstract datatype.
%%% @since 0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(epode_dict_SUITE).
-author('jay@duomark.com').

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/1, end_per_group/1
        ]).

%% Actual tests
-export([
         check_invalid_dict/1,          check_invalid_keyval/1,
         check_pure_binary_new/1,       check_atom_new/1,       check_any_new/1,
         check_pure_binary_from_list/1, check_atom_from_list/1, check_any_from_list/1
        ]).

-include("../epode_common_test.hrl").

-include("epode.hrl").

all() -> [{group, construction}].

groups() -> [
             %% Main test groups...
             {construction, [sequence], [
                                         {group, make_dict}, {group, from_list},
                                         check_invalid_dict, check_invalid_keyval
                                        ]},

             %% Supporting groups...
             {make_dict,    [sequence], [check_pure_binary_new,       check_atom_new,       check_any_new]},
             {from_list,    [sequence], [check_pure_binary_from_list, check_atom_from_list, check_any_from_list]}
            ].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(Config) -> Config.
end_per_group(_Config) -> ok.
     

%% Test Module is ?TM
-define(TM, epode_dict).


%%%------------------------------------------------------------------------------
%% Property:   New Dictionary
%% Validation: Type matches request, and 0 elements exist in new dictionary.
%%%------------------------------------------------------------------------------

-spec check_invalid_dict    (config()) -> ok.
-spec check_invalid_keyval  (config()) -> ok.
-spec check_pure_binary_new (config()) -> ok.
-spec check_atom_new        (config()) -> ok.
-spec check_any_new         (config()) -> ok.

map_fn   (_K,V) -> V.
xlate_fn ( K,V) -> {K,V}.

%% Operations on something that is not a dictionary.
%% This is not very robust because we don't yet have good OTP is_dict tests.
check_invalid_dict(_Config) ->
    ct:log("Test non-dictionaries return not_a_dict for epode_dict function calls"),
%% {some, random, tuple}
    %% Arbitrary lists are deemed to be orddicts.
    Bad_Dicts  = [27, [{a,b,c}], <<>>],
    Near_Dicts = [{dict, foo, dict:new()}, {orddict, foo, orddict:new()}, {vbisect, foo, vbisect:new()}],

    false      = lists:foldl(fun(Item, false)      -> ?TM:is_dict (Item) end, false,      Bad_Dicts),
    false      = lists:foldl(fun(Item, false)      -> ?TM:is_dict (Item) end, false,      Near_Dicts),
    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:to_list (Item) end, not_a_dict, Bad_Dicts),
    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:size    (Item) end, not_a_dict, Bad_Dicts),

    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:map   (fun map_fn/2,   Item, any) end, not_a_dict, Bad_Dicts),
    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:xlate (fun xlate_fn/2, Item, any) end, not_a_dict, Bad_Dicts),

    ct:comment("Tested: ~p", [Bad_Dicts]),
    ok.

%% Operations with invalid keyval types specified.

-define(DICT_DICT,    ?TM:new(dict,    any)).
-define(ORDDICT_DICT, ?TM:new(orddict, any)).
-define(VBISECT_DICT, ?TM:new(vbisect, pure_binary)).

-define(DICT_BAD_DICT,    {dict,    any, dict   :from_list([{bad,value}])}).
-define(ORDDICT_BAD_DICT, {orddict, any, orddict:from_list([{bad,value}])}).
-define(VBISECT_BAD_DICT, {vbisect, any, orddict:from_list([{bad,value}])}).
-define(FN_CLAUSE_CRASH   (__Fn_Call), try __Fn_Call catch error:function_clause -> crash end).
-define(CASE_CLAUSE_CRASH (__Fn_Call), try __Fn_Call catch error:{case_clause,_} -> crash end).

check_invalid_keyval(_Config) ->
    ct:log("Crash epode_dict:new/2 function calls with invalid keyval types"),
    crash = ?FN_CLAUSE_CRASH( ?TM:new       (foo,     any)  ),
    crash = ?FN_CLAUSE_CRASH( ?TM:new       (dict,    foo)  ),
    crash = ?FN_CLAUSE_CRASH( ?TM:new       (orddict, foo)  ),
    crash = ?FN_CLAUSE_CRASH( ?TM:new       (vbisect, foo)  ),
    crash = ?FN_CLAUSE_CRASH( ?TM:new       (vbisect, any)  ),
    crash = ?FN_CLAUSE_CRASH( ?TM:new       (vbisect, atom_attrs) ),

    ct:log("Crash epode_dict:from_list/3 function calls with invalid keyval types"),
    crash = ?FN_CLAUSE_CRASH( ?TM:from_list (foo,     any,  []) ),
    crash = ?FN_CLAUSE_CRASH( ?TM:from_list (dict,    foo,  []) ),
    crash = ?FN_CLAUSE_CRASH( ?TM:from_list (orddict, foo,  []) ),
    crash = ?FN_CLAUSE_CRASH( ?TM:from_list (vbisect, foo,  []) ),
    crash = ?FN_CLAUSE_CRASH( ?TM:from_list (vbisect, any,  []) ),
    crash = ?FN_CLAUSE_CRASH( ?TM:from_list (vbisect, atom_attrs, []) ),

    ct:log("Crash epode_dict:map/2 and xlate/3 function calls with invalid keyval type"),
    crash = ?FN_CLAUSE_CRASH   ( ?TM:map   (fun map_fn/2,   ?DICT_DICT,    foo)  ),
    crash = ?FN_CLAUSE_CRASH   ( ?TM:map   (fun map_fn/2,   ?ORDDICT_DICT, foo)  ),
    crash = ?FN_CLAUSE_CRASH   ( ?TM:map   (fun map_fn/2,   ?VBISECT_DICT, foo)  ),
    crash = ?CASE_CLAUSE_CRASH ( ?TM:map   (fun map_fn/2,   ?VBISECT_DICT, any)  ),
    crash = ?CASE_CLAUSE_CRASH ( ?TM:map   (fun map_fn/2,   ?VBISECT_DICT, atom_attrs) ),

    crash = ?FN_CLAUSE_CRASH   ( ?TM:xlate (fun xlate_fn/2, ?DICT_DICT,    foo)  ),
    crash = ?FN_CLAUSE_CRASH   ( ?TM:xlate (fun xlate_fn/2, ?ORDDICT_DICT, foo)  ),
    crash = ?FN_CLAUSE_CRASH   ( ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, foo)  ),
    crash = ?CASE_CLAUSE_CRASH ( ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, any)  ),
    crash = ?CASE_CLAUSE_CRASH ( ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, atom_attrs) ),

    ct:log("Crash epode_dict:map/2 and xlate/3 function calls with invalid dict internal types"),
    %% crash = ?CASE_CLAUSE_CRASH   ( ?TM:map   (fun map_fn/2,   ?DICT_BAD_DICT,    pure_binary) ),
    %% crash = ?CASE_CLAUSE_CRASH   ( ?TM:map   (fun map_fn/2,   ?ORDDICT_BAD_DICT, pure_binary) ),
    crash = ?CASE_CLAUSE_CRASH   ( ?TM:xlate (fun xlate_fn/2, ?DICT_BAD_DICT,    pure_binary) ),
    crash = ?CASE_CLAUSE_CRASH   ( ?TM:xlate (fun xlate_fn/2, ?ORDDICT_BAD_DICT, pure_binary) ),

    not_a_dict = ?TM:map   (fun map_fn/2,   ?VBISECT_BAD_DICT, pure_binary),
    not_a_dict = ?TM:xlate (fun xlate_fn/2, ?VBISECT_BAD_DICT, pure_binary),

    ct:comment("Tested: ~p", [[new, from_list, map, xlate]]),
    ok.
    

%% Construction and verification of empty dictionaries.
check_pure_binary_new(_Config) ->
    Log_Stmt = "Test empty dictionary construction for all pure_binary keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_new, fun pure_binary_constructor/2, 6),
    ok.

check_atom_new(_Config) ->
    Log_Stmt = "Test empty dictionary construction for all atom_attrs keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_new, fun atom_attrs_constructor/2, 4),
    ok.

check_any_new(_Config) ->
    Log_Stmt = "Test empty dictionary construction for all any keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_new, fun any_constructor/2, 4),
    ok.

%% Support functions for empty constructor quickcheck testing.
pure_binary_constructor(PD_Key, Num_Tests) ->
    Test = ?FORALL(Dict_Type, epode_all_dict_type(),
                   valid_empty_dict(Dict_Type, ?TM:new(Dict_Type, pure_binary), PD_Key)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

atom_attrs_constructor(PD_Key, Num_Tests) ->
    Test = ?FORALL(Dict_Type, epode_atom_dict_type(),
                   valid_empty_dict(Dict_Type, ?TM:new(Dict_Type, atom_attrs), PD_Key)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

any_constructor(PD_Key, Num_Tests) ->
    Test = ?FORALL(Dict_Type, epode_any_dict_type(),
                   valid_empty_dict(Dict_Type, ?TM:new(Dict_Type, any), PD_Key)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

valid_empty_dict(Dict_Type, Dict, PD_Key) ->
    true = ?TM:is_dict (Dict),
    0    = ?TM:size    (Dict),
    put(PD_Key, orddict:update_counter(Dict_Type, 1, get(PD_Key))),
    true.


%%%------------------------------------------------------------------------------
%% Property:   New Dictionary from list of data
%% Validation: Type matches request, and elements exist in new dictionary.
%%%------------------------------------------------------------------------------

-spec check_pure_binary_from_list (config()) -> ok.
-spec check_atom_from_list        (config()) -> ok.
-spec check_any_from_list         (config()) -> ok.

%% Construction and verification of empty dictionaries.
check_pure_binary_from_list(_Config) ->
    Log_Stmt = "Test dictionary constructed from_list for all pure_binary keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_from_list, fun pure_binary_from_list/2, 100),
    ok.

check_atom_from_list(_Config) ->
    Log_Stmt = "Test dictionary constructed from_list for all atom_attrs keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_from_list, fun atom_attrs_from_list/2, 100),
    ok.

check_any_from_list(_Config) ->
    Log_Stmt = "Test dictionary constructed from_list for all any keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_from_list, fun any_from_list/2, 100),
    ok.

%% Support functions for from_list constructor quickcheck testing.
pure_binary_from_list(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List}, {epode_all_dict_type(), list({non_empty(binary()), binary()})},
                   valid_starting_dict(Dict_Type, ?TM:from_list(Dict_Type, pure_binary, Attr_List), PD_Key, Attr_List)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

atom_attrs_from_list(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List}, {epode_atom_dict_type(), list({non_empty(atom()), any()})},
                   valid_starting_dict(Dict_Type, ?TM:from_list(Dict_Type, pure_binary, Attr_List), PD_Key, Attr_List)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

any_from_list(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List}, {epode_any_dict_type(), list({non_empty(any()), any()})},
                   valid_starting_dict(Dict_Type, ?TM:from_list(Dict_Type, pure_binary, Attr_List), PD_Key, Attr_List)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

make_orddict(Attrs) ->    
    lists:foldl(fun({Key, Val}, Acc_Dict) ->
                        case orddict:is_key(Key, Acc_Dict) of
                            false -> orddict:store(Key, Val, Acc_Dict);
                            true  -> Acc_Dict
                        end
                end, orddict:new(), Attrs).

valid_starting_dict(Dict_Type,  Dict, PD_Key, Orig_Props) ->
    Unshadowed_Props = make_orddict(Orig_Props),
    Exp_Size = orddict:size(Unshadowed_Props),
    log_from_list_case(length(Orig_Props), Exp_Size, orddict:to_list(Unshadowed_Props)),
    true     = ?TM:is_dict(Dict),
    Exp_Size = ?TM:size(Dict),
    Unshadowed_Props = lists:sort(?TM:to_list(Dict)),
    
    %% Report the number of tests run for each Dict_Type.
    put(PD_Key, orddict:update_counter(Dict_Type, 1, get(PD_Key))),
    true.

log_from_list_case(0, 0, []) ->
    ct:log("Num_Attrs: 0  Key_Size_Range: {0, 0}  Val_Size_Range {0, 0}~n", []);
log_from_list_case(Num_Props, Num_Unique_Props, Unique_Props) ->
    %% ct:log("Unshadowed: ~p (~p,~p)~n", [Unique_Props, Num_Props, Num_Unique_Props]),
    {Key_Sizes, Val_Sizes}
        = lists:foldl(fun({Key, Val}, {KSizes, VSizes}) ->
                              {[elem_size(Key) | KSizes], [elem_size(Val) | VSizes]}
                      end, {[], []}, Unique_Props),
    {Min_Key, Max_Key} = {lists:min(Key_Sizes), lists:max(Key_Sizes)},
    {Min_Val, Max_Val} = {lists:min(Val_Sizes), lists:max(Val_Sizes)},
    ct:log("Num_Attrs: ~p  Key_Size_Range: ~p  Val_Size_Range ~p~n",
           [{Num_Props, Num_Unique_Props}, {Min_Key, Max_Key}, {Min_Val, Max_Val}]).

elem_size( Item) when is_list(Item)   -> length(Item);
elem_size( Item) when is_binary(Item) -> byte_size(Item);
elem_size( Item) when is_atom(Item)   -> length(atom_to_list(Item));
elem_size(_Item)                      -> -1.
