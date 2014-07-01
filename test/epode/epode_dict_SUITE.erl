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
         check_pure_binary_from_list/1, check_atom_from_list/1, check_any_from_list/1,
         check_pure_binary_map_fn/1,    check_atom_map_fn/1,    check_any_map_fn/1,
         check_bin_xlate_fn/1,          check_atom_xlate_fn/1,  check_any_xlate_fn/1
        ]).

-include("../epode_common_test.hrl").

-include("epode.hrl").

all() -> [{group, construction}, {group, translation}].

groups() -> [
             %% Main test groups...
             {construction, [sequence], [
                                         {group, make_dict}, {group, from_list},
                                         check_invalid_dict, check_invalid_keyval
                                        ]},
             {translation,  [sequence], [
                                         {group, map_fn},    {group, xlate_fn}
                                        ]},

             %% Supporting groups...
             {make_dict,    [sequence], [check_pure_binary_new,       check_atom_new,       check_any_new]},
             {from_list,    [sequence], [check_pure_binary_from_list, check_atom_from_list, check_any_from_list]},
             {map_fn,       [sequence], [check_pure_binary_map_fn,    check_atom_map_fn,    check_any_map_fn]},
             {xlate_fn,     [sequence], [check_bin_xlate_fn,          check_atom_xlate_fn,  check_any_xlate_fn]}
            ].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(Config) -> Config.
end_per_group(_Config) -> ok.
     

%% Test Module is ?TM
-define(TM, epode_dict).


%%%------------------------------------------------------------------------------
%%% Property:   New Dictionary
%%% Validation: Type matches request, and 0 elements exist in new dictionary.
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

    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:map   (fun map_fn/2,   Item, any)          end, not_a_dict, Bad_Dicts),
    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:xlate (fun xlate_fn/2, Item, dict,    any) end, not_a_dict, Bad_Dicts),
    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:xlate (fun xlate_fn/2, Item, orddict, any) end, not_a_dict, Bad_Dicts),
    not_a_dict = lists:foldl(fun(Item, not_a_dict) -> ?TM:xlate (fun xlate_fn/2, Item, vbisect, any) end, not_a_dict, Bad_Dicts),

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
    {error, {invalid_types, {foo,     any}}}        = ?TM:new       (foo,     any),
    {error, {invalid_types, {dict,    foo}}}        = ?TM:new       (dict,    foo),
    {error, {invalid_types, {orddict, foo}}}        = ?TM:new       (orddict, foo),
    {error, {invalid_types, {vbisect, foo}}}        = ?TM:new       (vbisect, foo),
    {error, {invalid_types, {vbisect, any}}}        = ?TM:new       (vbisect, any),
    {error, {invalid_types, {vbisect, atom_attrs}}} = ?TM:new       (vbisect, atom_attrs),

    ct:log("Crash epode_dict:from_list/3 function calls with invalid keyval types"),
    {error, {invalid_types, {foo,     any}}}        = ?TM:from_list (foo,     any, []),
    {error, {invalid_types, {dict,    foo}}}        = ?TM:from_list (dict,    foo, []),
    {error, {invalid_types, {orddict, foo}}}        = ?TM:from_list (orddict, foo, []),
    {error, {invalid_types, {vbisect, foo}}}        = ?TM:from_list (vbisect, foo, []),
    {error, {invalid_types, {vbisect, any}}}        = ?TM:from_list (vbisect, any, []),
    {error, {invalid_types, {vbisect, atom_attrs}}} = ?TM:from_list (vbisect, atom_attrs, []),

    ct:log("Crash epode_dict:map/2 and xlate/3 function calls with invalid keyval type"),
    {error, {invalid_map_result, {dict,    foo}}}        = ?TM:map   (fun map_fn/2,   ?DICT_DICT,    foo),
    {error, {invalid_map_result, {orddict, foo}}}        = ?TM:map   (fun map_fn/2,   ?ORDDICT_DICT, foo),
    {error, {invalid_map_result, {vbisect, foo}}}        = ?TM:map   (fun map_fn/2,   ?VBISECT_DICT, foo),
    {error, {invalid_map_result, {vbisect, any}}}        = ?TM:map   (fun map_fn/2,   ?VBISECT_DICT, any),
    {error, {invalid_map_result, {vbisect, atom_attrs}}} = ?TM:map   (fun map_fn/2,   ?VBISECT_DICT, atom_attrs),

    {error, {invalid_xlate_result, {dict,    dict,    foo}}}        = ?TM:xlate (fun xlate_fn/2, ?DICT_DICT,    dict,    foo),
    {error, {invalid_xlate_result, {dict,    orddict, foo}}}        = ?TM:xlate (fun xlate_fn/2, ?DICT_DICT,    orddict, foo),
    {error, {invalid_xlate_result, {dict,    vbisect, foo}}}        = ?TM:xlate (fun xlate_fn/2, ?DICT_DICT,    vbisect, foo),
    {error, {invalid_xlate_result, {orddict, dict,    foo}}}        = ?TM:xlate (fun xlate_fn/2, ?ORDDICT_DICT, dict,    foo),
    {error, {invalid_xlate_result, {orddict, orddict, foo}}}        = ?TM:xlate (fun xlate_fn/2, ?ORDDICT_DICT, orddict, foo),
    {error, {invalid_xlate_result, {orddict, vbisect, foo}}}        = ?TM:xlate (fun xlate_fn/2, ?ORDDICT_DICT, vbisect, foo),
    {error, {invalid_xlate_result, {vbisect, dict,    foo}}}        = ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, dict,    foo),
    {error, {invalid_xlate_result, {vbisect, orddict, foo}}}        = ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, orddict, foo),
    {error, {invalid_xlate_result, {vbisect, vbisect, foo}}}        = ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, vbisect, foo),
    {error, {invalid_xlate_result, {dict,    vbisect, any}}}        = ?TM:xlate (fun xlate_fn/2, ?DICT_DICT,    vbisect, any),
    {error, {invalid_xlate_result, {orddict, vbisect, any}}}        = ?TM:xlate (fun xlate_fn/2, ?ORDDICT_DICT, vbisect, any),
    {error, {invalid_xlate_result, {vbisect, vbisect, any}}}        = ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, vbisect, any),
    {error, {invalid_xlate_result, {dict,    vbisect, atom_attrs}}} = ?TM:xlate (fun xlate_fn/2, ?DICT_DICT,    vbisect, atom_attrs),
    {error, {invalid_xlate_result, {orddict, vbisect, atom_attrs}}} = ?TM:xlate (fun xlate_fn/2, ?ORDDICT_DICT, vbisect, atom_attrs),
    {error, {invalid_xlate_result, {vbisect, vbisect, atom_attrs}}} = ?TM:xlate (fun xlate_fn/2, ?VBISECT_DICT, vbisect, atom_attrs),

    ct:log("Crash epode_dict:map/2 and xlate/3 function calls with invalid dict internal types"),
    crash = ?CASE_CLAUSE_CRASH   ( ?TM:map   (fun map_fn/2,   ?DICT_BAD_DICT,    pure_binary) ),
    crash = ?CASE_CLAUSE_CRASH   ( ?TM:map   (fun map_fn/2,   ?ORDDICT_BAD_DICT, pure_binary) ),
    crash = ?CASE_CLAUSE_CRASH   ( ?TM:xlate (fun xlate_fn/2, ?DICT_BAD_DICT,    dict,    pure_binary) ),
    crash = ?CASE_CLAUSE_CRASH   ( ?TM:xlate (fun xlate_fn/2, ?ORDDICT_BAD_DICT, orddict, pure_binary) ),

    not_a_dict = ?TM:map   (fun map_fn/2,   ?VBISECT_BAD_DICT, pure_binary),
    not_a_dict = ?TM:xlate (fun xlate_fn/2, ?VBISECT_BAD_DICT, dict,    pure_binary),
    not_a_dict = ?TM:xlate (fun xlate_fn/2, ?VBISECT_BAD_DICT, orddict, pure_binary),
    not_a_dict = ?TM:xlate (fun xlate_fn/2, ?VBISECT_BAD_DICT, vbisect, pure_binary),

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
%%% Property:   New Dictionary from list of data
%%% Validation: Type matches request, and elements exist in new dictionary.
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

valid_starting_dict(Dict_Type, Dict, PD_Key, Orig_Props) ->
    Unshadowed_Props = make_orddict(Orig_Props),
    Exp_Size = orddict:size(Unshadowed_Props),
    log_from_list_case(old, length(Orig_Props), Exp_Size, orddict:to_list(Unshadowed_Props)),
    true     = ?TM:is_dict(Dict),
    Exp_Size = ?TM:size(Dict),
    Unshadowed_Props = lists:sort(?TM:to_list(Dict)),
    
    %% Report the number of tests run for each Dict_Type.
    put(PD_Key, orddict:update_counter(Dict_Type, 1, get(PD_Key))),
    true.

elem_size( Item) when is_list(Item)   -> length(Item);
elem_size( Item) when is_binary(Item) -> byte_size(Item);
elem_size( Item) when is_atom(Item)   -> length(atom_to_list(Item));
elem_size(_Item)                      -> -1.


%%%------------------------------------------------------------------------------
%%% Property:   New Dictionary from list of data
%%% Validation: Type matches request, and elements exist in new dictionary.
%%%------------------------------------------------------------------------------

-spec check_pure_binary_map_fn (config()) -> ok.
-spec check_atom_map_fn        (config()) -> ok.
-spec check_any_map_fn         (config()) -> ok.

-spec check_bin_xlate_fn       (config()) -> ok.
-spec check_atom_xlate_fn      (config()) -> ok.

check_pure_binary_map_fn(_Config) ->
    Log_Stmt = "Test mapping values to a new dictionary (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_map, fun map_binary_size/2, 100),
    ok.

check_atom_map_fn(_Config) ->
    Log_Stmt = "Test mapping values to a new dictionary (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_map, fun map_atom_attrs/2, 100),
    ok.

check_any_map_fn(_Config) ->
    Log_Stmt = "Test mapping values to a new dictionary (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_map, fun map_any/2, 100),
    ok.

map_binary_size(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List}, {epode_bdict_type(), list({non_empty(binary()), binary()})},
                   map_dict(Dict_Type, ?TM:from_list(Dict_Type, pure_binary, Attr_List), PD_Key)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

map_atom_attrs(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List}, {epode_atom_dict_type(), list({atom(), any()})},
                   map_dup(Dict_Type, ?TM:from_list(Dict_Type, atom_attrs, Attr_List), PD_Key, atom_attrs)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

map_any(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List}, {epode_atom_dict_type(), list({any(), any()})},
                   map_dup(Dict_Type, ?TM:from_list(Dict_Type, any, Attr_List), PD_Key, any)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

trunc_binary(_Key, <<Val:3/binary, _Rest/binary>>) -> Val;
trunc_binary(_Key, <<Val/binary>>)                 -> Val;
trunc_binary(_Key, <<>>)                           -> <<>>.

dup_any(_Key, Val) -> {Val, Val}.
    

map_dict(Dict_Type, Dict, PD_Key) ->
    Exp_Size   = ?TM:size(Dict),
    Orig_Props = lists:sort(?TM:to_list(Dict)),
    Exp_Props  = [{Key, case byte_size(Val) of 
                            Big when Big > 3 -> binary:part(Val, {0, 3});
                            _Small -> Val
                        end} || {Key, Val} <- Orig_Props],
    New_Dict   = ?TM:map(fun trunc_binary/2, Dict, pure_binary),
    New_Props  = lists:sort(?TM:to_list(New_Dict)),
    log_from_list_case(old, ?TM:size(Dict), ?TM:size(New_Dict), Orig_Props),
    log_from_list_case(new, ?TM:size(Dict), ?TM:size(New_Dict), New_Props),
    true       = ?TM:is_dict(New_Dict),
    Exp_Size   = ?TM:size(New_Dict),
    Exp_Props  = New_Props,
    
    %% Report the number of tests run for each Dict_Type.
    put(PD_Key, orddict:update_counter(Dict_Type, 1, get(PD_Key))),
    true.

map_dup(Dict_Type, Dict, PD_Key, New_Keyval_Type) ->
    Exp_Size   = ?TM:size(Dict),
    Orig_Props = lists:sort(?TM:to_list(Dict)),
    Exp_Props  = [{Key, {Val, Val}} || {Key, Val} <- Orig_Props],
    New_Dict   = ?TM:map(fun dup_any/2, Dict, New_Keyval_Type),
    New_Props  = lists:sort(?TM:to_list(New_Dict)),
    log_from_list_case(old, ?TM:size(Dict), ?TM:size(New_Dict), Orig_Props),
    log_from_list_case(new, ?TM:size(Dict), ?TM:size(New_Dict), New_Props),
    true       = ?TM:is_dict(New_Dict),
    Exp_Size   = ?TM:size(New_Dict),
    Exp_Props  = New_Props,
    
    %% Report the number of tests run for each Dict_Type.
    put(PD_Key, orddict:update_counter(Dict_Type, 1, get(PD_Key))),
    true.
    

check_bin_xlate_fn(_Config) ->
    Log_Stmt_1 = "Test translating bin keys and values to a new atomkey dictionary (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt_1, ct_check_map, fun xlate_atomkey_stringval/2, 100),
    Log_Stmt_2 = "Test translating bin keys and values to a new listkey dictionary (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt_2, ct_check_map, fun xlate_string_intval/2, 100),
    ok.

check_atom_xlate_fn(_Config) ->
    Log_Stmt = "Test translating atom keys and values to a new binary dictionary (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_map, fun xlate_atoms_to_bin/2, 100),
    ok.

check_any_xlate_fn(_Config) ->
    Log_Stmt = "Test translating atom keys and values to a new any dictionary (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_map, fun xlate_atoms_to_bin/2, 100),
    ok.

xlate_atomkey_stringval(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List, New_Dict_Type, New_Keyval_Type},
                   {epode_bdict_type(), list({atom(), binary()}),
                    epode_atom_dict_type(), epode_keyval_atom_type()},

                   begin
                       Old_Dict = ?TM:from_list(Dict_Type, pure_binary,
                                                [{list_to_binary(atom_to_list(K)), V}
                                                 || {K,V} <- Attr_List, K =/= '']),
                       xlate_attrs(Old_Dict, New_Dict_Type, PD_Key, New_Keyval_Type, fun bin_to_atom_attrs/2)
                   end),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

xlate_string_intval(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List, New_Dict_Type, New_Keyval_Type},
                   {epode_bdict_type(), list({non_empty(binary()), binary()}),
                    epode_any_dict_type(), epode_keyval_any_type()},

                   xlate_attrs(?TM:from_list(Dict_Type, pure_binary, Attr_List),
                               New_Dict_Type, PD_Key, New_Keyval_Type, fun bin_to_any_attrs/2)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

xlate_atoms_to_bin(PD_Key, Num_Tests) ->
    Test = ?FORALL({Dict_Type, Attr_List, New_Dict_Type, New_Keyval_Type},
                   {epode_atom_dict_type(), list({atom(), binary()}),
                    epode_bdict_type(), epode_keyval_binary_type()},

                   xlate_attrs(?TM:from_list(Dict_Type, atom_attrs, Attr_List),
                               New_Dict_Type, PD_Key, New_Keyval_Type, fun atom_to_bin_attrs/2)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

atom_to_any_attrs (Key, Val) -> {atom_to_list(Key), {value, byte_size(Val)}}.
atom_to_bin_attrs (Key, Val) -> {list_to_binary(atom_to_list(Key)), term_to_binary(Val)}.
bin_to_atom_attrs (Key, Val) -> {list_to_atom(binary_to_list(Key) ++ "-atom"), {value, binary_to_list(Val)}}.
bin_to_any_attrs  (Key, Val) -> {binary_to_list(Key), binary_to_list(Val)}.

xlate_attrs({Old_Dict_Type, Old_Keyval_Type, _} = Dict, New_Dict_Type, PD_Key, New_Keyval_Type, Attr_Fn) ->
    Exp_Size   = ?TM:size(Dict),
    Orig_Props = lists:sort(?TM:to_list(Dict)),
    {Exp_Props, New_Dict}
        = case {Old_Keyval_Type, New_Keyval_Type} of
              {atom_attrs, pure_binary} ->
                  {
                    [{list_to_binary(atom_to_list(Key)), term_to_binary(Val)} || {Key, Val} <- Orig_Props],
                    ?TM:xlate(Attr_Fn, Dict, New_Dict_Type, New_Keyval_Type)
                  };
              {atom_attrs, any} ->
                  {
                    [{atom_to_list(Key), {value, byte_size(Val)}} || {Key, Val} <- Orig_Props],
                    ?TM:xlate(fun bin_to_any_attrs/2, Dict, New_Dict_Type, New_Keyval_Type)
                  };
              {pure_binary, atom_attrs} ->
                  {
                    [{list_to_atom(binary_to_list(Key) ++ "-atom"), {value, binary_to_list(Val)}} || {Key, Val} <- Orig_Props],
                    ?TM:xlate(Attr_Fn, Dict, New_Dict_Type, New_Keyval_Type)
                  };
              {pure_binary, any} ->
                  {
                    [{binary_to_list(Key), binary_to_list(Val)} || {Key, Val} <- Orig_Props],
                    ?TM:xlate(Attr_Fn, Dict, New_Dict_Type, New_Keyval_Type)
                  }
          end,
    New_Props  = lists:sort(?TM:to_list(New_Dict)),
    log_from_xlate_case(old, ?TM:size(Dict), ?TM:size(New_Dict), Orig_Props, Old_Dict_Type, Old_Keyval_Type),
    log_from_xlate_case(new, ?TM:size(Dict), ?TM:size(New_Dict), New_Props,  New_Dict_Type, New_Keyval_Type),
    true       = ?TM:is_dict(New_Dict),
    Exp_Size   = ?TM:size(New_Dict),
    Exp_Sorted = lists:sort(Exp_Props),
    Exp_Sorted = New_Props,
    
    %% Report the number of tests run for each Dict_Type.
    put(PD_Key, orddict:update_counter(New_Dict_Type, 1, get(PD_Key))),
    true.


%%%------------------------------------------------------------------------------
%%% Logging functions
%%%------------------------------------------------------------------------------

log_from_list_case(Old_Or_New, 0, 0, []) ->
    ct:log("~p Num_Attrs: 0  Key_Size_Range: {0, 0}  Val_Size_Range {0, 0}~n", [Old_Or_New]);
log_from_list_case(Old_Or_New, Num_Props, Num_Unique_Props, Unique_Props) ->
    %% ct:log("~p Unshadowed: ~p (~p,~p)~n", [Old_Or_New, Unique_Props, Num_Props, Num_Unique_Props]),
    {Key_Sizes, Val_Sizes}
        = lists:foldl(fun({Key, Val}, {KSizes, VSizes}) ->
                              {[elem_size(Key) | KSizes], [elem_size(Val) | VSizes]}
                      end, {[], []}, Unique_Props),
    {Min_Key, Max_Key} = {lists:min(Key_Sizes), lists:max(Key_Sizes)},
    {Min_Val, Max_Val} = {lists:min(Val_Sizes), lists:max(Val_Sizes)},
    ct:log("~p Num_Attrs: ~p  Key_Size_Range: ~p  Val_Size_Range ~p~n",
           [Old_Or_New, {Num_Props, Num_Unique_Props}, {Min_Key, Max_Key}, {Min_Val, Max_Val}]).

log_from_xlate_case(Old_Or_New, 0, 0, [], Dict_Type, Keyval_Type) ->
    ct:log("~p Num_Attrs: 0  Key_Size_Range: {0, 0}  Val_Size_Range {0, 0}  Dict: ~p ~n",
           [Old_Or_New, {Dict_Type, Keyval_Type}]);
log_from_xlate_case(Old_Or_New, Num_Props, Num_Unique_Props, Unique_Props, Dict_Type, Keyval_Type) ->
    %% ct:log("~p Unshadowed: ~p (~p,~p)~n", [Old_Or_New, Unique_Props, Num_Props, Num_Unique_Props]),
    {Key_Sizes, Val_Sizes}
        = lists:foldl(fun({Key, Val}, {KSizes, VSizes}) ->
                              {[elem_size(Key) | KSizes], [elem_size(Val) | VSizes]}
                      end, {[], []}, Unique_Props),
    {Min_Key, Max_Key} = {lists:min(Key_Sizes), lists:max(Key_Sizes)},
    {Min_Val, Max_Val} = {lists:min(Val_Sizes), lists:max(Val_Sizes)},
    ct:log("~p Num_Attrs: ~p  Key_Size_Range: ~p  Val_Size_Range ~p Dict: ~p~n",
           [Old_Or_New, {Num_Props, Num_Unique_Props}, {Min_Key, Max_Key},
            {Min_Val, Max_Val}, {Dict_Type, Keyval_Type}]).
