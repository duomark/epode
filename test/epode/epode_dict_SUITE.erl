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
         check_invalid/1,
         check_pure_binary_new/1,       check_atom_new/1,       check_any_new/1,
         check_pure_binary_from_list/1, check_atom_from_list/1, check_any_from_list/1
        ]).

-include("../epode_common_test.hrl").

-include("epode.hrl").

all() -> [{group, construction}].

groups() -> [
             %% Main test groups...
             {construction, [sequence], [check_invalid, {group, make_dict}, {group, from_list}]},

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

-spec check_invalid         (config()) -> ok.
-spec check_pure_binary_new (config()) -> ok.
-spec check_atom_new        (config()) -> ok.
-spec check_any_new         (config()) -> ok.

%% Operations on something that is not a dictionary.
%% This is not very robust because we don't yet have good OTP is_dict tests.
check_invalid(_Config) ->
    ct:log("Test non-dictionaries return not_a_dict for epode_dict:size(Thing)"),
    %% Arbitrary lists are deemed to be orddicts.
    Bad_Dicts = [27, {some, random, tuple}, [{a,b,c}], <<>>],
    Expected = lists:duplicate(length(Bad_Dicts), not_a_dict),
    Expected = [not_a_dict = ?TM:size(Item) || Item <- Bad_Dicts],
    ct:comment("Tested: ~p", [Bad_Dicts]),
    ok.

%% Construction and verification of empty dictionaries.
check_pure_binary_new(_Config) ->
    Log_Stmt = "Test empty dictionary construction for all pure_binary keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_new, fun pure_binary_constructor/2, 6).

check_atom_new(_Config) ->
    Log_Stmt = "Test empty dictionary construction for all atom_attrs keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_new, fun atom_attrs_constructor/2, 4).

check_any_new(_Config) ->
    Log_Stmt = "Test empty dictionary construction for all any keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_new, fun any_constructor/2, 4).

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
%% -spec check_atom_from_list        (config()) -> ok.
%% -spec check_any_from_list         (config()) -> ok.

%% Construction and verification of empty dictionaries.
check_pure_binary_from_list(_Config) ->
    Log_Stmt = "Test dictionary constructed from_list for all pure_binary keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_from_list, fun pure_binary_from_list/2, 100).

check_atom_from_list(_Config) ->
    Log_Stmt = "Test dictionary constructed from_list for all atom_attrs keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_from_list, fun atom_attrs_from_list/2, 100).

check_any_from_list(_Config) ->
    Log_Stmt = "Test dictionary constructed from_list for all any keyval dictionary types (~p tests)",
    true = epode_common_test:test_count_wrapper(Log_Stmt, ct_check_from_list, fun any_from_list/2, 100).

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
    Log_Stmt = "Test dictionary constructed from_list fall all pure_binary dictionary types (~p tests)",
    Test = ?FORALL({Dict_Type, Attr_List}, {epode_any_dict_type(), list({non_empty(any()), any()})},
                   valid_starting_dict(Dict_Type, ?TM:from_list(Dict_Type, pure_binary, Attr_List), PD_Key, Attr_List)),
    proper:quickcheck(Test, ?PQ_NUM(Num_Tests)).

make_orddict(Attrs) ->    
    lists:foldl(fun({Key, Val}, Acc_Dict) ->
                        case orddict:is_key(Key, Acc_Dict) of
                            true  -> orddict:store(Key, Val, Acc_Dict);
                            false -> Acc_Dict
                        end
                end, orddict:new(), Attrs).
    
%% valid_starting_dict(not_a_dict, Dict, PD_Key, Orig_Props) ->
valid_starting_dict(Dict_Type,  Dict, PD_Key, Orig_Props) ->
    Unshadowed_Props = make_orddict(Orig_Props),
    Exp_Size = orddict:size(Unshadowed_Props),
    true     = ?TM:is_dict(Dict),
    Exp_Size = ?TM:size(Dict),
    Unshadowed_Props = lists:sort(?TM:to_list(Dict)),
    
    %% Report the number of tests run for each Dict_Type.
    put(PD_Key, orddict:update_counter(Dict_Type, 1, get(PD_Key))),
    true.
