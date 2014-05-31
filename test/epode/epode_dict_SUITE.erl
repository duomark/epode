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
         check_new/1, check_invalid/1
        ]).

-include("../epode_common_test.hrl").

-include("epode.hrl").

all() -> [{group, construction}].

groups() -> [
             {construction, [sequence],
              [{make_dict, [sequence], [check_new, check_invalid]}]
             }
            ].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(Config) -> Config.
end_per_group(_Config) -> ok.
     

%% Test Module is ?TM
-define(TM, epode_dict).


%%%------------------------------------------------------------------------------
%% Property: New Dictionary
%% Validation: Type matches request, and 0 elements exist in new dictionary.
%%%------------------------------------------------------------------------------
-spec check_new(config()) -> ok.
check_new(_Config) ->
    Test_Fn = fun(PD_Key, Num_Tests) ->
                      Test_New = ?FORALL(Dict_Type, epode_dict_type(),
                                         valid_empty_dict(Dict_Type, ?TM:new(Dict_Type), PD_Key)),
                      proper:quickcheck(Test_New, ?PQ_NUM(Num_Tests))
              end,
    Log_Stmt = "Test empty dictionary construction for all dictionary types (~p tests)",
    epode_common_test:test_count_wrapper(Log_Stmt, ct_check_new, Test_Fn, 6).

valid_empty_dict(Dict_Type, Dict, PD_Key) ->
    true = ?TM:is_dict(Dict_Type, Dict),
    0 = ?TM:size(Dict),
    put(PD_Key, [Dict_Type | get(PD_Key)]),
    true.

%% Operations on something that is not a dictionary.
%% This is not very robust because we don't yet have good OTP is_dict tests.
-spec check_invalid(config()) -> ok.
check_invalid(_Config) ->
    ct:log("Test non-dictionaries return not_a_dict for epode_dict:size(Thing)"),
    %% Arbitrary lists are deemed to be orddicts.
    Bad_Dicts = [27, {some, random, tuple}, [{a,b,c}], <<>>],
    Expected = lists:duplicate(length(Bad_Dicts), not_a_dict),
    Expected = [not_a_dict = ?TM:size(Item) || Item <- Bad_Dicts],
    ct:comment("Tested: ~p", [Bad_Dicts]),
    ok.
