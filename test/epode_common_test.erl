%%%------------------------------------------------------------------------------
%%% @copyright (c) 2014, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com> [http://duomark.com/]
%%% @reference 2014 Development sponsored by TigerText, Inc. [http://tigertext.com/]
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Test wrapper for consistent common_test logging and results comment.
%%% @since 0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(epode_common_test).
-author('jay@duomark.com').

-export([test_count_wrapper/4]).


%% Log and verify results of a single (repeated) PropEr test case
test_count_wrapper(Log_Stmt, PD_Key, Test_Fn, Num_Tests) ->
    Old_PD_Value = put(PD_Key, []),
    ct:log(Log_Stmt, [Num_Tests]),
    Result = Test_Fn(PD_Key, Num_Tests),
    ct:comment("Tested: ~p", [get(PD_Key)]),
    put(PD_Key, Old_PD_Value),
    Result.
