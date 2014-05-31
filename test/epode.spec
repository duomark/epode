%%-*- mode: erlang -*-
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

{alias, epode, "./epode/"}.
{include, ["../include"]}.
{logdir, "./epode/logs/"}.
{cover, "./epode.coverspec"}.
{suites, epode, [
                 epode_dict_SUITE
                ]}.
