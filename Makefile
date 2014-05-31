PROJECT = epode

TEST_DEPS = proper
dep_proper = pkg://proper v1.1

DEPS = repdis vbisect
dep_repdis  = https://github.com/duomark/repdis   0.1.2
dep_vbisect = https://github.com/jaynel/vbisect   0.1.1

TEST_ERLC_DEFAULT_OPTS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
TEST_ERLC_OPTS = -I include/ $(TEST_ERLC_DEFAULT_OPTS)
CT_OPTS = -cover test/epode.coverspec
CT_SUITES = epode_dict

include erlang.mk
