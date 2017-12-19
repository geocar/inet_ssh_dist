PROJECT = inet_ssh_dist
PROJECT_DESCRIPTION = SSH distribution for Erlang
PROJECT_VERSION = 0.1.0

-include local.Makefile

ifeq ($(MAKE_VERSION),$(filter $(MAKE_VERSION),3.81 3.82))
all %::; echo "using gnu make" >&2; gmake "$@"
else
include erlang.mk
endif
