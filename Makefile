.PHONY: clean all

TOP = $(PWD)
SRC  = $(PWD)/src
EBIN = $(PWD)/ebin
Q_DATA = $(PWD)/data
ERLC = erlc

ERLC_FLAGS = +native +debug_info
ERLC_MACROS = -DQ_DATA=\"$(Q_DATA)\"

default: algo_target color_target src_target

#algo_target: $(ALGO_MODULES:%=$(EBIN)/%.beam)
algo_target: erlang-algorithms/src/*.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

color_target: erlang-color/src/*.erl
	$(ERLC) $(ERLC_FLAGS) -I erlang-color/include -o $(EBIN) $<

#src_target: $(SRC_MODULES:%=$(EBIN)/%.beam)
src_target: $(SRC)/*.erl
	$(ERLC) $(ERLC_FLAGS) $(ERLC_MACROS) -I $(TOP)/include -o $(EBIN) $(SRC)/*.erl

#$(EBIN)/%.beam: $(SRC)/%.erl
#	$(ERLC) $(ERLC_FLAGS) $(ERLC_MACROS) -o $(EBIN) $<

clean:
	$(RM) $(EBIN)/*.beam

shell:
	erl -pa $(EBIN)
