LTAC_TOOLS := \
 ltac_get_all_hyps \
 ltac_hidefalso \
 ltac_mutable_state \
 ltac_noob1 \
 ltac_number_goals \
 ltac_obviously1 \
 ltac_print_verbose \
 ltac_sandbox \
 ltac_utils

# ltac_trace \
# noob2 \

MODULES := \
 $(LTAC_TOOLS) \
 Prouf

GLOBS := $(patsubst %,%.glob,$(MODULES)) \
         $(patsubst %,test_%.glob,$(MODULES))

all: $(GLOBS)

$(GLOBS):

clean:
	rm -f $(GLOBS) $(patsubst %.glob,%.vo,$(GLOBS))

# Dependencies, TODO: should use coqdep
ltac_sandbox.glob: ltac_hidefalso.glob ltac_get_all_hyps.glob
ltac_noob1.glob: ltac_sandbox.glob
ltac_print_verbose.glob: ltac_get_all_hyps.glob
Prouf.glob : $(patsubst %,%.glob,$(LTAC_TOOLS))

test_%.glob test_%.vo: test_%.v %.glob
	coqc $<

%.glob %.vo: %.v
	coqc $<
