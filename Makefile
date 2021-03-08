.ONESHELL:

all: test

# Use install-deps instead of 'install' because usually 'make install' adds a
# binary to the system path and we don't want to confuse users
install-deps:
#	Install ligo/tezos specific system-level dependencies
	sudo scripts/install_native_dependencies.sh
	scripts/install_build_environment.sh # TODO: or scripts/install_opam.sh ?

build-deps:
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
#	Create opam dev switch locally for use with Ligo, add merlin/etc
	if [ ! -d "./_opam" ];
	then scripts/setup_switch.sh;
	fi
	eval $$(opam config env)
#	Install OCaml build dependencies for Ligo
	scripts/install_vendors_deps.sh

build: build-deps
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
#	Build Ligo for local dev use
	scripts/build_ligo_local.sh

test: build
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
	scripts/test_ligo.sh

clean:
	dune clean
	rm -fr _coverage_all _coverage_cli _coverage_ligo

coverage: clean
	BISECT_ENABLE=yes dune runtest --force
	bisect-ppx-report html -o ./_coverage_all --title="LIGO overall test coverage"
	bisect-ppx-report summary --per-file

coverage-ligo: clean
	BISECT_ENABLE=yes dune runtest src/test --force
	bisect-ppx-report html -o ./_coverage_ligo --title="LIGO test coverage"
	bisect-ppx-report summary --per-file

coverage-doc: clean
	BISECT_ENABLE=yes dune build @doc-test --force
	bisect-ppx-report html -o ./_coverage_doc --title="LIGO doc coverage"
	bisect-ppx-report summary --per-file

coverage-cli: clean
	BISECT_ENABLE=yes dune runtest src/bin/expect_tests
	bisect-ppx-report html -o ./_coverage_cli --title="CLI test coverage"
	bisect-ppx-report summary --per-file
