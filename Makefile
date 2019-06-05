dev-test:
	scripts/setup_ligo_opam_repository.sh
	opam install -y --build-test --deps-only ./src
