ARG target
FROM ocaml/opam2:${target}

USER root

RUN opam switch 4.07 && eval $(opam env)

COPY scripts /ligo_prebuild/scripts

WORKDIR /ligo_prebuild

# Install required native dependencies
RUN sh scripts/install_native_dependencies.sh

# Add tezos repository
RUN sh scripts/setup_repos.sh

RUN opam update

COPY ligo.opam /ligo_prebuild/ligo.opam
COPY vendors /ligo_prebuild/vendors

# Install opam deps
RUN sh scripts/install_vendors_deps.sh

# Now copy rest of repo (making above steps more cacheable)
COPY . /ligo

WORKDIR /ligo

ARG ci_job_id
ENV CI_JOB_ID=$ci_job_id

# Install ligo
RUN opam install -y .

# Use the ligo binary as a default command
ENTRYPOINT [ "/home/opam/.opam/4.07/bin/ligo" ]
