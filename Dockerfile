#
# This Dockerfile builds OpenMemex to run in a docker container.
#
# The container will serve http on container port 3000, and stores user data in a volume at /data.
#
# Dockerfile ARGs enable customizing aspects of the resulting image directly from the cli `docker build ...` step:
#
# --build-arg LIBTORCH_VERSION=<value>
# --build-arg LIBTOKENIZERS_VERSION=<value>
#

#
# Install and setup runtime dependencies
#
FROM ubuntu:20.04 as base

# Install apt packages, clearing package cache in same layer
ARG DEBIAN_FRONTEND=noninteractive
RUN apt update -qq \
	&& apt -y install --no-install-recommends curl wget imagemagick libva-dev snapd chromium-browser libtinfo-dev neovim ripgrep unzip ca-certificates \
	&& rm -rf /var/cache/apt/lists

# Install libtorch, removing the package download in the same layer
ARG LIBTORCH_VERSION=1.9.0+cpu-1
RUN wget -q -O libtorch.deb -q https://github.com/hasktorch/libtorch-binary-for-ci/releases/download/apt/libtorch_${LIBTORCH_VERSION}_amd64.deb \
	&& dpkg -i libtorch.deb \
	&& rm libtorch.deb

#
# Install and setup build & dev dependencies
#
FROM base as build

# Install haskell
# TODO: Install a specific version (or range) of haskell stack, specified by an ARG. (e.g. `ARG STACK_VERSION=xyz`)
ENV PATH="/root/.local/bin:/root/.stack/bin:$PATH"
RUN curl -sSL https://get.haskellstack.org/ | sh

# Install rust
# TODO: Install a specific version (or range) of rust, specified by an ARG. (e.g. `ARG RUST_VERSION=abc`)
ENV PATH="/root/.cargo/bin:$PATH"
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
	&& curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Print out selected & installed versions of dev tools
RUN cargo --version; wasm-pack --version; stack --version

#
# Build Rust Frontend
#
FROM build as build-rust
WORKDIR /src

# Dummy build to cache dep builds -- only rebuilds when Cargo.toml changes
# RUN USER=root cargo new --lib frontend
# ADD ./frontend/Cargo.toml ./frontend/Makefile ./frontend/
# RUN cd ./frontend && make build

# Copy all app files over and build, using cache from the previous step
ADD ./frontend ./frontend/
RUN cd ./frontend && make build

#
# Build Haskell Backend
#
FROM build as build-haskell
WORKDIR /app

# Install libtokenizers, and remove download file in the same step
# ARG LIBTOKENIZERS_VERSION=libtokenizers-v0.1
# RUN wget -q -O libtokenizers-linux.zip https://github.com/hasktorch/tokenizers/releases/download/$LIBTOKENIZERS_VERSION/libtokenizers-linux.zip \
#	&& mkdir -p ./deps/tokenizers \
#	&& unzip -p libtokenizers-linux.zip libtokenizers/lib/libtokenizers_haskell.so >./deps/tokenizers/libtokenizers_haskell.so \
#	&& rm libtokenizers-linux.zip

# Copy over all haskell app files
# TODO: Move haskell files into a dir that is a sibling to the rust dir (./frontend) so this can be done in 1 command
COPY ./frontend ./frontend
COPY ./cli ./cli
COPY ./crawler ./crawler
COPY ./deps ./deps
COPY ./experimental ./experimental
COPY ./server ./server
COPY ./shared ./shared
COPY ./stack.yaml ./Setup.hs ./package.json ./openmemex.cabal ./README.md ./LICENSE ./

# Run stack build; this seems to download and install and compile ghc every time
# TODO: figure out how to move ghc setup into the 'base' layer
RUN stack setup && stack build openmemex:server --ghc-options="-O2"

#
# Package together the outputs from both the rust and haskell stages
#
# Having the previous stages start from the 'build' stage indpendently means that either part of the projct that has
# been built before will build instantly, by resolving directly from the docker cache which is keyed on on the hash
# of all the input files.
#
# TODO: Create the final / output stage from the 'base' stage, omitting dev dependencies. This should be possible
# since everything is statically compiled.
FROM build-haskell as final

COPY --from=build-rust /src/frontend/static /app/static
ADD startup.sh /app
RUN chmod +x /app/startup.sh
RUN find

EXPOSE 3000
# TODO: Adjust the backend to accept a parameter for the data storage dir, and set it to /data in startup.sh.
# Using a volume will automatically persist data for docker users by default, clearly communicates and simplifies
# customization of the storage location, enables easy version upgrades by attaching the data volume from the previous
# container in a new container running the newer-version image, and facilitates compact backups of user data.
VOLUME /data

CMD ["/app/startup.sh"]
