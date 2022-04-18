# Build PostgREST for ARM architectures

FROM ubuntu:focal as postgrest-build

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y git build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 llvm libnuma-dev zlib1g-dev libpq-dev jq gcc \
    && apt-get clean

# Install ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

# Install cabal
RUN bash -c "ghcup upgrade"
RUN bash -c "ghcup install cabal 3.4.0.0"
RUN bash -c "ghcup set cabal 3.4.0.0"

# Install GHC
RUN bash -c "ghcup install ghc 8.10.7"
RUN bash -c "ghcup set ghc 8.10.7"

# Update Path to include Cabal and GHC exports
RUN bash -c "echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc"
RUN bash -c "echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc"
RUN bash -c "source $HOME/.bashrc"

# Clone the repository
RUN git clone https://github.com/PostgREST/postgrest.git /postgrest
WORKDIR /postgrest
RUN cabal v2-update && cabal v2-build

# Arguments are declared here to save the above installation in cache
ARG PGRST_GITHUB_COMMIT

RUN git pull origin main \
    && git checkout $PGRST_GITHUB_COMMIT

# Build PostgREST
RUN mkdir -p /build-export
RUN cabal v2-update && cabal v2-build
RUN PGRST_BIN=$(cabal exec which postgrest | tail -1) \
    && mv $PGRST_BIN /build-export



# Simple image to generate the PostgREST binaries

FROM scratch as postgrest-bin

COPY --from=postgrest-build /build-export /



# PostgREST docker hub image

FROM ubuntu:focal AS postgrest

RUN apt-get update -y
RUN apt install libpq-dev zlib1g-dev jq gcc libnuma-dev -y
RUN apt-get clean

COPY --from=postgrest-bin postgrest /usr/bin/postgrest

EXPOSE 3000

USER 1000

CMD postgrest
