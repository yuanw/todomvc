# Use the official Haskell image to create a build artifact.
# https://hub.docker.com/_/haskell/
FROM nixos/nix as builder

# Copy local code to the container image.
WORKDIR /app


RUN nix-env -iA nixpkgs.git
# Build and test our code, then build the “helloworld-haskell-exe” executable.
COPY app app
COPY src src
COPY static static
COPY default.nix .
COPY flake.nix .
COPY flake.lock .
COPY todomvc.cabal .
RUN nix-build
