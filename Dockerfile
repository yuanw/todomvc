# Use the official Haskell image to create a build artifact.
# https://hub.docker.com/_/haskell/
FROM nixos/nix as builder

# Copy local code to the container image.
WORKDIR /app


RUN nix-env -iA nixpkgs.git
# Build and test our code, then build the “helloworld-haskell-exe” executable.
COPY app app
COPY src src
COPY default.nix .
COPY flake.nix .
COPY flake.lock .
COPY todomvc.cabal .
RUN nix-build

# Use the official Debian slim image for a lean production container.
# https://hub.docker.com/_/debian
# https://docs.docker.com/develop/develop-images/multistage-build/#use-multi-stage-builds

#FROM alpine:3.7 AS cloud-run-hello
# WORKDIR /opt

# Copy the "helloworld-haskell-exe" executable from the builder stage to the production image.

#COPY --from=builder /app/result/ /todomvc

# Run the web service on container startup.

CMD ["/app/result/bin/todomvc-exe"]
