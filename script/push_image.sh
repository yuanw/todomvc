#! /usr/bin/env nix-shell
#! nix-shell -i bash -p skopeo

set -eu

OCI_ARCHIVE=$(nix-build --no-out-link)
DOCKER_REPOSITORY="docker://gcr.io/${GOOGLE_CLOUD_PROJECT_NAME}/${GOOGLE_CLOUD_RUN_SERVICE_NAME}:${GITHUB_SHA}"

skopeo login -u _json_key -p ${DOCKER_ACCESS_TOKEN}  gcr.io 2>/dev/null
skopeo copy "docker-archive:${OCI_ARCHIVE}" "$DOCKER_REPOSITORY"
