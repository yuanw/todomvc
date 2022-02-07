set -eu


OCI_ARCHIVE=$(nix-build --no-out-link)
DOCKER_REPOSITORY="docker://gcr.io/$GOOGLE_CLOUD_PROJECT_NAME/$GOOGLE_CLOUD_RUN_SERVICE_NAME:$GITHUB_SHA"

# skopeo login -u _json_key -p "${GCR_DEVOPS_SERVICE_ACCOUNT_KEY}"  gcr.io
skopeo --insecure-policy copy --dest-creds="_json_key:$GCR_DEVOPS_SERVICE_ACCOUNT_KEY" "docker-archive:$OCI_ARCHIVE" "$DOCKER_REPOSITORY"
