git config user.email "gianmarco@heliax.dev"
git config user.name "Drone CI"

if [ -z "$(git status --porcelain)" ]; then 
    echo "No changes to commit."
    exit 0
fi

REMOTE=$(git remote get-url origin | cut -c 9-)
PUSH_URL="https://${GITHUB_TOKEN}@${REMOTE}"

git fetch --all
CHECKOUT_OUTPUT=$(git checkout $DRONE_SOURCE_BRANCH  2>&1)

if [ $? -ne 0 ]; then
    CHECKOUT_PATHSPEC_ERROR="error: pathspec '$DRONE_SOURCE_BRANCH' did not match any file(s) known to git"
    if [[ "$CHECKOUT_OUTPUT" == "$CHECKOUT_PATHSPEC_ERROR" ]];
    then
        echo "Can't checkout $DRONE_SOURCE_BRANCH, because $DRONE_SOURCE_BRANCH is probably not part of $REMOTE. You should manually run org-gen and formatting."
    else
        echo "Can't checkout $DRONE_SOURCE_BRANCH. Unknown reason. Manually run org-gen and formatting."
    fi
    exit 1
fi

git remote set-url origin $PUSH_URL

git add -A
git status
git commit -m "[ci] changes from CI"

git push

exit $?