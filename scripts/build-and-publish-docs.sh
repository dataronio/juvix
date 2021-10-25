#!/bin/bash
# script used only during CI execution.

for folder in library/*; do
    if [ -d "$folder" ] && [ "$folder" != "library/Backends" ] && [ "$folder" != "library/Playground" ] && [ "$folder" != "library/Test" ]; then
        echo "Starting ${folder}..."
        cd "$folder"
	    stack haddock &> /dev/null
	    cd -
        echo "Done ${folder}."
    fi
done

for folder in library/Backends/*; do
    if [ -d "$folder" ]; then
        echo "Starting ${folder}..."
        cd "$folder"
	    stack haddock &> /dev/null
	    cd -
        echo "Done ${folder}."
    fi
done

for folder in library/Playground/*; do
    if [ -d "$folder" ]; then
        echo "Starting ${folder}..."
        cd "$folder"
	    stack haddock &> /dev/null
	    cd -
        echo "Done ${folder}."
    fi
done

for folder in library/Test/*; do
    if [ -d "$folder" ]; then
        echo "Starting ${folder}..."
        cd "$folder"
	    stack haddock &> /dev/null
	    cd -
        echo "Done ${folder}."
    fi
done

PUSH_URL="https://${GITHUB_TOKEN}@github.com/heliaxdev/juvix-docs.git"

if [ -n "$DRONE_PULL_REQUEST" ]; then 
    exit 0; 
fi

mkdir -p /tmp/docs

for folder in library/*; do
    if [ -d "$folder" ] && [ "$folder" != "library/Backends" ] && [ "$folder" != "library/Playground" ] && [ "$folder" != "library/Test" ]; then
        DIR_NAME=$(ls ${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html)
        cp -r "${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html/${DIR_NAME}" "/tmp/docs"
        echo "Done ${folder}-${DIR_NAME}"
    fi
done

for folder in library/Backends/*; do
    if [ -d "$folder" ]; then
        DIR_NAME=$(ls ${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html)
        cp -r "${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html/${DIR_NAME}" "/tmp/docs"
        echo "Done ${folder}-${DIR_NAME}"
    fi
done


for folder in library/Playground/*; do
    if [ -d "$folder" ]; then
        DIR_NAME=$(ls ${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html)
        cp -r "${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html/${DIR_NAME}" "/tmp/docs"
        echo "Done ${folder}-${DIR_NAME}"
    fi
done

for folder in library/Test/*; do
    if [ -d "$folder" ]; then
        DIR_NAME=$(ls ${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html)
        cp -r "${folder}/.stack-work/dist/x86_64-linux/Cabal-3.2.1.0/doc/html/${DIR_NAME}" "/tmp/docs"
        echo "Done ${folder}-${DIR_NAME}"
    fi
done

cd /tmp

git clone $PUSH_URL --branch gh-pages --single-branch juvix-docs && cd juvix-docs
git config user.name "JuvixBot"
git config user.email "gianmarco@heliax.dev"

rm -rf juvix
mkdir -p juvix
mv /tmp/docs/* juvix

if [ -z "$(git status --porcelain)" ]; 
then 
    echo "No changes to commit." && exit 0
fi

git add -A
git commit -m "[docs]: update commit ${DRONE_COMMIT}"
git push --set-upstream origin gh-pages