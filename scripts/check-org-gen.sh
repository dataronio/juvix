#!/bin/sh

git status docs/Code --porcelain

PATH=~/.roswell/bin:$PATH make org-gen

if [ -z "$(git status docs/Code --porcelain)" ]; then
  exit 0
else
  git status docs/Code --porcelain
  git diff docs/Code
  exit 1
fi