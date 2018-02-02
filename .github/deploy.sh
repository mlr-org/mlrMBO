#!/bin/bash

R --no-save <<< 'devtools::install(); pkgdown::build_site()'
git checkout master
export TRAVIS_COMMIT_MSG="$(git log --format=%B --no-merges -n 1)"
git config --global user.name "Travis CI"
git config --global user.email "$COMMIT_AUTHOR_EMAIL"
git config credential.helper "store --file=.git/credentials"
echo "https://${GH_TOKEN}:@github.com" >> .git/credentials
git config push.default matching
git add --force man/*
git add --force README.md
git add --force docs/*
git rm -r $(find . -type d -name '*_cache')
git commit man DESCRIPTION NAMESPACE README.md docs -m "update auto-generated documentation [ci skip]" -m "$TRAVIS_COMMIT_MSG" || true
git push
