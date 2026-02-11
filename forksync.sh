#!/bin/sh
git remote add upstream https://github.com/HDFGroup/hdf5.git
git fetch upstream
#git merge upstream/develop
git rebase -Xtheirs upstream/develop
