#!/usr/bin/env bash

cd "${BASH_SOURCE%/*}" &&
GitSetup/setup-user && echo &&
GitSetup/setup-hooks && echo &&
GitSetup/setup-aliases && echo &&
GitSetup/setup-upstream && echo &&
GitSetup/tips

# Rebase master by default
git config rebase.stat true
git config branch.master.rebase true

# Disable Gerrit hook explicitly so the commit-msg hook will
# not complain even if some gerrit remotes are still configured.
git config hooks.GerritId false

# Record the version of this setup so Scripts/pre-commit can check it.
SetupForDevelopment_VERSION=2
git config hooks.SetupForDevelopment ${SetupForDevelopment_VERSION}
