SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >

ifdef PROGRAMFILES
DOXYGEN=C:/Program\ Files/doxygen/bin/doxygen.exe
else
DOXYGEN=doxygen
endif

build:
> $(DOXYGEN) Doxyfile
