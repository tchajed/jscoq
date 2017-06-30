# Jobs to use
JOBS=8

COQ_VERSION:=v8.7
JSCOQ_BRANCH:=js-worker

JSCOQ_VERSION:=$(COQ_VERSION)

ifdef JSCOQ_BRANCH
JSCOQ_VERSION:=$(JSCOQ_VERSION)-$(JSCOQ_BRANCH)
endif

# Directory where the coq sources are
COQDIR=~/external/coq-$(COQ_VERSION)+32bit/

# Addons to build
ADDONS =
# Woking on coq-8.6:
# ADDONS += ssr-libs coquelicot flocq sf cpdt dsp hott color
# Woking on coq-8.5:
# ADDONS += ssr-libs mtac coquelicot flocq tlc color sf cpdt hott dsp relalg unimath plugin-utils cel mirror-core

# Have in every add-on a make jscoq target? We could certainly
# classify coq_makefile ones.
ADDONS_PATH:=/home/egallego/external/coq

# Custom stdlib for hott
HOTT_COQLIB:=$(ADDONS_PATH)/HoTT/coq/theories/

RELEASE_DIR=~/research/jscoq-builds/
WEB_DIR=~/x80/jscoq-builds/$(JSCOQ_VERSION)/
HOTT_RELEASE=~/x80/rhino-hott/
