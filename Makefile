CWD     := $(shell pwd)
RANDOMPATH := $(CWD)
STACK   := stack
BUILD   := $(STACK) build
COMMIT  := commit.hs
RANDOM  := $(STACK) exec Main

GIT       := git
GITDIFF   := $(GIT) diff
GITLOG    := $(GIT) log
GITSTATUS := $(GIT) status
GITPULL   := $(GIT) pull
GITPUSH   := $(GIT) push

all: build random
.PHONY: all

build:
	$(BUILD)

commit:
	$(COMMIT)

random:
	$(RANDOM)

diff:
	$(GITDIFF)

log:
	$(GITLOG)

pull:
	$(GITPULL)

run:
	@echo "stack and haskell"

status:
	$(GITSTATUS)

upgrade:
	$(STACK) upgrade

version:
	$(STACK) --version
