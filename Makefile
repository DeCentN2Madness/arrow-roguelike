CWD     := $(shell pwd)
ARROWPATH := $(CWD)
STACK   := stack
BUILD   := $(STACK) build
COMMIT  := commit.hs
ARROW  := $(STACK) exec Arrow

GIT       := git
GITDIFF   := $(GIT) diff
GITLOG    := $(GIT) log
GITSTATUS := $(GIT) status
GITPULL   := $(GIT) pull
GITPUSH   := $(GIT) push

all: build arrow
.PHONY: all

build:
	$(BUILD)

commit:
	$(COMMIT)

arrow:
	$(ARROW)

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
