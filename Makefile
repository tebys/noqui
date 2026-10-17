# Setup for macOS (Homebrew) and Arch Linux (pacman).
#   make install   system packages + Lua rocks + Claude ACP adapter + config.fnl
#   make check     verify everything is in place
#   make run       start the REPL

UNAME := $(shell uname -s)
LUA_VERSION := $(shell lua -e 'print(_VERSION:match("%d+%.%d+"))' 2>/dev/null)
ROCKS := dkjson luv

ifeq ($(UNAME),Darwin)
SYSTEM_INSTALL = brew install lua luarocks fennel cmake libuv node gh
else ifneq ($(wildcard /etc/arch-release),)
SYSTEM_INSTALL = sudo pacman -S --needed base-devel lua luarocks fennel cmake libuv nodejs npm github-cli
else
SYSTEM_INSTALL = @echo "unsupported system: install lua, luarocks, fennel, cmake, libuv, node and gh by hand, then run: make rocks acp config" && false
endif

.PHONY: install system rocks acp config check run

install: system rocks acp config

system:
	$(SYSTEM_INSTALL)

rocks:
	@test -n "$(LUA_VERSION)" || { echo "lua not found: run 'make system' first"; exit 1; }
	luarocks --lua-version=$(LUA_VERSION) install --local $(ROCKS)

acp:
	npm install -g @agentclientprotocol/claude-agent-acp

config:
	@test -f config.fnl || { cp config.example.fnl config.fnl; echo "created config.fnl: edit it"; }

check:
	@ok=1; \
	for c in fennel lua luarocks node gh curl claude-agent-acp; do \
	  command -v $$c >/dev/null || { echo "missing: $$c"; ok=0; }; \
	done; \
	for r in $(ROCKS); do \
	  luarocks --lua-version=$(LUA_VERSION) list --porcelain 2>/dev/null | grep -qE "^$$r[[:space:]]" || { echo "missing rock: $$r"; ok=0; }; \
	done; \
	test -f config.fnl || { echo "missing: config.fnl (make config)"; ok=0; }; \
	command -v ollama >/dev/null || echo "note: ollama not found (https://ollama.com)"; \
	test $$ok = 1 && echo "all good"

run:
	fennel main.fnl
