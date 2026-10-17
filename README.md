# ñoqui

A small terminal REPL that turns a request into a chain of tool calls, written in [Fennel](https://fennel-lang.org).
A local model (Ollama) plans the steps and picks the tools; Claude, over [ACP](https://agentclientprotocol.com),
is only called for the steps that need writing or reasoning. Simple requests cost cents instead of a full
Claude Code session (see [COSTS.org](COSTS.org)).

```
> get my PRs from the last month -> keep the ones with more than two comments -> count them
```

## Self-development (`:dev`)

The point of ñoqui is that it extends itself. Type `:dev` (optionally `:dev opus`) and describe the tool you
need; a dedicated Claude session, primed with a guide to ñoqui's tool format, writes it for you.

```
> :dev
dev> a tool that lists my open PRs with failing CI
```

- Claude only proposes code. ñoqui compiles it and, if it fails, feeds the error back (up to 3 -configurable- automatic fixes).
- For a modification you see a diff and confirm with `y` before anything is written.
- The tool is saved to `tools/` and hot-loaded at once, so it is usable right away. If it fails to load, the change is rolled back; replaced versions are backed up in `/tmp`.
- Name an existing tool in your message to have its code attached, so Claude can edit it.
- `:fin` goes back to normal mode.

## Features

- `a -> b -> c` chains steps; each step declares what it depends on.
- Tools are single files in `tools/`, defined with `deftool` (compile-time checks) and hot-reloaded, written by hand or with `:dev`.
- Snippets (`snippets.fnl`): fixed plans that skip the planner.
- Worktree mode (`:wt repo ticket`), launching a terminal or Claude in it, and background CI `:watch`.

## Requirements

- Fennel, Lua (5.4+), LuaRocks, plus the rocks `dkjson` and `luv`
- [Ollama](https://ollama.com) with a model such as `qwen3:4b`
- [Claude Code](https://claude.com/claude-code) and the `claude-agent-acp` adapter (`npm i -g @agentclientprotocol/claude-agent-acp`)
- `gh` (authenticated), `curl`, and `JIRA_EMAIL` / `JIRA_TOKEN` for the Jira tools

## Usage

`make install` sets everything up on macOS (Homebrew) and Arch Linux (pacman) except Ollama; `make check` verifies it.

```sh
make install                       # then edit config.fnl: repos, Jira, models
fennel main.fnl                    # REPL
fennel main.fnl --model sonnet     # choose the Claude model
fennel main.fnl "which PRs do I have open?"   # one-shot
```

## License

[MIT](LICENSE)
