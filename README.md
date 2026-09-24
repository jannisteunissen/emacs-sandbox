# sandbox-tools

Run [gptel](https://github.com/karthink/gptel/) tools or other emacs commands
in a Linux [bubblewrap](https://github.com/containers/bubblewrap) (`bwrap`)
sandbox. Writes to the project are kept in an
[overlay](https://en.wikipedia.org/wiki/OverlayFS) until you review and
selectively apply them to the host file system.

## Requirements

- `Emacs 29` or later.
- `Linux >= 5.11` with user namespaces and overlayfs available
- `bubblewrap` 0.9 or later, with overlayfs support
- `rsync` for copying from/to the sandbox
- `python3` for the `read_file` and `edit_file` tools in the sandbox
- [gptel](https://github.com/karthink/gptel/) to use the included model tools

## Installation

Place this directory somewhere and load it, for example:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/sandbox-tools")
(require 'sandbox-tools)
```

If gptel is available, loading `sandbox-tools` will register `run_command`,
`read_file`, `write_file`, and `edit_file` with gptel under the `sandbox` category (see
below for details).

To bind the menu to a key use for example:

```elisp
(keymap-global-set "C-c s" #'sandbox-tools-menu)
```

## How it works

### Project and scratch directories

The project root is found from the `default-directory` of the current file:
the nearest parent folder with a `.git` entry is looked up first; otherwise
Emacs' current `project.el` root is used, and if this is not available
`default-directory` itself is used. The path is resolved through symlinks.

For each project, the module creates directories under
`sandbox-tools-scratch-dir`, whose default is `~/.emacs.d/sandbox/`. A
project's directory name combines its basename and a hash of its path. The
main entries are:

- `upper/`: the sandbox's pending changes. The project is mounted at
  `/workspace` as an overlay. A file the sandbox creates or edits appears in
  `upper/` at the same relative path. The real project is never touched.
  Changes in `upper/` can be copied back through the `sandbox-tools-apply`
  procedure described below.
- `staging/`: a snapshot used for reviewing and applying changes.
- `home/` and `tmp/`: sandbox `/home/sandbox` and `/tmp`. These persist across
  tool calls for that project.
- `work/`: private scratch space required for overlayfs functionality, does
  not need to be inspected or modified.

`sandbox-tools-reset` removes `upper/`, `work/`, and `staging/`, but leaves the
sandbox HOME and `/tmp` in place.

### Sandbox permissions and environment

Each tool command starts a fresh bubblewrap sandbox. The sandbox uses isolated
namespaces and clears the environment, see `sandbox-tools--base-args` for
details. The sandbox has a minimal `/dev` and `/proc` and read-only access to
the host system paths in `sandbox-tools-ro-binds` (if they are present). A
minimal default environment is defined in `sandbox-tools-env`, while the
variables in `sandbox-tools-preserve-env` are preserved from the host shell.
The project files are at `/workspace`. Writes appear in the overlay rather
than the host project.

Network access is disabled by default. `run_command` can request the host
network by passing `network: true`, which is only granted after approval.
Approving shares the host network, including access to localhost.

### Adding writable mounts on host

By default there are no writable host mounts. To add one, configure
`sandbox-tools-cache-binds` as `(HOST-SRC . SANDBOX-DST)` pairs, for example:

```elisp
(setq sandbox-tools-cache-binds
      '(("~/.cache/some-tool" . "/home/sandbox/.cache/some-tool")))
```

These mounts are writable directly on the host and bypass overlay review and
reset, so use with care!

## gptel tools

Tool calls do not ask for confirmation, except for network acces.

- **`run_command`** runs a Bash command from `/workspace`. Each call has a
  fresh shell and environment; chain commands when you need to retain a
  directory change (`cd subdir && ...`). Only one command may run at a time
  for a given project. Standard error is combined with standard output. The
  captured output is capped and returned as a head-and-tail excerpt. The
  command is timed out after `sandbox-tools-timeout` seconds (120 by default).
  Prefer bounded inspection commands such as `grep`, `head`, `sed -n`, `wc`,
  or `git ls-files` rather than dumping large files.
- **`read_file`** returns a text file's lines verbatim (optionally from
  `offset`, at most `limit` lines). Output is cut at a line boundary after
  `sandbox-tools-max-read-output` bytes (60000 by default), with a note giving
  the offset to continue from. Requires Python 3 in the sandbox.
- **`write_file`** creates or overwrites a file in the project overlay. Supply
  the entire file contents; it is not a patch operation. Its path must be
  relative to the project and remain within the project root.
- **`edit_file`** replaces a literal block in a file in the overlay. Read the
  file first and provide a unique, verbatim `old` block with useful context;
  `new` is the complete replacement (the empty string deletes the block).
  It can make a whitespace/indentation-tolerant match if the exact block is
  not found. By default the match must be unique; `replace_all: true` replaces
  every match. On failure no edit is made; on success the tool returns a diff.
  This tool requires Python 3 in the sandbox.


## Reviewing, applying, and discarding changes

The following commands can be accessed through the `sandbox-tools-menu`:

- `sandbox-tools-diff` displays changes between sandbox and host
- `sandbox-tools-apply` to copy the selected items to the host project
- `sandbox-tools-reset` to discard the changes in the sandbox
- `sandbox-tools-status` reports whether a command is running and whether there are changes
- `sandbox-tools-interrupt` kills running command processes for the current project
- `sandbox-tools-command` run a custom command in the sandbox

## Configuration

Options are in the `sandbox-tools` customization group.
