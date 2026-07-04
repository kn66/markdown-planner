# markdown-planner

`markdown-planner` is a Steel/Scheme package for org-like Markdown work in Helix.
It focuses on the smaller feature set requested here:

- TODO state toggling for Markdown headings, list items, and task checkboxes
- TODO agenda buffers that scan Markdown files
- Markdown-only keybindings through Steel extension keymaps
- Markdown hierarchy support through subtree selection, outline buffers, and a `folds.scm` query

## Commands

Expose these from your `helix.scm`:

```scheme
(require (only-in "/home/nobu43/src/helix/markdown-planner/helix.scm"
                  markdown-planner-toggle-todo
                  *markdown-planner-capture-file*
                  markdown-planner-capture-default-file
                  markdown-planner-capture-file
                  markdown-planner-set-capture-file!
                  markdown-planner-clear-capture-file!
                  *markdown-planner-archive-file*
                  markdown-planner-archive-default-file
                  markdown-planner-archive-file
                  markdown-planner-set-archive-file!
                  markdown-planner-clear-archive-file!
                  markdown-planner-archive-task
                  markdown-planner-capture-task
                  markdown-planner-capture-task-to-file
                  markdown-planner-capture-scheduled-task
                  markdown-planner-capture-scheduled-task-to-file
                  markdown-planner-insert-schedule
                  markdown-planner-agenda
                  markdown-planner-agenda-all
                  markdown-planner-outline
                  markdown-planner-select-subtree
                  markdown-planner-activate
                  markdown-planner-install-keybindings
                  markdown-planner-install-folds))

(provide markdown-planner-toggle-todo
         *markdown-planner-capture-file*
         markdown-planner-capture-default-file
         markdown-planner-capture-file
         markdown-planner-set-capture-file!
         markdown-planner-clear-capture-file!
         *markdown-planner-archive-file*
         markdown-planner-archive-default-file
         markdown-planner-archive-file
         markdown-planner-set-archive-file!
         markdown-planner-clear-archive-file!
         markdown-planner-archive-task
         markdown-planner-capture-task
         markdown-planner-capture-task-to-file
         markdown-planner-capture-scheduled-task
         markdown-planner-capture-scheduled-task-to-file
         markdown-planner-insert-schedule
         markdown-planner-agenda
         markdown-planner-agenda-all
         markdown-planner-outline
         markdown-planner-select-subtree
         markdown-planner-activate
         markdown-planner-install-keybindings
         markdown-planner-install-folds)
```

Then reload the Steel config or restart Helix.

Available commands:

```text
:markdown-planner-toggle-todo
:markdown-planner-capture-file
:markdown-planner-set-capture-file!
:markdown-planner-clear-capture-file!
:markdown-planner-archive-file
:markdown-planner-set-archive-file!
:markdown-planner-clear-archive-file!
:markdown-planner-archive-task
:markdown-planner-capture-task
:markdown-planner-capture-task-to-file
:markdown-planner-capture-scheduled-task
:markdown-planner-capture-scheduled-task-to-file
:markdown-planner-insert-schedule
:markdown-planner-agenda
:markdown-planner-agenda-all
:markdown-planner-outline
:markdown-planner-select-subtree
:markdown-planner-activate
:markdown-planner-install-keybindings
:markdown-planner-install-folds
```

## Markdown Keybindings

The plugin installs extension-specific Steel keymaps for `md`, `markdown`,
`mdown`, `mkd`, and `mkdn` files when `helix.scm` is loaded. The bindings
inherit the current global keymap. You can rerun
`:markdown-planner-install-keybindings` if you change global keybindings later.
The direct bindings follow org/markdown/outline editing conventions where
possible: `S-right` and `RET` act on the current item, `TAB` selects the
current subtree, and `z` keys are used for outline actions.

The installed normal-mode bindings are:

```text
S-right      toggle TODO/DONE
RET          toggle TODO/DONE
TAB          select current subtree
S-TAB        open outline
za           select current subtree
zc           select current subtree
zo           open outline
zO           open outline
zM           open outline
zR           open outline
<space> m t  toggle TODO/DONE
<space> m c  capture task
<space> m C  capture scheduled task
<space> m s  insert schedule from calendar
<space> m a  agenda
<space> m A  agenda including DONE/CANCELED
<space> m o  outline
<space> m h  select subtree
<space> m x  archive task
```

## TODO Syntax

The parser recognizes org-style states in headings and lists:

```markdown
# TODO Write package
## NEXT Add README
- WAIT blocked item
1. DONE finished numbered item
```

It also recognizes Markdown task checkboxes:

```markdown
- [ ] unchecked task
- [x] finished task
- [-] waiting task
```

`:markdown-planner-toggle-todo` cycles:

```text
plain -> TODO -> DONE -> TODO
[ ] or [-] -> [x] -> [ ]
```

Open states are `TODO`, `NEXT`, `DOING`, `WAIT`, `WAITING`, and `HOLD`.
Done states are `DONE`, `CANCELED`, and `CANCELLED`.

## Capture And Schedule

`:markdown-planner-capture-task` appends a Markdown task to the active capture file.
By default this is centralized under the Helix Steel config directory:

```text
<helix-config>/markdown-planner/tasks.md
```

For example:

```text
:markdown-planner-capture-task Write release notes
```

This writes:

```markdown
- [ ] Write release notes
```

With no arguments, `:markdown-planner-agenda` scans that same active capture file, so
captured tasks are reflected automatically.

Use a custom central file from your Steel config:

```scheme
(markdown-planner-set-capture-file! "/home/me/notes/tasks.md")
```

Relative custom paths are resolved from the Helix Steel config directory:

```scheme
(markdown-planner-set-capture-file! "org/tasks.md")
```

Reset to the default central file with:

```scheme
(markdown-planner-clear-capture-file!)
```

Use a specific file with:

```text
:markdown-planner-capture-task-to-file notes/tasks.md Write release notes
```

Scheduled captures use an org-style schedule token. With no schedule argument,
the command opens a calendar picker with today's date selected by default:

```text
:markdown-planner-capture-scheduled-task Write release notes
```

Select a date and the task is written as:

```markdown
- [ ] Write release notes SCHEDULED: <2026-07-05>
```

In the picker, arrow keys move by day or week, PageUp/PageDown moves by month,
Home returns to today, Enter selects, and Esc closes.

You can still pass an explicit date or date/time:

```text
:markdown-planner-capture-scheduled-task "2026-07-05 09:00" Write release notes
```

This writes:

```markdown
- [ ] Write release notes SCHEDULED: <2026-07-05 09:00>
```

`:markdown-planner-insert-schedule` inserts or replaces a schedule token on the
current line. With no arguments it opens the same calendar picker, defaulting to
today's date. You can also pass an explicit date or date/time:

```text
:markdown-planner-insert-schedule 2026-07-05 09:00
```

Agenda output includes scheduled tokens and sorts scheduled items before
unscheduled items.

## Archive

`:markdown-planner-archive-task` moves the current Markdown heading/list subtree to the
active archive file. By default this is centralized next to the capture file:

```text
<helix-config>/markdown-planner/archive.md
```

The command removes the subtree from the current buffer and appends the original
Markdown block to the archive file. It also writes the source file immediately
so the task is not left duplicated if you close Helix without a later save.

Use a custom archive file from your Steel config:

```scheme
(markdown-planner-set-archive-file! "/home/me/notes/archive.md")
```

Relative custom archive paths are resolved from the Helix Steel config directory:

```scheme
(markdown-planner-set-archive-file! "org/archive.md")
```

Reset to the default archive file with:

```scheme
(markdown-planner-clear-archive-file!)
```

## Hierarchy And Folding

`:markdown-planner-select-subtree` selects the current Markdown subtree:

- On a list item, it selects the nested list item block.
- Elsewhere, it selects the nearest heading section.

`:markdown-planner-outline` opens a scratch buffer with a heading outline.

`:markdown-planner-install-folds` writes this tree-sitter query to your Helix config runtime:

```text
<helix-config>/runtime/queries/markdown/folds.scm
```

The package also keeps the source query at:

```text
queries/markdown/folds.scm
```

## Agenda

`:markdown-planner-agenda` scans the active capture file for unfinished TODOs by default.
Pass roots to scan specific files or directories:

```text
:markdown-planner-agenda notes project/docs
```

`:markdown-planner-agenda-all` includes finished items.

## Tests

Run the pure Scheme tests with:

```sh
cd /home/nobu43/src/helix/markdown-planner
STEEL_HOME=/home/nobu43/src/helix/markdown-planner steel markdown-planner-test.scm
```
