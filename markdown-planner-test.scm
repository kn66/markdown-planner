(require "markdown-planner.scm")

(define sample
  (string-append
    "# TODO Project :work:\n"
    "Intro\n"
    "\n"
    "## NEXT Write package\n"
    "- [ ] Add toggle command :code:\n"
    "  - WAIT Nested blocker\n"
    "- [x] Finished item\n"
    "\n"
    "```markdown\n"
    "- [ ] ignored code task\n"
    "```\n"
    "\n"
    "## DONE Old section\n"
    "Text\n"
    "# Later\n"))

(define open-todos (markdown-planner-todos-from-string "sample.md" sample))
(define all-todos (markdown-planner-todos-from-string "sample.md" sample #true))
(define subtree (markdown-planner-subtree-range-from-string sample 4))
(define nested-list-subtree (markdown-planner-subtree-range-from-string sample 5))
(define fold-ranges (markdown-planner-fold-ranges-from-string sample))
(define line-info (markdown-planner-line-range sample 5))
(define char-range (markdown-planner-lines-range->char-range sample 4 7))
(define scheduled-line (markdown-planner-set-line-schedule "- [ ] Call Alice" "2026-07-05 09:00"))
(define rescheduled-line (markdown-planner-set-line-schedule scheduled-line "2026-07-06 10:30"))
(define capture-entry (markdown-planner-capture-entry "Call Alice" "2026-07-05 09:00"))
(define captured-todos (markdown-planner-todos-from-string "tasks.md" capture-entry))
(define archive-block (markdown-planner-lines-range->string sample 4 6))
(define archive-entry (markdown-planner-archive-entry archive-block))
(define delete-range (markdown-planner-lines-range->delete-char-range sample 4 6))
(define sample-date (markdown-planner-string->date "2026-07-04"))
(define sample-date-next-day (markdown-planner-date-add-days sample-date 1))
(define sample-date-next-month (markdown-planner-date-add-months sample-date 1))
(define sample-calendar-cells (markdown-planner-calendar-cells 2026 7))
(define after-delete
  (string-append (substring sample 0 (car delete-range))
                 (substring sample (cdr delete-range) (string-length sample))))

(define (check name condition)
  (unless condition
    (error (string-append "check failed: " name))))

(check "open todo count" (= (length open-todos) 4))
(check "all todo count" (= (length all-todos) 6))
(check "first state" (string=? (hash-ref (car open-todos) 'state) "TODO"))
(check "first title" (string=? (hash-ref (car open-todos) 'title) "Project"))
(check "second state" (string=? (hash-ref (cadr open-todos) 'state) "NEXT"))
(check "checkbox kind" (equal? (hash-ref (caddr open-todos) 'kind) 'checkbox))
(check "code block ignored" (not (string-contains? (markdown-planner-format-todos open-todos) "ignored code task")))

(check "plain heading toggle" (string=? (markdown-planner-toggle-line "# Heading") "# TODO Heading"))
(check "todo heading toggle" (string=? (markdown-planner-toggle-line "# TODO Heading") "# DONE Heading"))
(check "done heading toggle" (string=? (markdown-planner-toggle-line "# DONE Heading") "# TODO Heading"))
(check "checkbox on" (string=? (markdown-planner-toggle-line "- [ ] Task") "- [x] Task"))
(check "checkbox off" (string=? (markdown-planner-toggle-line "- [x] Task") "- [ ] Task"))
(check "stateful list toggle" (string=? (markdown-planner-toggle-line "  - WAIT Blocked") "  - DONE Blocked"))

(check "heading subtree start" (= (car subtree) 4))
(check "heading subtree end" (= (cdr subtree) 12))
(check "list subtree start" (= (car nested-list-subtree) 5))
(check "list subtree end" (= (cdr nested-list-subtree) 6))
(check "fold ranges" (> (length fold-ranges) 1))
(check "line lookup" (string=? (hash-ref line-info 'line) "- [ ] Add toggle command :code:"))
(check "char range" (< (car char-range) (cdr char-range)))
(check "outline" (string-contains? (markdown-planner-outline-from-string sample) "NEXT Write package"))
(check "schedule insert" (string=? scheduled-line "- [ ] Call Alice SCHEDULED: <2026-07-05 09:00>"))
(check "schedule replace" (string=? rescheduled-line "- [ ] Call Alice SCHEDULED: <2026-07-06 10:30>"))
(check "capture entry" (string=? capture-entry "- [ ] Call Alice SCHEDULED: <2026-07-05 09:00>\n"))
(check "capture agenda count" (= (length captured-todos) 1))
(check "capture scheduled agenda" (string=? (hash-ref (car captured-todos) 'scheduled) "<2026-07-05 09:00>"))
(check "capture agenda format" (string-contains? (markdown-planner-format-todos captured-todos) "SCHEDULED: <2026-07-05 09:00>"))
(check "archive block" (string-contains? archive-block "NEXT Write package"))
(check "archive entry" (ends-with? archive-entry "\n\n"))
(check "archive delete removes block" (not (string-contains? after-delete "NEXT Write package")))
(check "archive delete keeps following task" (string-contains? after-delete "Finished item"))
(check "date parse" (equal? sample-date '(2026 7 4)))
(check "date format" (string=? (markdown-planner-date->string sample-date) "2026-07-04"))
(check "date add day" (string=? (markdown-planner-date->string sample-date-next-day) "2026-07-05"))
(check "date add month" (string=? (markdown-planner-date->string sample-date-next-month) "2026-08-04"))
(check "weekday saturday" (= (markdown-planner-weekday sample-date) 6))
(check "calendar leading blanks" (not (list-ref sample-calendar-cells 2)))
(check "calendar first day" (equal? (list-ref sample-calendar-cells 3) '(2026 7 1)))
(check "calendar selected week" (equal? (list-ref sample-calendar-cells 6) '(2026 7 4)))
(check "date schedule-like" (markdown-planner-schedule-like? "2026-07-05"))
(check "timestamp schedule-like" (markdown-planner-schedule-like? "<2026-07-05 09:00>"))
(check "title not schedule-like" (not (markdown-planner-schedule-like? "Write release notes")))

(displayln "markdown-planner tests passed")
