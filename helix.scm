(require "markdown-planner.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require (prefix-in helix.static. "helix/static.scm"))
(require (prefix-in helix.editor. "helix/editor.scm"))
(require (prefix-in helix.misc. "helix/misc.scm"))
(require (prefix-in helix.components. "helix/components.scm"))
(require (only-in "helix/keymaps.scm"
                  keymap
                  deep-copy-global-keybindings))
(require-builtin helix/core/text as text.)
(require-builtin steel/filesystem)

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

(define *markdown-planner-capture-file* #false)
(define *markdown-planner-archive-file* #false)
(define markdown-planner-markdown-extensions '("md" "markdown" "mdown" "mkd" "mkdn"))

(define (current-buffer-text)
  (let* ([focus (helix.editor.editor-focus)]
         [doc-id (helix.editor.editor->doc-id focus)])
    (text.rope->string (helix.editor.editor->text doc-id))))

(define (current-line-number-1)
  (+ (helix.static.get-current-line-number) 1))

(define (join-command-parts parts)
  (string-join parts " "))

(define (absolute-path? path)
  (starts-with? path "/"))

(define (workspace-path path)
  (if (absolute-path? path)
      path
      (path-join (helix.static.get-helix-cwd) path)))

(define (current-file-path)
  (let ([path (helix.static.cx->current-file)])
    (if path
        path
        (error "current buffer has no file path"))))

(define (file->string path)
  (let* ([port (open-input-file path)]
         [content (read-port-to-string port)])
    (close-port port)
    content))

(define (archive-snapshot path)
  (if (path-exists? path)
      (cons #true (file->string path))
      (cons #false "# Archive\n\n")))

(define (restore-archive! path snapshot)
  (write-file path (cdr snapshot)))

(define (replace-range start end replacement)
  (let ([selection (helix.static.range->selection (helix.static.range start end))])
    (helix.static.set-current-selection-object! selection)
    (if (= start end)
        (helix.static.insert_string replacement)
        (helix.static.replace-selection-with replacement))))

(define (open-scratch name content)
  (helix.new)
  (helix.editor.set-scratch-buffer-name! name)
  (helix.static.insert_string content)
  (helix.goto-line 1))

(define (prompt-input label callback)
  (helix.misc.push-component! (prompt label callback)))

(define (calendar-two-char number)
  (if (< number 10)
      (string-append " " (number->string number))
      (number->string number)))

(define (calendar-current-date)
  (let ([date (markdown-planner-string->date (markdown-planner-current-date-string))])
    (if date
        date
        (error "failed to read current date"))))

(define (calendar-window-area area)
  (let* ([width (min 38 (helix.components.area-width area))]
         [height (min 13 (helix.components.area-height area))]
         [x (+ (helix.components.area-x area)
               (quotient (max 0 (- (helix.components.area-width area) width)) 2))]
         [y (+ (helix.components.area-y area)
               (quotient (max 0 (- (helix.components.area-height area) height)) 2))])
    (helix.components.area x y width height)))

(define (calendar-header date)
  (string-append (number->string (list-ref date 0))
                 "-"
                 (if (< (list-ref date 1) 10)
                     (string-append "0" (number->string (list-ref date 1)))
                     (number->string (list-ref date 1)))
                 "  "
                 (markdown-planner-date->string date)))

(define (calendar-cell-style cell selected-date today-date)
  (cond [(equal? cell selected-date)
         (helix.components.style-with-reversed
          (helix.components.style-with-bold (helix.components.style)))]
        [(equal? cell today-date)
         (helix.components.style-with-bold (helix.components.style))]
        [else (helix.components.style)]))

(define (calendar-render-cell frame base-x base-y cell selected-date today-date index)
  (let* ([row (quotient index 7)]
         [column (modulo index 7)]
         [x (+ base-x (* column 5))]
         [y (+ base-y row)])
    (if cell
        (helix.components.frame-set-string!
         frame
         x
         y
         (calendar-two-char (list-ref cell 2))
         (calendar-cell-style cell selected-date today-date))
        (helix.components.frame-set-string! frame x y "  " (helix.components.style)))))

(define (calendar-render-cells frame x y cells selected-date today-date index)
  (unless (null? cells)
    (calendar-render-cell frame x y (car cells) selected-date today-date index)
    (calendar-render-cells frame x y (cdr cells) selected-date today-date (+ index 1))))

(define (markdown-planner-calendar-render state area frame)
  (let* ([window (calendar-window-area area)]
         [x (helix.components.area-x window)]
         [y (helix.components.area-y window)]
         [selected-date (vector-ref state 0)]
         [today-date (vector-ref state 1)]
         [year (list-ref selected-date 0)]
         [month (list-ref selected-date 1)])
    (helix.components.buffer/clear frame window)
    (helix.components.block/render frame window (helix.components.block))
    (helix.components.frame-set-string!
     frame (+ x 2) (+ y 1) "Schedule" (helix.components.style-with-bold (helix.components.style)))
    (helix.components.frame-set-string!
     frame (+ x 2) (+ y 2) (calendar-header selected-date) (helix.components.style))
    (helix.components.frame-set-string!
     frame (+ x 2) (+ y 4) "Su   Mo   Tu   We   Th   Fr   Sa" (helix.components.style))
    (calendar-render-cells
     frame
     (+ x 2)
     (+ y 5)
     (markdown-planner-calendar-cells year month)
     selected-date
     today-date
     0)
    (helix.components.frame-set-string!
     frame (+ x 2) (+ y 11) "Enter selects, Esc closes" (helix.components.style))))

(define (calendar-set-selected! state date)
  (vector-set! state 0 date))

(define (calendar-confirm! state)
  ((vector-ref state 2) (markdown-planner-date->string (vector-ref state 0))))

(define (markdown-planner-calendar-handle-event state event)
  (cond [(helix.components.key-event-escape? event)
         helix.components.event-result/close]
        [(helix.components.key-event-enter? event)
         (begin
           (calendar-confirm! state)
           helix.components.event-result/close)]
        [(helix.components.key-event-left? event)
         (begin
           (calendar-set-selected! state (markdown-planner-date-add-days (vector-ref state 0) -1))
           helix.components.event-result/consume)]
        [(helix.components.key-event-right? event)
         (begin
           (calendar-set-selected! state (markdown-planner-date-add-days (vector-ref state 0) 1))
           helix.components.event-result/consume)]
        [(helix.components.key-event-up? event)
         (begin
           (calendar-set-selected! state (markdown-planner-date-add-days (vector-ref state 0) -7))
           helix.components.event-result/consume)]
        [(helix.components.key-event-down? event)
         (begin
           (calendar-set-selected! state (markdown-planner-date-add-days (vector-ref state 0) 7))
           helix.components.event-result/consume)]
        [(helix.components.key-event-page-up? event)
         (begin
           (calendar-set-selected! state (markdown-planner-date-add-months (vector-ref state 0) -1))
           helix.components.event-result/consume)]
        [(helix.components.key-event-page-down? event)
         (begin
           (calendar-set-selected! state (markdown-planner-date-add-months (vector-ref state 0) 1))
           helix.components.event-result/consume)]
        [(helix.components.key-event-home? event)
         (begin
           (calendar-set-selected! state (vector-ref state 1))
           helix.components.event-result/consume)]
        [else helix.components.event-result/ignore]))

(define (open-schedule-calendar callback)
  (let* ([today (calendar-current-date)]
         [state (vector today today callback)]
         [component (helix.components.new-component!
                     "markdown-planner-calendar"
                     state
                     markdown-planner-calendar-render
                     (hash "handle_event" markdown-planner-calendar-handle-event))])
    (helix.components.overlaid component)
    (helix.misc.push-component! component)
    "schedule calendar opened"))

;;@doc
;; Toggle TODO/DONE state on the current Markdown heading, list item, or checkbox.
(define (markdown-planner-toggle-todo)
  (let* ([content (current-buffer-text)]
         [line-number (current-line-number-1)]
         [line-info (markdown-planner-line-range content line-number)]
         [line (hash-ref line-info 'line)]
         [replacement (markdown-planner-toggle-line line)])
    (replace-range (hash-ref line-info 'start)
                   (hash-ref line-info 'end)
                   replacement)
    replacement))

(define (capture-task-to-file file title scheduled)
  (markdown-planner-append-capture! file title scheduled)
  (if scheduled
      (string-append "captured scheduled task in " file)
      (string-append "captured task in " file)))

;;@doc
;; Return the default centralized Markdown capture file.
(define (markdown-planner-capture-default-file)
  (path-join (config-root) "markdown-planner" "tasks.md"))

;;@doc
;; Return the active Markdown capture file.
(define (markdown-planner-capture-file)
  (if *markdown-planner-capture-file*
      *markdown-planner-capture-file*
      (markdown-planner-capture-default-file)))

;;@doc
;; Set a custom Markdown capture file. Relative paths are resolved from the
;; Helix Steel config directory.
(define (markdown-planner-set-capture-file! path)
  (set! *markdown-planner-capture-file*
        (if (absolute-path? path)
            path
            (path-join (config-root) path)))
  *markdown-planner-capture-file*)

;;@doc
;; Reset the Markdown capture file to the default centralized file.
(define (markdown-planner-clear-capture-file!)
  (set! *markdown-planner-capture-file* #false)
  (markdown-planner-capture-file))

;;@doc
;; Return the default centralized Markdown archive file.
(define (markdown-planner-archive-default-file)
  (path-join (config-root) "markdown-planner" "archive.md"))

;;@doc
;; Return the active Markdown archive file.
(define (markdown-planner-archive-file)
  (if *markdown-planner-archive-file*
      *markdown-planner-archive-file*
      (markdown-planner-archive-default-file)))

;;@doc
;; Set a custom Markdown archive file. Relative paths are resolved from the
;; Helix Steel config directory.
(define (markdown-planner-set-archive-file! path)
  (set! *markdown-planner-archive-file*
        (if (absolute-path? path)
            path
            (path-join (config-root) path)))
  *markdown-planner-archive-file*)

;;@doc
;; Reset the Markdown archive file to the default centralized file.
(define (markdown-planner-clear-archive-file!)
  (set! *markdown-planner-archive-file* #false)
  (markdown-planner-archive-file))

;;@doc
;; Move the current Markdown heading/list subtree to the active archive file.
(define (markdown-planner-archive-task)
  (let* ([content (current-buffer-text)]
         [source-file (current-file-path)]
         [line-number (current-line-number-1)]
         [line-range (markdown-planner-subtree-range-from-string content line-number)]
         [start-line (car line-range)]
         [end-line (cdr line-range)]
         [block (markdown-planner-lines-range->string content start-line end-line)]
         [delete-range (markdown-planner-lines-range->delete-char-range content start-line end-line)]
         [delete-start (car delete-range)]
         [delete-end (cdr delete-range)]
         [source-after-archive (string-append
                                (substring content 0 delete-start)
                                (substring content delete-end (string-length content)))]
         [archive-file (markdown-planner-archive-file)])
    (let ([snapshot (archive-snapshot archive-file)])
      (markdown-planner-append-archive! archive-file block)
      (with-handler
        (lambda (err)
          (with-handler (lambda (_) #false)
                        (restore-archive! archive-file snapshot))
          (with-handler (lambda (_) #false)
                        (write-file source-file content))
          (error (string-append "archive failed: " (to-string err))))
        (begin
          (write-file source-file source-after-archive)
          (replace-range delete-start delete-end "")
          (string-append "archived lines "
                         (number->string start-line)
                         "-"
                         (number->string end-line)
                         " to "
                         archive-file))))))

;;@doc
;; Capture a Markdown TODO into the active capture file.
;; With no arguments, prompts for the task title.
(define (markdown-planner-capture-task . title-parts)
  (if (null? title-parts)
      (begin
        (prompt-input "Capture task: "
                      (lambda (title)
                        (markdown-planner-capture-task title)))
        "capture prompt opened")
      (capture-task-to-file (markdown-planner-capture-file)
                            (join-command-parts title-parts)
                            #false)))

;;@doc
;; Capture a Markdown TODO into a specific file.
(define (markdown-planner-capture-task-to-file file . title-parts)
  (when (null? title-parts)
    (error "capture title cannot be blank"))
  (capture-task-to-file (workspace-path file) (join-command-parts title-parts) #false))

;;@doc
;; Capture a scheduled Markdown TODO into the active capture file.
;; With no arguments, prompts for the title and schedule.
(define (markdown-planner-capture-scheduled-task . parts)
  (cond [(null? parts)
         (begin
           (prompt-input "Capture task: "
                         (lambda (title)
                           (open-schedule-calendar
                            (lambda (scheduled)
                              (capture-task-to-file
                               (markdown-planner-capture-file)
                               title
                               scheduled)))))
           "scheduled capture prompt opened")]
        [(markdown-planner-schedule-like? (car parts))
         (begin
           (if (null? (cdr parts))
               (begin
                 (prompt-input "Capture task: "
                               (lambda (title)
                                 (capture-task-to-file (markdown-planner-capture-file)
                                                       title
                                                       (car parts))))
                 "capture prompt opened")
               (capture-task-to-file (markdown-planner-capture-file)
                                     (join-command-parts (cdr parts))
                                     (car parts))))]
        [else
         (begin
           (open-schedule-calendar
            (lambda (scheduled)
              (capture-task-to-file (markdown-planner-capture-file)
                                    (join-command-parts parts)
                                    scheduled)))
           "schedule calendar opened")]))

;;@doc
;; Capture a scheduled Markdown TODO into a specific file.
(define (markdown-planner-capture-scheduled-task-to-file file scheduled . title-parts)
  (when (null? title-parts)
    (error "capture title cannot be blank"))
  (capture-task-to-file (workspace-path file) (join-command-parts title-parts) scheduled))

;;@doc
;; Insert or replace `SCHEDULED: <...>` on the current line.
;; With no arguments, prompts for the schedule timestamp.
(define (markdown-planner-insert-schedule . schedule-parts)
  (if (null? schedule-parts)
      (begin
        (open-schedule-calendar
         (lambda (scheduled)
           (markdown-planner-insert-schedule scheduled)))
        "schedule calendar opened")
      (let* ([content (current-buffer-text)]
             [line-number (current-line-number-1)]
             [line-info (markdown-planner-line-range content line-number)]
             [line (hash-ref line-info 'line)]
             [replacement (markdown-planner-set-line-schedule line
                                                    (join-command-parts schedule-parts))])
        (replace-range (hash-ref line-info 'start)
                       (hash-ref line-info 'end)
                       replacement)
        replacement)))

;;@doc
;; Open a scratch agenda for unfinished Markdown TODO items.
;; With no roots, scans the active capture file.
(define (markdown-planner-agenda . roots)
  (let ([scan-roots (if (null? roots)
                        (list (markdown-planner-capture-file))
                        roots)])
    (open-scratch "*markdown-planner-agenda*" (markdown-planner-format-todos-roots scan-roots #false))))

;;@doc
;; Open a scratch agenda including DONE/CANCELED Markdown TODO items.
(define (markdown-planner-agenda-all . roots)
  (let ([scan-roots (if (null? roots)
                        (list (markdown-planner-capture-file))
                        roots)])
    (open-scratch "*markdown-planner-agenda*" (markdown-planner-format-todos-roots scan-roots #true))))

;;@doc
;; Open a scratch outline for the current Markdown buffer.
(define (markdown-planner-outline)
  (open-scratch "*markdown-planner-outline*"
                (markdown-planner-outline-from-string (current-buffer-text))))

;;@doc
;; Select the Markdown heading/list subtree at the cursor.
(define (markdown-planner-select-subtree)
  (let* ([content (current-buffer-text)]
         [line-number (current-line-number-1)]
         [line-range (markdown-planner-subtree-range-from-string content line-number)]
         [char-range (markdown-planner-lines-range->char-range content
                                                     (car line-range)
                                                     (cdr line-range))]
         [selection (helix.static.range->selection
                     (helix.static.range (car char-range) (cdr char-range)))])
    (helix.static.set-current-selection-object! selection)
    (string-append "selected lines "
                   (number->string (car line-range))
                   "-"
                   (number->string (cdr line-range)))))

(define (path-join . parts)
  (cond
    [(null? parts) ""]
    [(null? (cdr parts)) (car parts)]
    [else
     (let ([left (trim-end-matches (car parts) "/")]
           [right (trim-start-matches (apply path-join (cdr parts)) "/")])
       (string-append left "/" right))]))

(define (config-root)
  (parent-name (helix.static.get-init-scm-path)))

(define (ensure-directory path)
  (unless (path-exists? path)
    (create-directory! path)))

(define (fold-query-path)
  (path-join (config-root) "runtime" "queries" "markdown" "folds.scm"))

(define (ensure-fold-query-directory)
  (let* ([runtime (path-join (config-root) "runtime")]
         [queries (path-join runtime "queries")]
         [markdown (path-join queries "markdown")])
    (ensure-directory runtime)
    (ensure-directory queries)
    (ensure-directory markdown)))

(define (write-file path content)
  (let ([port (open-output-file path #:exists 'truncate)])
    (display content port)
    (close-port port)))

;;@doc
;; Install markdown-planner keybindings for Markdown file extensions.
(define (markdown-planner-install-keybindings)
  (define (install-extension-keymap extension)
    (keymap (extension extension (inherit-from (deep-copy-global-keybindings)))
            (normal
             (S-right ":markdown-planner-toggle-todo")
             (ret ":markdown-planner-toggle-todo")
             (tab ":markdown-planner-select-subtree")
             (S-tab ":markdown-planner-outline")
             (z
              (a ":markdown-planner-select-subtree")
              (c ":markdown-planner-select-subtree")
              (o ":markdown-planner-outline")
              (O ":markdown-planner-outline")
              (M ":markdown-planner-outline")
              (R ":markdown-planner-outline"))
             (space
              (m
               (t ":markdown-planner-toggle-todo")
               (c ":markdown-planner-capture-task")
               (C ":markdown-planner-capture-scheduled-task")
               (s ":markdown-planner-insert-schedule")
               (a ":markdown-planner-agenda")
               (A ":markdown-planner-agenda-all")
               (o ":markdown-planner-outline")
               (h ":markdown-planner-select-subtree")
               (x ":markdown-planner-archive-task"))))))
  (for-each install-extension-keymap markdown-planner-markdown-extensions)
  (string-append "installed markdown-planner keybindings for "
                 (string-join markdown-planner-markdown-extensions ", ")))

;;@doc
;; Activate markdown-planner integration after Helix has finished loading the
;; surrounding Steel config.
(define (markdown-planner-activate)
  (helix.misc.enqueue-thread-local-callback markdown-planner-install-keybindings)
  "scheduled markdown-planner keybinding installation")

;;@doc
;; Install the Markdown tree-sitter folds query into the Helix config runtime.
(define (markdown-planner-install-folds)
  (ensure-fold-query-directory)
  (write-file (fold-query-path) markdown-planner-fold-query)
  (string-append "installed markdown folds query: " (fold-query-path)))

(markdown-planner-activate)
