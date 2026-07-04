(require-builtin steel/filesystem)

(provide markdown-planner-open-states
         markdown-planner-done-states
         markdown-planner-all-states
         markdown-planner-fold-query
         markdown-planner-schedule-token
         markdown-planner-extract-schedule
         markdown-planner-set-line-schedule
         markdown-planner-capture-entry
         markdown-planner-append-capture!
         markdown-planner-archive-entry
         markdown-planner-append-archive!
         markdown-planner-ensure-directory-tree
         markdown-planner-markdown-file?
         markdown-planner-files
         markdown-planner-todos
         markdown-planner-open-todos
         markdown-planner-todos-from-string
         markdown-planner-format-todos
         markdown-planner-format-todos-roots
         markdown-planner-toggle-line
         markdown-planner-line-range
         markdown-planner-lines-range->char-range
         markdown-planner-lines-range->delete-char-range
         markdown-planner-lines-range->string
         markdown-planner-subtree-range-from-string
         markdown-planner-fold-ranges-from-string
         markdown-planner-outline-from-string)

(define markdown-planner-open-states '("TODO" "NEXT" "DOING" "WAIT" "WAITING" "HOLD"))
(define markdown-planner-done-states '("DONE" "CANCELED" "CANCELLED"))
(define markdown-planner-all-states (append markdown-planner-open-states markdown-planner-done-states))
(define markdown-planner-schedule-marker "SCHEDULED:")

(define markdown-planner-ignored-directories
  '(".git" ".hg" ".jj" ".svn" ".direnv" "node_modules" "target"))

(define markdown-planner-fold-query
  "; Markdown hierarchy folds for Helix.\n\
(section) @fold\n\
(list_item) @fold\n\
(block_quote) @fold\n")

(define (string-member? needle haystack)
  (cond [(null? haystack) #false]
        [(string=? needle (car haystack)) #true]
        [else (string-member? needle (cdr haystack))]))

(define (safe-substring value start end)
  (if (>= start end)
      ""
      (substring value start end)))

(define (char-at value index)
  (string-ref value index))

(define (char-whitespace-or-end? value index)
  (or (>= index (string-length value))
      (char-whitespace? (char-at value index))))

(define (skip-whitespace value index)
  (if (and (< index (string-length value))
           (char-whitespace? (char-at value index)))
      (skip-whitespace value (+ index 1))
      index))

(define (starts-with-word? value word)
  (let ([len (string-length word)])
    (and (>= (string-length value) len)
         (starts-with? value word)
         (char-whitespace-or-end? value len))))

(define (strip-state-prefix value states)
  (cond [(null? states) #false]
        [(starts-with-word? value (car states))
         (let* ([state (car states)]
                [rest (trim-start
                        (safe-substring value
                                        (string-length state)
                                        (string-length value)))])
           (cons state rest))]
        [else (strip-state-prefix value (cdr states))]))

(define (substring-at? value needle index)
  (let ([needle-length (string-length needle)])
    (and (<= (+ index needle-length) (string-length value))
         (string=? (substring value index (+ index needle-length)) needle))))

(define (find-substring-index value needle)
  (let loop ([index 0])
    (cond [(> (+ index (string-length needle)) (string-length value)) #false]
          [(substring-at? value needle index) index]
          [else (loop (+ index 1))])))

(define (find-char-index value needle start)
  (let loop ([index start])
    (cond [(>= index (string-length value)) #false]
          [(char=? (char-at value index) needle) index]
          [else (loop (+ index 1))])))

(define (wrapped-timestamp? value)
  (and (> (string-length value) 1)
       (starts-with? value "<")
       (ends-with? value ">")))

;;@doc
;; Return an org-style schedule token for a timestamp.
;;
;; Plain input is wrapped as `SCHEDULED: <...>`. Already wrapped timestamps
;; keep their angle brackets.
(define (markdown-planner-schedule-token timestamp)
  (let ([value (trim timestamp)])
    (cond [(string-blank? value) (error "schedule cannot be blank")]
          [(starts-with? value markdown-planner-schedule-marker) value]
          [(wrapped-timestamp? value)
           (string-append markdown-planner-schedule-marker " " value)]
          [else
           (string-append markdown-planner-schedule-marker " <" value ">")])))

(define (schedule-token-value token)
  (let ([trimmed (trim token)])
    (if (starts-with? trimmed markdown-planner-schedule-marker)
        (trim (safe-substring trimmed
                              (string-length markdown-planner-schedule-marker)
                              (string-length trimmed)))
        trimmed)))

(define (schedule-token-end value after-marker)
  (if (and (< after-marker (string-length value))
           (char=? (char-at value after-marker) #\<))
      (let ([closing (find-char-index value #\> (+ after-marker 1))])
        (if closing (+ closing 1) (string-length value)))
      (string-length value)))

(define (join-schedule-sides left right)
  (let ([clean-left (trim-end left)]
        [clean-right (trim-start right)])
    (cond [(string-blank? clean-left) clean-right]
          [(string-blank? clean-right) clean-left]
          [else (string-append clean-left " " clean-right)])))

;;@doc
;; Remove the first `SCHEDULED: <...>` token from a string.
;; Returns `(clean-text . scheduled-value)`.
(define (markdown-planner-extract-schedule value)
  (let ([marker-index (find-substring-index value markdown-planner-schedule-marker)])
    (if marker-index
        (let* ([after-marker (skip-whitespace value
                                              (+ marker-index
                                                 (string-length markdown-planner-schedule-marker)))]
               [token-end (schedule-token-end value after-marker)]
               [token (safe-substring value marker-index token-end)]
               [scheduled (schedule-token-value token)]
               [left (safe-substring value 0 marker-index)]
               [right (safe-substring value token-end (string-length value))])
          (cons (join-schedule-sides left right)
                (if (string-blank? scheduled) #false scheduled)))
        (cons value #false))))

;;@doc
;; Insert or replace a schedule token on a Markdown line.
(define (markdown-planner-set-line-schedule line timestamp)
  (let* ([schedule-result (markdown-planner-extract-schedule line)]
         [line-without-schedule (car schedule-result)]
         [schedule-token (markdown-planner-schedule-token timestamp)])
    (if (string-blank? line-without-schedule)
        schedule-token
        (string-append (trim-end line-without-schedule) " " schedule-token))))

(define (open-state? state)
  (string-member? state markdown-planner-open-states))

(define (done-state? state)
  (string-member? state markdown-planner-done-states))

(define (next-state state)
  (cond [(open-state? state) "DONE"]
        [(done-state? state) "TODO"]
        [else "TODO"]))

(define (count-heading-markers value)
  (let loop ([index 0])
    (if (and (< index (string-length value))
             (char=? (char-at value index) #\#))
        (loop (+ index 1))
        index)))

(define (parse-heading-line line)
  (let* ([trimmed (trim-start line)]
         [indent-size (- (string-length line) (string-length trimmed))]
         [level (count-heading-markers trimmed)])
    (if (and (> level 0)
             (<= level 6)
             (char-whitespace-or-end? trimmed level))
        (let* ([content-start (+ indent-size (skip-whitespace trimmed level))]
               [prefix (safe-substring line 0 content-start)]
               [title (safe-substring line content-start (string-length line))])
          (hash 'line line
                'prefix prefix
                'level level
                'title title))
        #false)))

(define (ordered-list-marker-end value index)
  (let loop ([cursor index] [seen-digit? #false])
    (cond [(>= cursor (string-length value)) #false]
          [(char-digit? (char-at value cursor))
           (loop (+ cursor 1) #true)]
          [(and seen-digit?
                (or (char=? (char-at value cursor) #\.)
                    (char=? (char-at value cursor) #\)))
                (char-whitespace-or-end? value (+ cursor 1)))
           (+ cursor 1)]
          [else #false])))

(define (list-marker-split line)
  (let* ([trimmed (trim-start line)]
         [indent-size (- (string-length line) (string-length trimmed))])
    (cond [(and (>= (string-length trimmed) 2)
                (or (char=? (char-at trimmed 0) #\-)
                    (char=? (char-at trimmed 0) #\*)
                    (char=? (char-at trimmed 0) #\+))
                (char-whitespace? (char-at trimmed 1)))
           (let* ([content-index (+ indent-size (skip-whitespace trimmed 2))]
                  [prefix (safe-substring line 0 content-index)]
                  [rest (safe-substring line content-index (string-length line))])
             (hash 'indent indent-size 'prefix prefix 'rest rest))]
          [(ordered-list-marker-end trimmed 0)
           => (lambda (marker-end)
                (let* ([content-index (+ indent-size (skip-whitespace trimmed marker-end))]
                       [prefix (safe-substring line 0 content-index)]
                       [rest (safe-substring line content-index (string-length line))])
                  (hash 'indent indent-size 'prefix prefix 'rest rest)))]
          [else #false])))

(define (checkbox-state-and-title value)
  (if (and (>= (string-length value) 3)
           (char=? (char-at value 0) #\[)
           (char=? (char-at value 2) #\]))
      (let ([mark (char-at value 1)]
            [title (trim-start (safe-substring value 3 (string-length value)))])
        (cond [(char=? mark #\space) (cons "TODO" title)]
              [(or (char=? mark #\x) (char=? mark #\X)) (cons "DONE" title)]
              [(char=? mark #\-) (cons "WAIT" title)]
              [else #false]))
      #false))

(define (toggle-checkbox-line prefix rest)
  (let* ([mark (char-at rest 1)]
         [next-mark (if (or (char=? mark #\x) (char=? mark #\X)) " " "x")]
         [title (trim-start (safe-substring rest 3 (string-length rest)))])
    (string-append prefix "[" next-mark "] " title)))

(define (toggle-stateful-text prefix text)
  (let ([state-prefix (strip-state-prefix (trim-start text) markdown-planner-all-states)])
    (if state-prefix
        (string-append prefix
                       (next-state (car state-prefix))
                       " "
                       (cdr state-prefix))
        (string-append prefix "TODO " (trim-start text)))))

;;@doc
;; Toggle TODO state for one Markdown line.
;;
;; Headings and list items cycle no-state -> TODO -> DONE -> TODO.
;; Task checkboxes cycle unchecked/waiting -> checked -> unchecked.
(define (markdown-planner-toggle-line line)
  (let ([heading (parse-heading-line line)])
    (cond [heading
           (toggle-stateful-text (hash-ref heading 'prefix)
                                 (hash-ref heading 'title))]
          [(list-marker-split line)
           => (lambda (split)
                (let ([prefix (hash-ref split 'prefix)]
                      [rest (hash-ref split 'rest)])
                  (if (checkbox-state-and-title rest)
                      (toggle-checkbox-line prefix rest)
                      (toggle-stateful-text prefix rest))))]
          [else
           (let* ([trimmed (trim-start line)]
                  [indent-size (- (string-length line) (string-length trimmed))]
                  [prefix (safe-substring line 0 indent-size)])
             (toggle-stateful-text prefix trimmed))])))

(define (colon-tag-token? value)
  (and (> (string-length value) 2)
       (starts-with? value ":")
       (ends-with? value ":")))

(define (string-blank? value)
  (= (string-length (trim value)) 0))

(define (remove-empty-strings values)
  (filter (lambda (value) (not (string-blank? value))) values))

(define (last-list-item values)
  (cond [(null? values) #false]
        [(null? (cdr values)) (car values)]
        [else (last-list-item (cdr values))]))

(define (drop-last-list-item values)
  (cond [(null? values) '()]
        [(null? (cdr values)) '()]
        [else (cons (car values) (drop-last-list-item (cdr values)))]))

(define (extract-tags title)
  (let* ([parts (split-whitespace title)]
         [last-part (last-list-item parts)])
    (if (and last-part (colon-tag-token? last-part))
        (let* ([tag-body (substring last-part 1 (- (string-length last-part) 1))]
               [tags (remove-empty-strings (split-many tag-body ":"))]
               [clean-title (string-join (drop-last-list-item parts) " ")])
          (cons clean-title tags))
        (cons title '()))))

(define (make-todo file line kind level state title context)
  (let* ([raw-title (trim title)]
         [schedule-result (markdown-planner-extract-schedule raw-title)]
         [title-without-schedule (car schedule-result)]
         [scheduled (cdr schedule-result)]
         [tags-result (extract-tags (trim title-without-schedule))]
         [clean-title (car tags-result)]
         [tags (cdr tags-result)])
    (hash 'file file
          'line line
          'kind kind
          'level level
          'state state
          'title clean-title
          'scheduled scheduled
          'tags tags
          'context context)))

(define (parse-todo-line file line-number context line)
  (let ([heading (parse-heading-line line)])
    (cond [heading
           (let* ([title (hash-ref heading 'title)]
                  [state-prefix (strip-state-prefix title markdown-planner-all-states)])
             (if state-prefix
                 (make-todo file
                            line-number
                            'heading
                            (hash-ref heading 'level)
                            (car state-prefix)
                            (cdr state-prefix)
                            context)
                 #false))]
          [(list-marker-split line)
           => (lambda (split)
                (let* ([rest (hash-ref split 'rest)]
                       [checkbox (checkbox-state-and-title rest)]
                       [state-prefix (and (not checkbox)
                                          (strip-state-prefix rest markdown-planner-all-states))])
                  (cond [checkbox
                         (make-todo file line-number 'checkbox #false (car checkbox) (cdr checkbox) context)]
                        [state-prefix
                         (make-todo file line-number 'list #false (car state-prefix) (cdr state-prefix) context)]
                        [else #false])))]
          [else #false])))

(define (fence-line? line)
  (let ([trimmed (trim-start line)])
    (or (starts-with? trimmed "```")
        (starts-with? trimmed "~~~"))))

(define (heading-context heading)
  (if heading
      (let* ([state-prefix (strip-state-prefix (hash-ref heading 'title) markdown-planner-all-states)]
             [title (if state-prefix (cdr state-prefix) (hash-ref heading 'title))]
             [schedule-result (markdown-planner-extract-schedule title)]
             [tags-result (extract-tags (car schedule-result))])
        (car tags-result))
      #false))

;;@doc
;; Parse Markdown TODO entries from a string.
(define (markdown-planner-todos-from-string file content [include-done? #false])
  (let loop ([lines (split-many content "\n")]
             [line-number 1]
             [context #false]
             [in-code-block? #false]
             [entries '()])
    (if (null? lines)
        (reverse entries)
        (let* ([line (car lines)]
               [fence? (fence-line? line)]
               [next-in-code-block? (if fence? (not in-code-block?) in-code-block?)]
               [heading (and (not in-code-block?) (parse-heading-line line))]
               [next-context (or (heading-context heading) context)]
               [entry (and (not in-code-block?)
                           (parse-todo-line file line-number context line))]
               [visible-entry? (and entry
                                    (or include-done?
                                        (open-state? (hash-ref entry 'state))))])
          (loop (cdr lines)
                (+ line-number 1)
                next-context
                next-in-code-block?
                (if visible-entry? (cons entry entries) entries))))))

(define (markdown-planner-markdown-file? path)
  (let ([extension (path->extension path)])
    (and (string? extension)
         (or (string=? extension "md")
             (string=? extension "markdown")
             (string=? extension "mdown")
             (string=? extension "mkd")
             (string=? extension "mkdn")))))

(define (ignored-directory? path)
  (string-member? (file-name path) markdown-planner-ignored-directories))

(define (walk-markdown-files path acc)
  (cond [(is-file? path)
         (if (markdown-planner-markdown-file? path) (cons path acc) acc)]
        [(and (is-dir? path) (not (ignored-directory? path)))
         (foldl (lambda (child next-acc)
                  (walk-markdown-files child next-acc))
                acc
                (read-dir path))]
        [else acc]))

(define (markdown-planner-files roots)
  (reverse (foldl (lambda (root acc)
                    (walk-markdown-files root acc))
                  '()
                  roots)))

(define (read-file-to-string path)
  (let* ([port (open-input-file path)]
         [content (read-port-to-string port)])
    (close-input-port port)
    content))

(define (write-string-to-file path content)
  (let ([port (open-output-file path #:exists 'truncate)])
    (display content port)
    (close-port port)))

(define (root-or-empty-path? path)
  (or (string-blank? path)
      (string=? path "/")
      (string=? path (parent-name path))))

;;@doc
;; Create a directory and its parents when they do not exist.
(define (markdown-planner-ensure-directory-tree path)
  (unless (or (root-or-empty-path? path)
              (path-exists? path))
    (let ([parent (parent-name path)])
      (unless (root-or-empty-path? parent)
        (markdown-planner-ensure-directory-tree parent))
      (create-directory! path))))

(define (capture-file-prefix path)
  (if (path-exists? path)
      (let ([content (read-file-to-string path)])
        (cond [(string-blank? content) "# Tasks\n\n"]
              [(ends-with? content "\n") content]
              [else (string-append content "\n")]))
      "# Tasks\n\n"))

(define (archive-file-prefix path)
  (if (path-exists? path)
      (let ([content (read-file-to-string path)])
        (cond [(string-blank? content) "# Archive\n\n"]
              [(ends-with? content "\n") content]
              [else (string-append content "\n")]))
      "# Archive\n\n"))

;;@doc
;; Format a Markdown TODO capture entry.
(define (markdown-planner-capture-entry title [scheduled #false])
  (let* ([clean-title (trim title)]
         [title-without-schedule (if scheduled
                                     (car (markdown-planner-extract-schedule clean-title))
                                     clean-title)])
    (when (string-blank? title-without-schedule)
      (error "capture title cannot be blank"))
    (string-append "- [ ] "
                   title-without-schedule
                   (if scheduled
                       (string-append " " (markdown-planner-schedule-token scheduled))
                       "")
                   "\n")))

;;@doc
;; Append a Markdown TODO capture entry to a file.
(define (markdown-planner-append-capture! path title [scheduled #false])
  (markdown-planner-ensure-directory-tree (parent-name path))
  (write-string-to-file path
                        (string-append (capture-file-prefix path)
                                       (markdown-planner-capture-entry title scheduled)))
  path)

;;@doc
;; Format a Markdown block for the archive file.
(define (markdown-planner-archive-entry block)
  (let ([clean-block (trim block)])
    (when (string-blank? clean-block)
      (error "archive block cannot be blank"))
    (string-append clean-block "\n\n")))

;;@doc
;; Append a Markdown block to an archive file.
(define (markdown-planner-append-archive! path block)
  (markdown-planner-ensure-directory-tree (parent-name path))
  (write-string-to-file path
                        (string-append (archive-file-prefix path)
                                       (markdown-planner-archive-entry block)))
  path)

(define (file->todos path include-done?)
  (markdown-planner-todos-from-string path (read-file-to-string path) include-done?))

(define (markdown-planner-todos roots [include-done? #false])
  (foldl (lambda (path entries)
           (append entries (file->todos path include-done?)))
         '()
         (markdown-planner-files roots)))

(define (markdown-planner-open-todos roots)
  (markdown-planner-todos roots #false))

(define (entry-location entry)
  (string-append (hash-ref entry 'file)
                 ":"
                 (number->string (hash-ref entry 'line))))

(define (format-tags tags)
  (if (null? tags)
      ""
      (string-append " :" (string-join tags ":") ":")))

(define (format-context entry)
  (let ([context (hash-try-get entry 'context)])
    (if context
        (string-append " < " context)
        "")))

(define (format-scheduled entry)
  (let ([scheduled (hash-try-get entry 'scheduled)])
    (if scheduled
        (string-append " " markdown-planner-schedule-marker " " scheduled)
        "")))

(define (format-entry entry)
  (string-append "- ["
                 (if (open-state? (hash-ref entry 'state)) " " "x")
                 "] "
                 (hash-ref entry 'state)
                 " "
                 (hash-ref entry 'title)
                 (format-tags (hash-ref entry 'tags))
                 (format-scheduled entry)
                 (format-context entry)
                 " ("
                 (entry-location entry)
                 ")\n"))

(define (entry-scheduled-for-sort entry)
  (let ([scheduled (hash-try-get entry 'scheduled)])
    (if scheduled scheduled "~~~~")))

(define (entry<=? left right)
  (let ([left-scheduled (entry-scheduled-for-sort left)]
        [right-scheduled (entry-scheduled-for-sort right)]
        [left-file (hash-ref left 'file)]
        [right-file (hash-ref right 'file)]
        [left-line (hash-ref left 'line)]
        [right-line (hash-ref right 'line)])
    (cond [(string<? left-scheduled right-scheduled) #true]
          [(string>? left-scheduled right-scheduled) #false]
          [(string<? left-file right-file) #true]
          [(string>? left-file right-file) #false]
          [else (<= left-line right-line)])))

(define (insert-sorted-entry entry entries)
  (cond [(null? entries) (list entry)]
        [(entry<=? entry (car entries)) (cons entry entries)]
        [else (cons (car entries)
                    (insert-sorted-entry entry (cdr entries)))]))

(define (sort-entries entries)
  (foldl (lambda (entry sorted)
           (insert-sorted-entry entry sorted))
         '()
         entries))

(define (markdown-planner-format-todos entries)
  (let ([sorted (sort-entries entries)])
    (if (null? sorted)
        "# Markdown Planner TODOs\n\nNo TODO items.\n"
        (string-append "# Markdown Planner TODOs\n\n"
                       (foldl (lambda (entry acc)
                                (string-append acc (format-entry entry)))
                              ""
                              sorted)))))

(define (markdown-planner-format-todos-roots roots [include-done? #false])
  (markdown-planner-format-todos (markdown-planner-todos roots include-done?)))

(define (heading-entry line-number line)
  (let ([heading (parse-heading-line line)])
    (if heading
        (let* ([state-prefix (strip-state-prefix (hash-ref heading 'title) markdown-planner-all-states)]
               [title (if state-prefix (cdr state-prefix) (hash-ref heading 'title))]
               [schedule-result (markdown-planner-extract-schedule title)]
               [tags-result (extract-tags (car schedule-result))])
          (hash 'line line-number
                'level (hash-ref heading 'level)
                'title (car tags-result)
                'scheduled (cdr schedule-result)
                'state (if state-prefix (car state-prefix) #false)))
        #false)))

(define (headings-from-lines lines)
  (let loop ([remaining lines]
             [line-number 1]
             [headings '()])
    (if (null? remaining)
        (reverse headings)
        (let ([entry (heading-entry line-number (car remaining))])
          (loop (cdr remaining)
                (+ line-number 1)
                (if entry (cons entry headings) headings))))))

(define (first-heading-after headings start-line level)
  (cond [(null? headings) #false]
        [(and (> (hash-ref (car headings) 'line) start-line)
              (<= (hash-ref (car headings) 'level) level))
         (car headings)]
        [else (first-heading-after (cdr headings) start-line level)]))

(define (heading-at-or-before headings cursor-line)
  (let loop ([remaining headings]
             [candidate #false])
    (cond [(null? remaining) candidate]
          [(<= (hash-ref (car remaining) 'line) cursor-line)
           (loop (cdr remaining) (car remaining))]
          [else candidate])))

(define (line-count content)
  (length (split-many content "\n")))

(define (heading-subtree-range lines cursor-line)
  (let* ([headings (headings-from-lines lines)]
         [heading (heading-at-or-before headings cursor-line)])
    (if heading
        (let* ([start-line (hash-ref heading 'line)]
               [level (hash-ref heading 'level)]
               [next (first-heading-after headings start-line level)]
               [end-line (if next (- (hash-ref next 'line) 1) (length lines))])
          (cons start-line end-line))
        #false)))

(define (list-item-at-line lines cursor-line)
  (let ([line (list-ref lines (- cursor-line 1))])
    (list-marker-split line)))

(define (list-subtree-end lines start-line indent)
  (let loop ([line-number (+ start-line 1)]
             [remaining (list-tail lines start-line)]
             [last-line start-line])
    (if (null? remaining)
        last-line
        (let* ([line (car remaining)]
               [trimmed (trim line)]
               [split (list-marker-split line)]
               [heading (parse-heading-line line)])
          (cond [(string-blank? line)
                 (loop (+ line-number 1) (cdr remaining) line-number)]
                [(and heading (<= (- (string-length line) (string-length (trim-start line))) indent))
                 last-line]
                [(and split (<= (hash-ref split 'indent) indent))
                 last-line]
                [else
                 (loop (+ line-number 1) (cdr remaining) line-number)])))))

(define (list-subtree-range lines cursor-line)
  (if (and (>= cursor-line 1)
           (<= cursor-line (length lines)))
      (let ([split (list-item-at-line lines cursor-line)])
        (if split
            (cons cursor-line
                  (list-subtree-end lines cursor-line (hash-ref split 'indent)))
            #false))
      #false))

;;@doc
;; Return the 1-based line range for the current Markdown subtree.
(define (markdown-planner-subtree-range-from-string content cursor-line)
  (let* ([lines (split-many content "\n")]
         [list-range (list-subtree-range lines cursor-line)])
    (or list-range
        (heading-subtree-range lines cursor-line)
        (cons cursor-line cursor-line))))

(define (list-fold-ranges lines)
  (let loop ([remaining lines]
             [line-number 1]
             [ranges '()])
    (if (null? remaining)
        (reverse ranges)
        (let ([split (list-marker-split (car remaining))])
          (loop (cdr remaining)
                (+ line-number 1)
                (if split
                    (let ([range (cons line-number
                                       (list-subtree-end lines line-number (hash-ref split 'indent)))])
                      (if (> (cdr range) (car range))
                          (cons range ranges)
                          ranges))
                    ranges))))))

(define (heading-fold-ranges lines)
  (let* ([headings (headings-from-lines lines)])
    (foldl (lambda (heading ranges)
             (let* ([start-line (hash-ref heading 'line)]
                    [level (hash-ref heading 'level)]
                    [next (first-heading-after headings start-line level)]
                    [end-line (if next (- (hash-ref next 'line) 1) (length lines))]
                    [range (cons start-line end-line)])
               (if (> end-line start-line)
                   (cons range ranges)
                   ranges)))
           '()
           headings)))

(define (markdown-planner-fold-ranges-from-string content)
  (let ([lines (split-many content "\n")])
    (reverse (append (heading-fold-ranges lines)
                     (list-fold-ranges lines)))))

(define (make-spaces count)
  (if (<= count 0)
      ""
      (string-append " " (make-spaces (- count 1)))))

(define (outline-heading-line heading)
  (let* ([level (hash-ref heading 'level)]
         [state (hash-try-get heading 'state)]
         [state-text (if state (string-append state " ") "")]
         [indent (make-spaces (* 2 (- level 1)))])
    (string-append indent
                   "- "
                   state-text
                   (hash-ref heading 'title)
                   " (line "
                   (number->string (hash-ref heading 'line))
                   ")\n")))

(define (markdown-planner-outline-from-string content)
  (let ([headings (headings-from-lines (split-many content "\n"))])
    (if (null? headings)
        "# Markdown Planner Outline\n\nNo headings.\n"
        (string-append "# Markdown Planner Outline\n\n"
                       (foldl (lambda (heading acc)
                                (string-append acc (outline-heading-line heading)))
                              ""
                              headings)))))

(define (line-start-offset lines line-number)
  (let loop ([remaining lines]
             [current-line 1]
             [offset 0])
    (if (or (null? remaining) (= current-line line-number))
        offset
        (loop (cdr remaining)
              (+ current-line 1)
              (+ offset (string-length (car remaining)) 1)))))

(define (markdown-planner-line-range content line-number)
  (let* ([lines (split-many content "\n")]
         [line (if (and (>= line-number 1) (<= line-number (length lines)))
                   (list-ref lines (- line-number 1))
                   "")]
         [start (line-start-offset lines line-number)]
         [end (+ start (string-length line))])
    (hash 'line line 'start start 'end end)))

(define (markdown-planner-lines-range->char-range content start-line end-line)
  (let* ([lines (split-many content "\n")]
         [start (line-start-offset lines start-line)]
         [end-info (markdown-planner-line-range content end-line)])
    (cons start (hash-ref end-info 'end))))

(define (markdown-planner-lines-range->delete-char-range content start-line end-line)
  (let* ([lines (split-many content "\n")]
         [start (line-start-offset lines start-line)]
         [end (if (< end-line (length lines))
                  (line-start-offset lines (+ end-line 1))
                  (hash-ref (markdown-planner-line-range content end-line) 'end))])
    (cons start end)))

(define (markdown-planner-lines-range->string content start-line end-line)
  (let* ([char-range (markdown-planner-lines-range->char-range content start-line end-line)]
         [start (car char-range)]
         [end (cdr char-range)])
    (safe-substring content start end)))
