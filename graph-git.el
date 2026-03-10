;;; graph-git.el --- Git history graphs using graph.el + graph-svg.el  -*- lexical-binding: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (graph "0.1") (graph-svg "0.1") (cl-lib "0.5"))
;; Keywords: git, graph, vcs
;; URL: https://github.com/hkjels/graph.el

;;; Commentary:

;; Async Git specific glue on top of:
;;
;;   - graph.el     (abstract nodes and edges)
;;   - graph-svg.el (SVG rendering)
;;
;; It:
;;   - Streams `git log` asynchronously.
;;   - Parses commits incrementally.
;;   - Colors history lanes by refs (branches, tags, remotes).
;;   - Builds a `graph-graph` and renders it as SVG when the log finishes.
;;
;; The final UI is a `*Git Graph*` buffer where:
;;
;;   - Each line is a commit subject (plain text).
;;   - The SVG graph is displayed in the left margin.
;;   - Node rows correspond to line numbers in the buffer.
;;   - Details about the currently selected commit is shown in a side-buffer

;;; Code:

(require 'svg)
(require 'svg-lib)
(require 'subr-x)
(require 'cl-lib)
(require 'cl-seq)
(require 'vc)
(require 'graph)
(require 'graph-svg)

(defgroup graph-git nil
  "View git history using graph.el and graph-svg.el."
  :group 'tools)

(defface graph-git-branch-face-1
  '((t :foreground "#00bfff"))
  "Branch lane face 1."
  :group 'graph-git)

(defface graph-git-branch-face-2
  '((t :foreground "#98fb98"))
  "Branch lane face 2."
  :group 'graph-git)

(defface graph-git-branch-face-3
  '((t :foreground "#ffa07a"))
  "Branch lane face 3."
  :group 'graph-git)

(defface graph-git-branch-face-4
  '((t :foreground "#da70d6"))
  "Branch lane face 4."
  :group 'graph-git)

(defface graph-git-branch-face-5
  '((t :foreground "#708090"))
  "Branch lane face 5."
  :group 'graph-git)

(defcustom graph-git-face-palette
  [graph-git-branch-face-1
   graph-git-branch-face-2
   graph-git-branch-face-3
   graph-git-branch-face-4
   graph-git-branch-face-5]
  "Vector of faces used as a palette when coloring refs and lanes."
  :type '(vector face)
  :group 'graph-git)

(defcustom graph-git-date-format "%Y.%m.%d %H:%M %z"
  "Format string used to display commit dates in the info panel.

The default is a sortable representation:
  2025.12.03 09:50 +0200

This uses `format-time-string' syntax.  The date provided by git is
parsed via `date-to-time'."
  :type 'string
  :group 'graph-git)

(defcustom graph-git-fold-diffs t
  "Automatically fold diffs under their respective filename."
  :type 'boolean
  :group 'graph-git)

(defcustom graph-git-page-size 280
  "How many commits to fetch per batch."
  :type 'integer
  :group 'graph-git)

(defcustom graph-git-load-more-threshold 40
  "Load another page when point gets within this many lines of the end."
  :type 'integer
  :group 'graph-git)

(defcustom graph-git-load-more-idle-delay 0.35
  "How long to wait before auto-loading more history near the bottom."
  :type 'number
  :group 'graph-git)

(defun graph-git--palette-face (key)
  "Return a face from `graph-git-face-palette' for KEY.

KEY is usually a ref string or lane index."
  (let* ((len (length graph-git-face-palette))
         (idx (mod (abs (sxhash key)) len)))
    (aref graph-git-face-palette idx)))

;;; Async session struct

(cl-defstruct graph-git-session
  dir          ;; repo root
  proc         ;; git log process
  partial      ;; partial record (string)
  commits      ;; loaded commit plists (newest first)
  batch        ;; in-flight batch, newest first after sentinel nreverse
  ref-table    ;; hash: commit-hash -> list of refnames
  buffer       ;; buffer where we show the graph
  skip         ;; how many commits are already loaded
  loading      ;; non-nil while a git log process is running
  exhausted)   ;; non-nil when there are no more commits to fetch

(defvar graph-git--session nil
  "Current graph-git session for this buffer.")
(make-variable-buffer-local 'graph-git--session)
(put 'graph-git--session 'permanent-local t)

;;; Main display mode

(defun graph-git--format-date (git-date-string)
  "Convert GIT-DATE-STRING returned by git into user formatted form.

Uses `graph-git-date-format'."
  (condition-case nil
      (format-time-string graph-git-date-format
                          (date-to-time git-date-string))
    (error git-date-string)))

(defvar graph-git-info-buffer-name "*Git Graph Info*")
(defvar graph-git-buffer-name "*Git Graph*")

(defun graph-git--short-refname (refname)
  "Return a compact display form for REFNAME."
  (cond
   ((string-prefix-p "refs/heads/" refname)
    (substring refname (length "refs/heads/")))
   ((string-prefix-p "refs/remotes/" refname)
    (substring refname (length "refs/remotes/")))
   ((string-prefix-p "refs/tags/" refname)
    (concat "tag:" (substring refname (length "refs/tags/"))))
   (t refname)))

(defun graph-git--commit-refs (commit ref-table)
  "Return display refs for COMMIT from REF-TABLE."
  (let ((refs (and ref-table
                   (gethash (plist-get commit :hash) ref-table))))
    (mapcar #'graph-git--short-refname refs)))

(defun graph-git--format-ref-labels (refs face)
  "Return a propertized string for REFS using FACE."
  (when refs
    (concat
     (propertize
      (format "[%s] " (string-join refs ", "))
      'face `(:inherit ,face :weight bold)))))

;;; Lane state helpers (Git-ish lane allocator)

(defun graph-git--select-primary-parent (parents lanes)
  "Select the best primary parent from PARENTS given active LANES.

Heuristic:
- Prefer the parent whose hash already exists in LANES.
- If multiple match, choose the leftmost one.
- Otherwise fall back to the first parent in PARENTS."
  (or (cl-loop for lane in lanes
               when (member lane parents)
               return lane)
      (car parents)))

(defun graph-git--compact-lanes (lanes lane-keys)
  "Compact LANES while keeping LANE-KEYS aligned.

Rules:
  - Drop nil entries.
  - Drop duplicate commit hashes, keeping the leftmost occurrence.

Return (LANES' . LANE-KEYS')."
  (let ((seen (make-hash-table :test #'equal))
        (out-lanes '())
        (out-keys  '()))
    (cl-loop for h in lanes
             for k in lane-keys
             do (when (and h (not (gethash h seen)))
                  (puthash h t seen)
                  (push h out-lanes)
                  (push k out-keys)))
    (cons (nreverse out-lanes) (nreverse out-keys))))

(defun graph-git--lane-key-for (key)
  "Return a stable palette key for KEY (usually a hash)."
  key)

(defun graph-git--dedupe-consecutive-points (points)
  "Return POINTS with consecutive duplicate coordinates removed."
  (let (out)
    (dolist (point points)
      (unless (equal point (car out))
        (push point out)))
    (nreverse out)))

(defun graph-git--walk-lane-path (start-row start-col end-row end-col)
  "Return a Git-style path from START-ROW / START-COL to END-ROW / END-COL.

The path descends one row at a time and shifts at most one column per row,
so lane changes become visible diagonals instead of same-row elbows."
  (let ((row start-row)
        (col start-col)
        (points (list (cons start-row start-col))))
    (while (< row end-row)
      (setq row (1+ row))
      (when (/= col end-col)
        (setq col (+ col (if (< col end-col) 1 -1))))
      (push (cons row col) points))
    (while (/= col end-col)
      (setq col (+ col (if (< col end-col) 1 -1)))
      (push (cons row col) points))
    (nreverse points)))

(defun graph-git--edge-path (row col-start parent-row col-mid parent-lane)
  "Return a polyline for an edge from ROW/COL-START to PARENT-ROW.

COL-MID is the lane the edge should occupy between rows, and PARENT-LANE is
the lane of the parent node itself."
  (let* ((approach-row (if (and (/= col-mid parent-lane)
                                (> parent-row (1+ row)))
                           (1- parent-row)
                         parent-row))
         (path-to-lane (graph-git--walk-lane-path row col-start approach-row col-mid))
         (tail (if (< approach-row parent-row)
                   (graph-git--walk-lane-path approach-row col-mid
                                              parent-row parent-lane)
                 (list (cons parent-row parent-lane)))))
    (graph-git--dedupe-consecutive-points
     (append path-to-lane tail))))

(defvar-local graph-git--commits nil)
(defvar-local graph-git--current-commit nil)
(defvar-local graph-git--load-more-timer nil)
(put 'graph-git--commits 'permanent-local t)
(put 'graph-git--current-commit 'permanent-local t)
(put 'graph-git--load-more-timer 'permanent-local t)

(defvar graph-git-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'graph-git-quit)
    map)
  "Keymap for `graph-git-info-mode'.")

(defvar graph-git-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'graph-git-refresh)
    (define-key map (kbd "q") #'graph-git-quit)
    map)
  "Keymap for `graph-git-view-mode'.")

(defun graph-git--ensure-info-window ()
  (let ((buf (get-buffer-create graph-git-info-buffer-name)))
    (unless (get-buffer-window buf)
      (let ((win (display-buffer-in-side-window
                  buf '((side . right) (window-width . 0.33)))))
        (with-current-buffer buf
          (graph-git-info-mode))))
    buf))

(defun graph-git--strip-diff-header (diff)
  "Strip git show header lines from DIFF, keep only hunks."
  (let* ((lines (string-lines diff))
         (out nil)
         (seen-hunk nil))
    (dolist (l lines)
      (when (and (not seen-hunk)
                 (string-prefix-p "@@" l))
        (setq seen-hunk t))
      (when seen-hunk
        (push l out)))
    (string-join (nreverse out) "\n")))

(defun graph-git--fontify-diff (diff-text)
  "Return DIFF-TEXT with `diff-mode` font-lock applied."
  (with-temp-buffer
    (diff-mode)
    (insert diff-text)
    (font-lock-ensure)
    (buffer-substring (point-min) (point-max))))

(defun graph-git--commit-details (dir hash)
  "Return plist with :subject :body :author :date :files for HASH in DIR.

:files is a list of plists (:path :add :del :diff), where :diff is
the full patch for that file."
  (let* (;; Full message, then split into subject + body
         (full-msg (string-trim
                    (graph-git--call-raw dir
                                         "show" "-s"
                                         "--format=%B" hash)))
         (msg-lines (string-lines full-msg t))
         (subject (or (car msg-lines) ""))
         (body    (string-join (or (cdr msg-lines) '()) "\n"))

         ;; Author + date on separate lines
         (meta (string-trim
                (graph-git--call-raw dir
                                     "show" "-s"
                                     "--format=%an%n%ad" hash)))
         (meta-lines (string-lines meta t))
         (author (or (nth 0 meta-lines) ""))
         (date   (or (nth 1 meta-lines) ""))

         ;; Per-file stats (additions / deletions)
         (numstat-raw (graph-git--call-raw dir
                                           "show" "--numstat"
                                           "--format=" hash))
         (stats (make-hash-table :test #'equal)))

    ;; Parse numstat output into hash table: path -> (add . del)
    (dolist (line (string-lines numstat-raw t))
      (when (string-match
             "\\`\\([^[:space:]]+\\)[[:space:]]+\\([^[:space:]]+\\)[[:space:]]+\\(.+\\)\\'"
             line)
        (let ((add-str (match-string 1 line))
              (del-str (match-string 2 line))
              (path    (match-string 3 line)))
          (puthash path
                   (cons (if (string= add-str "-") 0 (string-to-number add-str))
                         (if (string= del-str "-") 0 (string-to-number del-str)))
                   stats))))

    ;; Full patch, split per file by `diff --git` blocks
    (let* ((patch-raw (graph-git--call-raw dir
                                           "show" "--patch"
                                           "--format=" hash))
           (start (string-match "^diff --git " patch-raw))
           (patch-part (if start (substring patch-raw start) ""))
           (chunks (unless (string-empty-p patch-part)
                     (mapcar
                      (lambda (s) (concat "diff --git " s))
                      (split-string patch-part "^diff --git " t)))))

      (list
       :subject subject
       :body    body
       :author  author
       :date    (graph-git--format-date date)
       :files
       (mapcar
        (lambda (chunk)
          (let ((path "?")
                (add 0)
                (del 0))
            ;; Extract path from `diff --git a/PATH b/PATH`
            (when (string-match "^diff --git a/\\([^[:space:]]+\\) b/" chunk)
              (setq path (match-string 1 chunk)))
            (let ((stat (gethash path stats)))
              (when stat
                (setq add (car stat)
                      del (cdr stat))))
            (list :path path
                  :add  add
                  :del  del
                  :diff chunk)))
        chunks)))))

(defun graph-git--update-info-panel (commit)
  "Given COMMIT plist, update the info panel."
  (let ((buf (get-buffer graph-git-info-buffer-name))
        (dir (and graph-git--session
                  (graph-git-session-dir graph-git--session)))
        (refs (graph-git--commit-refs commit
                                      (and graph-git--session
                                           (graph-git-session-ref-table
                                            graph-git--session)))))
    (when (and buf commit)
      (with-current-buffer buf
        (let* ((hash    (plist-get commit :hash))
               (details (graph-git--commit-details dir hash))
               (subject (plist-get details :subject))
               (body    (plist-get details :body))
               (author  (plist-get details :author))
               (date    (plist-get details :date))
               (files   (plist-get details :files)))
          (let ((inhibit-read-only t))
            (erase-buffer)

            ;; Subject
            (insert (propertize subject 'face 'bold) "\n")

            (when refs
              (insert (graph-git--format-ref-labels refs 'font-lock-keyword-face)
                      "\n"))
            (insert "\n")

            ;; Body
            (when (and body (not (string-empty-p body)))
              (insert body "\n\n"))

            ;; Metadata
            (insert (format "Author: %s\n" author))
            (insert (format "Commit: %s\n" hash))
            (when refs
              (insert (format "Refs:   %s\n" (string-join refs ", "))))
            (insert (format "Date:   %s\n\n" date))

            ;; File list and diffs
            (dolist (f files)
              (let ((path (plist-get f :path))
                    (add  (plist-get f :add))
                    (del  (plist-get f :del))
                    (diff (plist-get f :diff)))
                ;; Heading
                (insert (format "%s (+%d / -%d)\n" path add del))
                ;; Insert diff body, syntax highlighted
                (when diff
                  (insert
                   (graph-git--fontify-diff diff))))
              (goto-char (point-min))
              (when graph-git-fold-diffs
                (while (re-search-forward "^\\S-.*(\\+\\([0-9]+\\) / -\\([0-9]+\\))$" nil t)
                  (let* ((bol (line-beginning-position))
                         (beg (line-beginning-position 2)))
                    (save-excursion
                      (goto-char beg)
                      (outline-hide-sublevels 1)))))
              (goto-char (point-min)))))))))

(defun graph-git--post-command-update ()
  "Update information panel with the commit at the current line."
  (when (and (eq major-mode 'graph-git-view-mode)
             graph-git--commits)
    (let* ((row (max 0 (1- (line-number-at-pos))))
           (commit (nth row graph-git--commits)))
      (when (and commit (not (eq commit graph-git--current-commit)))
        (setq graph-git--current-commit commit)
        (graph-git--update-info-panel commit)))
    (graph-git--maybe-load-more)))

(defun graph-git--scroll-update (_window _display-start)
  "Trigger lazy loading when the graph window scrolls."
  (when (window-live-p _window)
    (with-current-buffer (window-buffer _window)
      (when (eq major-mode 'graph-git-view-mode)
        (graph-git--maybe-load-more)))))

(defun graph-git--commit-at-point ()
  "Return the commit plist for the current line, or nil."
  (when graph-git--commits
    (nth (max 0 (1- (line-number-at-pos))) graph-git--commits)))

(defun graph-git--cancel-load-more-timer ()
  "Cancel any pending auto-load timer for the current buffer."
  (when (timerp graph-git--load-more-timer)
    (cancel-timer graph-git--load-more-timer))
  (setq graph-git--load-more-timer nil))

(defun graph-git--run-load-more-if-needed (buffer)
  "Load another page for BUFFER if it still needs more history."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq graph-git--load-more-timer nil)
      (when (and (eq major-mode 'graph-git-view-mode)
                 graph-git--session
                 (not (graph-git-session-loading graph-git--session))
                 (not (graph-git-session-exhausted graph-git--session)))
        (graph-git-load-more)))))

(defun graph-git-quit ()
  "Quit the graph view and its associated info window."
  (interactive)
  (let ((graph-buffer (or (and graph-git--session
                               (graph-git-session-buffer graph-git--session))
                          (get-buffer graph-git-buffer-name)))
        (info-buffer (get-buffer graph-git-info-buffer-name)))
    (graph-git--cancel-load-more-timer)
    (when (buffer-live-p info-buffer)
      (delete-windows-on info-buffer)
      (kill-buffer info-buffer))
    (when (buffer-live-p graph-buffer)
      (with-current-buffer graph-buffer
        (graph-git--cancel-load-more-timer))
      (delete-windows-on graph-buffer)
      (kill-buffer graph-buffer))))

(define-derived-mode graph-git-info-mode special-mode "Git-Info"
  "Mode for showing commit details."
  (setq-local buffer-read-only t
              truncate-lines nil
              outline-regexp "^\\S-.*(\\+\\([0-9]+\\) / -\\([0-9]+\\))"
              outline-level (lambda () 1))
  (outline-minor-mode 1)
  (outline-hide-sublevels 1))

(define-derived-mode graph-git-view-mode special-mode "Git-Graph"
  "Major mode for viewing git history as text plus an SVG margin graph."
  (hl-line-mode)
  (setq-local buffer-read-only t
              truncate-lines t
              line-spacing 0
              header-line-format
              "g refresh   q close")
  ;; Real margin width is set when we know the SVG width.
  (setq-local left-margin-width 0)
  (add-hook 'post-command-hook #'graph-git--post-command-update nil t)
  (add-hook 'window-scroll-functions #'graph-git--scroll-update nil t))

;;; Repo root and sync git helper

(defun graph-git--repo-root (&optional dir)
  "Return the Git repo root for DIR or `default-directory'.

Tries `vc-root-dir' first, then looks up .git."
  (let* ((start (or dir default-directory))
         (vc-root (vc-root-dir)))
    (or (and vc-root
             (file-directory-p (expand-file-name ".git" vc-root))
             vc-root)
        (locate-dominating-file start ".git"))))

(defun graph-git--call (dir &rest args)
  "Run git with ARGS in DIR and return trimmed stdout as a string."
  (unless dir
    (error "graph-git--call: DIR is nil"))
  (with-temp-buffer
    (let ((default-directory dir))
      (apply #'process-file "git" nil t nil args))
    (string-trim (buffer-string))))

(defun graph-git--call-raw (dir &rest args)
  "Run git with ARGS in DIR and return raw stdout as a string."
  (unless dir
    (error "graph-git--call-raw: DIR is nil"))
  (with-temp-buffer
    (let ((default-directory dir))
      (apply #'process-file "git" nil t nil args))
    (buffer-string)))

;;; Ref table: commit hash -> list of refs

(defun graph-git--load-ref-table (dir)
  "Return hash-table mapping commit hashes to list of refnames for DIR."
  (let* ((fmt "%(objectname)%x00%(refname)")
         (out (graph-git--call dir
                               "for-each-ref"
                               (concat "--format=" fmt)
                               "refs/heads" "refs/tags" "refs/remotes"))
         (tbl (make-hash-table :test 'equal)))
    (dolist (line (split-string out "\n" t))
      (pcase (split-string line "\x00")
        (`(,hash ,refname)
         (let ((existing (gethash hash tbl)))
           (puthash hash (cons refname existing) tbl)))))
    tbl))

(defun graph-git--commit-face (commit ref-table)
  "Return a face for COMMIT using REF-TABLE.

REF-TABLE maps commit hashes to lists of ref names."
  (let* ((hash (plist-get commit :hash))
         (lane (plist-get commit :lane))
         (lane-key (plist-get commit :lane-key))
         (refs (and ref-table (gethash hash ref-table)))
         (primary (car refs))
         ;; Prefer refs for coloring; otherwise inherit lane color via :lane-key.
         (key (or primary lane-key lane "default")))
    (graph-git--palette-face key)))

;;; Git log format and parsing

(defun graph-git--parse-entry (s)
  "Parse one raw log record S into a commit plist.

Result contains:
  :hash      string
  :parents   list of strings
  :subject   string
  :timestamp integer

Return nil if S looks malformed."
  (let* ((clean (string-trim s))
         (fields (split-string clean "\x1f")))
    (when (>= (length fields) 4)
      (let* ((hash    (string-trim (nth 0 fields)))
             (parents-raw (string-trim (nth 1 fields)))
             (subject (nth 2 fields))
             (ts-str  (nth 3 fields))
             (parents (unless (string-empty-p parents-raw)
                        (cl-loop for p in (split-string parents-raw " ")
                                 for q = (string-trim p)
                                 unless (or (string-empty-p q)
                                            (not (string-match-p "^[0-9a-fA-F]" q)))
                                 collect q))))
        (when (and (stringp hash)
                   (not (string-empty-p hash)))
          (list :hash hash
                :parents parents
                :subject subject
                :timestamp (string-to-number ts-str)))))))

;;; Lane assignment
(defun seq-remove-at (seq pos)
  (seq-concatenate (type-of seq)
                   (seq-subseq seq 0 pos)
                   (seq-subseq seq (1+ pos))))

(defun graph-git--assign-lanes (commits)
  "Assign :lane index and :lane-key to each commit in COMMITS (newest first).

Lane behavior:
- Reuse an existing lane if the commit hash is expected there.
- Otherwise allocate a new lane on the right.
- The first parent inherits the lane (and its color).
- Additional parents create new lanes to the right.
- Lanes are compacted to remove nils and duplicates."
  (let ((lanes '())       ;; expected hashes per column
        (lane-keys '()))  ;; color keys per column (aligned with lanes)
    (mapcar
     (lambda (c)
       (let* ((hash    (plist-get c :hash))
              (parents (plist-get c :parents))
              (pos     (cl-position hash lanes :test #'equal))
              parent-cols)

         ;; Allocate new lane if this commit was not expected.
         (unless pos
           (setq lanes (append lanes (list hash)))
           (setq lane-keys (append lane-keys (list hash)))
           (setq pos (1- (length lanes))))

         ;; Assign lane + inherited color key.
         (plist-put c :lane pos)
         (plist-put c :lane-key (nth pos lane-keys))

         ;; Advance lane state.
         (cond
          ;; No parents: lane terminates.
          ((null parents)
           (setq lanes (seq-remove-at lanes pos))
           (setq lane-keys (seq-remove-at lane-keys pos)))

          ;; One or more parents.
          (t
           (let* ((primary (graph-git--select-primary-parent parents lanes))
                  (rest    (remove primary parents)))

             ;; Primary parent inherits this lane.
             (setf (nth pos lanes) primary)

             ;; Remaining parents create new lanes to the right.
             (let ((insert-at (1+ pos)))
               (dolist (p rest)
                 (setq lanes
                       (append (cl-subseq lanes 0 insert-at)
                               (list p)
                               (cl-subseq lanes insert-at)))
                 (setq lane-keys
                       (append (cl-subseq lane-keys 0 insert-at)
                               (list p)
                               (cl-subseq lane-keys insert-at)))
                 (setq insert-at (1+ insert-at)))))))

         ;; Compact lanes: drop nils and duplicate hashes (keep leftmost).
         (pcase-let ((`(,new-lanes . ,new-keys)
                      (graph-git--compact-lanes lanes lane-keys)))
           (setq lanes new-lanes)
           (setq lane-keys new-keys))

         ;; Record where each parent line continues after this row.
         (setq parent-cols
               (cl-loop for parent in (delete-dups (copy-sequence parents))
                        for col = (cl-position parent lanes :test #'equal)
                        when col
                        collect (cons parent col)))
         (plist-put c :parent-cols parent-cols)

         c))
     commits)))

;;; Build graph from commits

(defun graph-git--build-graph (commits ref-table)
  "Build a `graph-graph' from COMMITS using REF-TABLE for coloring.

COMMITS must have :lane set and be in newest first order (row 0 newest)."
  (let ((g (graph-create))
        (row 0)
        (parent-row-table (make-hash-table :test 'equal)))
    ;; Nodes
    (dolist (c commits)
      (let* ((hash (plist-get c :hash))
             (subject (plist-get c :subject))
             (lane (plist-get c :lane))
             (face (graph-git--commit-face c ref-table)))
        (graph-add-node
         g
         :id hash
         :row row
         :col lane
         :face face
         :tooltip hash
         :data (list :label subject
                     :hash hash
                     :refs (gethash hash ref-table)))
        (plist-put c :row row)
        (puthash hash row parent-row-table)
        (setq row (1+ row))))
    ;; Edges
    (dolist (c commits)
      (let* ((row (plist-get c :row))
             (lane (plist-get c :lane))
             (parents (plist-get c :parents))
             (parent-cols (plist-get c :parent-cols)))
        (dolist (p parents)
          (when (and p (stringp p) (not (string-empty-p p)))
            (let* ((parent-row (gethash p parent-row-table))
                   (parent-commit
                    (cl-find p commits
                             :key (lambda (x) (plist-get x :hash))
                             :test #'equal)))
              (when (and parent-commit (numberp parent-row))
                (let* ((parent-lane (plist-get parent-commit :lane))
                       (col-start lane)
                       (col-mid (or (cdr (assoc p parent-cols)) parent-lane))
                       (path (graph-git--edge-path
                              row col-start parent-row col-mid parent-lane)))
                  (graph-add-edge
                   g
                   :from (plist-get c :hash)
                   :to p
                   :points path
                   :face (graph-git--commit-face c ref-table)))))))))
    g))

;;; Async process plumbing

(defvar graph-git--loading-timer nil)

(defun graph-git--commit-display-subject (commit ref-table)
  "Return the display string for COMMIT, including refs from REF-TABLE."
  (let* ((face (graph-git--commit-face commit ref-table))
         (refs (graph-git--commit-refs commit ref-table))
         (subject (plist-get commit :subject)))
    (concat (or (graph-git--format-ref-labels refs face) "")
            subject)))

(defun graph-git--commit-index-by-hash (commits hash)
  "Return the index of HASH within COMMITS, or nil."
  (cl-position hash commits
               :key (lambda (commit) (plist-get commit :hash))
               :test #'equal))

(defun graph-git--restore-point (commits selected-hash)
  "Move point to SELECTED-HASH in COMMITS, or keep the first line."
  (when commits
    (goto-char (point-min))
    (let ((idx (and selected-hash
                    (graph-git--commit-index-by-hash commits selected-hash))))
      (forward-line (or idx 0)))))

(defun graph-git--render-session (buffer &optional selected-hash)
  "Render BUFFER from its current git session, preserving SELECTED-HASH."
  (with-current-buffer buffer
    (let* ((session graph-git--session)
           (commits (graph-git-session-commits session))
           (ref-table (graph-git-session-ref-table session))
           (commits (graph-git--assign-lanes commits))
           (graph (graph-git--build-graph commits ref-table)))
      (graph-git--insert-graph-buffer commits graph buffer)
      (setq-local graph-git--commits commits)
      (graph-git--restore-point commits selected-hash)
      (setq graph-git--current-commit nil)
      (when-let* ((commit (graph-git--commit-at-point)))
        (graph-git--update-info-panel commit)))))

(defun graph-git--session-create (dir buffer)
  "Create and store a new session for DIR in BUFFER."
  (with-current-buffer buffer
    (setq graph-git--session
          (make-graph-git-session
           :dir dir
           :proc nil
           :partial ""
           :commits nil
           :batch nil
           :ref-table (graph-git--load-ref-table dir)
           :buffer buffer
           :skip 0
           :loading nil
           :exhausted nil))))

(defun graph-git--prepare-session-batch (session)
  "Reset SESSION state for a new in-flight batch."
  (setf (graph-git-session-partial session) ""
        (graph-git-session-batch session) nil
        (graph-git-session-loading session) t))

(defun graph-git--finalize-batch (session)
  "Merge the in-flight batch into SESSION and return the new batch."
  (let ((partial (graph-git-session-partial session)))
    (when (and partial (string-match-p "\\S-" partial))
      (let ((entry (graph-git--parse-entry partial)))
        (when entry
          (push entry (graph-git-session-batch session)))))
    (setf (graph-git-session-partial session) "")
    (let* ((batch (nreverse (graph-git-session-batch session)))
           (count (length batch)))
      (setf (graph-git-session-batch session) nil
            (graph-git-session-commits session)
            (append (graph-git-session-commits session) batch)
            (graph-git-session-skip session)
            (+ (graph-git-session-skip session) count)
            (graph-git-session-loading session) nil
            (graph-git-session-exhausted session)
            (< count graph-git-page-size))
      batch)))

(defun graph-git--process-filter (proc chunk)
  "PROC filter for async git log.

CHUNK is a string from Git; we buffer until we see NULs, then parse."
  (let ((buf (process-get proc 'graph-git-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((session graph-git--session)
               (partial (graph-git-session-partial session))
               (text (concat partial chunk))
               (parts (split-string text "\0"))
               (new-partial (car (last parts)))
               (complete (butlast parts)))
          (setf (graph-git-session-partial session) new-partial)
          (dolist (rec complete)
            (let ((entry (graph-git--parse-entry rec)))
              (when entry
                (push entry (graph-git-session-batch session))))))))))

(defun graph-git--insert-graph-buffer (commits graph buffer)
  "Fill BUFFER with COMMITS text and SVG slices in the margin of the GRAPH."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (session graph-git--session))
      (erase-buffer)
      (graph-git-view-mode)
      (setq-local graph-git--session session)
      (graph-git--ensure-info-window)

      ;; The margin SVGs must match the visual line height exactly
      (setq-local line-spacing 0)

      (let* ((n (length commits))
             (ref-table (graph-git-session-ref-table graph-git--session))
             ;; Measure the real pixel line height in this buffer
             (line-h (or (line-pixel-height) graph-svg-y-step)))

        ;; Use the measured line height as vertical step for the SVG
        (let* ((graph-svg-y-step line-h)
               ;; Render the whole graph once
               (render (graph-svg-render graph))
               (dims (cdr render))
               (px-width (car dims))
               ;; Precompute one image per row, reusing the same render
               (row-images
                (cl-loop for row from 0 below n
                         collect (graph-svg-slice-row graph row render)))
               ;; Margin width in columns based on pixel width
               (margin-cols
                (ceiling (/ px-width (float (frame-char-width))))))

          (setq-local left-margin-width margin-cols)

          ;; Insert commit subjects, each prefixed with its margin image
          (dotimes (i n)
            (let ((commit (nth i commits)))
              (insert
               (propertize " "
                           'display `(margin left-margin ,(nth i row-images))))
              (insert (graph-git--commit-display-subject commit ref-table) "\n"))))))))

(defun graph-git--maybe-load-more ()
  "Load another page when point nears the end of the loaded history."
  (when (and (eq major-mode 'graph-git-view-mode)
             graph-git--session
             (not (graph-git-session-loading graph-git--session))
             (not (graph-git-session-exhausted graph-git--session)))
    (let* ((win (or (get-buffer-window (current-buffer) 0)
                    (selected-window)))
           (visible-end (if (window-live-p win)
                            (window-end win t)
                          (point)))
           (trigger-line (max (line-number-at-pos)
                              (line-number-at-pos visible-end)))
           (total-lines (count-lines (point-min) (point-max))))
      (when (>= trigger-line
                (max 1 (- total-lines graph-git-load-more-threshold)))
        (unless (timerp graph-git--load-more-timer)
          (setq graph-git--load-more-timer
                (run-with-idle-timer
                 graph-git-load-more-idle-delay
                 nil
                 #'graph-git--run-load-more-if-needed
                 (current-buffer))))))))

(defun graph-git-load-more ()
  "Load the next page of commits into the current graph buffer."
  (interactive)
  (graph-git--cancel-load-more-timer)
  (unless (and graph-git--session
               (eq major-mode 'graph-git-view-mode))
    (user-error "Not in a graph-git buffer"))
  (unless (graph-git-session-exhausted graph-git--session)
    (graph-git--start-async-log (graph-git-session-dir graph-git--session)
                                (current-buffer)))
  (when (graph-git-session-exhausted graph-git--session)
    (message "graph-git: no more commits to load")))

(defun graph-git-refresh ()
  "Reload the git graph for the current buffer from scratch."
  (interactive)
  (unless (and graph-git--session
               (eq major-mode 'graph-git-view-mode))
    (user-error "Not in a graph-git buffer"))
  (let ((dir (graph-git-session-dir graph-git--session)))
    (setq graph-git--commits nil
          graph-git--current-commit nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Refreshing\n"))
    (graph-git--start-async-log dir (current-buffer) t)))

(defun graph-git--process-sentinel (proc event)
  "Sentinel for async EVENT git log PROC."
  (let ((buf (process-get proc 'graph-git-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((session graph-git--session))
          (cond
           ((string-match "finished" event)
            (let* ((selected-hash (plist-get (graph-git--commit-at-point) :hash))
                   (batch (graph-git--finalize-batch session)))
              (when (timerp graph-git--loading-timer)
                (cancel-timer graph-git--loading-timer))
              (setq graph-git--loading-timer nil)
              (message "graph-git: loaded %d commits (%d total)"
                       (length batch)
                       (length (graph-git-session-commits session)))
              (graph-git--render-session buf selected-hash)
              (unless selected-hash
                (goto-char (point-min)))
              (unless (get-buffer-window buf)
                (pop-to-buffer buf))))
           ((or (string-match "exited abnormally" event)
                (string-match "failed" event))
            (setf (graph-git-session-loading session) nil)
            (message "graph-git: git log failed: %s" (string-trim event)))))))))

(defun graph-git--start-async-log (dir buffer &optional reset)
  "Start async `git log` in DIR, associated with BUFFER.

If RESET is non-nil, discard any currently loaded history first."
  (with-current-buffer buffer
    (when (or reset (null graph-git--session))
      (graph-git--session-create dir buffer))
    (let ((session graph-git--session))
      (when (process-live-p (graph-git-session-proc session))
        (user-error "graph-git: already loading"))
      (graph-git--prepare-session-batch session)
      (let* ((default-directory dir)
             (skip (graph-git-session-skip session))
             (cmd `("git" "--no-pager" "log" "--all" "--date-order"
                    ,(format "--skip=%d" skip)
                    ,(format "--max-count=%d" graph-git-page-size)
                    "-z" "--pretty=format:%H%x1f%P%x1f%s%x1f%ct"))
             (proc (make-process
                    :name "graph-git-log"
                    :buffer nil
                    :command cmd
                    :filter #'graph-git--process-filter
                    :sentinel #'graph-git--process-sentinel
                    :noquery t)))
        (process-put proc 'graph-git-buffer buffer)
        (setf (graph-git-session-proc session) proc)
        proc))))

;;;###autoload
(defun graph-git-show (&optional dir)
  "Show git history of DIR as an SVG graph plus text, asynchronously.

If DIR is nil, use the Git root of the current buffer.

This starts an async `git log` process.  When the process finishes, the
history graph is rendered as an SVG in the left margin of a `*Git Graph*`
buffer, with commit subjects as normal text lines."
  (interactive)
  (let* ((repo (graph-git--repo-root dir)))
    (unless repo
      (user-error "Not inside a Git repository: %S" default-directory))
    (let ((buf (get-buffer-create graph-git-buffer-name))
          (max-dots 3)
          (count 0))
      (pop-to-buffer buf)

      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (graph-git-view-mode)
          (insert "Loading\n")))

      (setq graph-git--loading-timer
            (run-at-time
             0 0.3
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((inhibit-read-only t))
                     (save-excursion
                       (goto-char (point-min))
                       (forward-line 0)
                       (delete-region (line-beginning-position) (line-end-position))
                       (insert (format "Loading%s" (make-string count ?.))))
                     (setq count (mod (1+ count) (1+ max-dots)))))))))
      (graph-git--start-async-log repo buf t))))

(provide 'graph-git)

;;; graph-git.el ends here
