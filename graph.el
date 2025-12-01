;;; graph.el --- Simple interactive grid based graphs  -*- lexical-binding: t; -*-

;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))
;; Keywords: graph, visualization
;; URL: https://github.com/hkjels/graph.el

;;; Commentary:

;; This library provides a simple, task agnostic way to draw small, grid
;; based graphs inside an Emacs buffer.
;;
;; Features:
;; - Nodes at discrete (row, col) positions
;; - Edges as polylines through grid points
;; - Per node and per edge faces and tooltips
;; - Nodes aligned to buffer lines so external text can be placed beside
;;
;; The library does not compute layouts.  Callers supply rows, columns and
;; edge polylines.

;;; Code:

(require 'cl-lib)

(defgroup graph nil
  "Grid based graphs for Emacs."
  :group 'tools)

(defface graph-node-face
  '((t :inherit default))
  "Default face for graph nodes."
  :group 'graph)

(defface graph-edge-face
  '((t :inherit shadow))
  "Default face for graph edges."
  :group 'graph)

(defcustom graph-default-node-char ?o
  "Default character used to render nodes."
  :type 'character
  :group 'graph)

(defcustom graph-default-vertical-char ?|
  "Default character used for vertical edges."
  :type 'character
  :group 'graph)

(defcustom graph-default-horizontal-char ?-
  "Default character used for horizontal edges."
  :type 'character
  :group 'graph)

(defcustom graph-default-diag-up-char ?/
  "Default character used for edges going bottom left to top right."
  :type 'character
  :group 'graph)

(defcustom graph-default-diag-down-char ?\\
  "Default character used for edges going top left to bottom right."
  :type 'character
  :group 'graph)

(cl-defstruct (graph-node
               (:constructor graph-node-create))
  "A node in a graph.

FIELDS

- ID: arbitrary identifier (symbol, string, number).
- ROW, COL: integer grid coordinates, zero based.
- CHAR: character used to draw the node.
- FACE: face or plist for `face` text property.
- TOOLTIP: string for `help-echo`.
- KEYMAP: keymap installed on node cell.
- DATA: arbitrary extra data for callers."
  id row col char face tooltip keymap data)

(cl-defstruct (graph-edge
               (:constructor graph-edge-create))
  "An edge in a graph.

FIELDS

- FROM, TO: node IDs or arbitrary identifiers, not interpreted here.
- POINTS: non empty list of (ROW . COL) grid points forming a polyline.
          At least two points.
- CHAR: base character used to draw segments.
- FACE: face or plist for `face` text property.
- TOOLTIP: string for `help-echo` along the edge.
- KEYMAP: keymap installed on edge cells.
- DATA: arbitrary extra data for callers."
  from to points char face tooltip keymap data)

(cl-defstruct (graph-graph
               (:constructor graph-graph-create))
  "A graph made of nodes and edges.

FIELDS

- NODES: list of `graph-node` structs.
- EDGES: list of `graph-edge` structs.
- NODE-TABLE: hash table from node ID to node.
- WIDTH, HEIGHT: cached dimensions of last render, or nil."
  nodes edges node-table width height)

;;; Public construction API

(cl-defun graph-create ()
  "Create an empty graph object."
  (graph-graph-create
   :nodes nil
   :edges nil
   :node-table (make-hash-table :test #'equal)
   :width nil
   :height nil))

(cl-defun graph-add-node (graph &key id row col
                                (char graph-default-node-char)
                                face tooltip keymap data)
  "Add a node to GRAPH and return it.

ID is required and should be unique inside GRAPH.
ROW and COL are zero based integer coordinates.
CHAR is the character used to represent the graph item.

Optional FACE, TOOLTIP, KEYMAP and DATA are stored on the node
and used at render time."
  (unless (and id (integerp row) (integerp col))
    (error "graph-add-node: Id, row and col are required"))
  (let* ((node (graph-node-create
                :id id
                :row row
                :col col
                :char char
                :face (or face 'graph-node-face)
                :tooltip tooltip
                :keymap keymap
                :data data))
         (tbl (graph-graph-node-table graph)))
    (puthash id node tbl)
    (setf (graph-graph-nodes graph)
          (cons node (graph-graph-nodes graph)))
    node))

(cl-defun graph-add-edge (graph &key from to points
                                char face tooltip keymap data)
  "Add an edge to GRAPH and return it.

FROM and TO are node IDs or arbitrary identifiers.

POINTS must be a list of (ROW . COL) grid coordinates describing
a polyline, with at least two points.  The library draws straight
segments between consecutive points.

Optional CHAR overrides the default edge characters.  Since edges
may include vertical, horizontal and diagonal parts, CHAR acts as
a fallback when no specific shape is chosen.

Optional FACE, TOOLTIP, KEYMAP and DATA attach styling and metadata."
  (unless (and (consp points)
               (consp (cdr points)))
    (error "graph-add-edge: Need at least two points"))
  (let ((edge (graph-edge-create
               :from from
               :to to
               :points points
               :char char
               :face (or face 'graph-edge-face)
               :tooltip tooltip
               :keymap keymap
               :data data)))
    (setf (graph-graph-edges graph)
          (cons edge (graph-graph-edges graph)))
    edge))

;;; Rendering

(defvar-local graph--current-graph nil
  "Buffer local graph currently shown in `graph-view-mode'.")

(defun graph-current-graph ()
  "Return the graph currently associated with the buffer.

Signals an error if the current buffer is not in `graph-view-mode'."
  (unless (derived-mode-p 'graph-view-mode)
    (error "Not in a graph-view-mode buffer"))
  graph--current-graph)

(defun graph--compute-bounds (graph)
  "Compute width and height for GRAPH from its nodes and edges.

Return (WIDTH . HEIGHT)."
  (let ((max-row -1)
        (max-col -1))
    (dolist (node (graph-graph-nodes graph))
      (setq max-row (max max-row (graph-node-row node)))
      (setq max-col (max max-col (graph-node-col node))))
    (dolist (edge (graph-graph-edges graph))
      (dolist (pt (graph-edge-points edge))
        (setq max-row (max max-row (car pt)))
        (setq max-col (max max-col (cdr pt)))))
    (cons (1+ max-col) (1+ max-row))))

(defun graph--make-grid (width height)
  "Create a HEIGHT by WIDTH grid as a vector of vectors.

Each cell is initialized to nil and later becomes a plist:
  (:char CHAR :face FACE :tooltip STRING :keymap KEYMAP :kind KIND ...)."
  (let ((rows (make-vector height nil)))
    (dotimes (r height)
      (let ((cols (make-vector width nil)))
        (aset rows r cols)))
    rows))

(defun graph--grid-ref (grid row col)
  "Value in GRID from ROW & COL."
  (aref (aref grid row) col))

(defun graph--grid-set (grid row col value)
  "Set VALUE in GRID at ROW & COL."
  (aset (aref grid row) col value))

(defun graph--merge-cell (existing new)
  "Merge NEW cell plist into EXISTING cell plist.

Currently NEW wins on conflicts.

Both EXISTING and NEW can be nil."
  (cond
   ((null existing) new)
   ((null new) existing)
   (t (append new existing))))

(defun graph--place-node (grid node)
  "Place NODE into GRID."
  (let* ((row (graph-node-row node))
         (col (graph-node-col node))
         (cell (graph--grid-ref grid row col))
         (new (list :kind 'node
                    :node node
                    :char (graph-node-char node)
                    :face (graph-node-face node)
                    :tooltip (graph-node-tooltip node)
                    :keymap (graph-node-keymap node))))
    (graph--grid-set grid row col
                     (graph--merge-cell cell new))))

(defun graph--segment-char (r1 c1 r2 c2 fallback)
  "Choose a character for segment between (R1,C1) and (R2,C2).

FALLBACK is used when no specific shape is chosen."
  (cond
   ((= c1 c2) graph-default-vertical-char)
   ((= r1 r2) graph-default-horizontal-char)
   ((= (- r2 r1) (- c2 c1)) graph-default-diag-down-char)
   ((= (- r2 r1) (- c1 c2)) graph-default-diag-up-char)
   (t (or fallback ?*))))

(defun graph--walk-segment (r1 c1 r2 c2)
  "Return a list of intermediate points for segment from (R1,C1) to (R2,C2).

Includes the end point, but not the start point."
  (let ((dr (cond
             ((< r1 r2) 1)
             ((> r1 r2) -1)
             (t 0)))
        (dc (cond
             ((< c1 c2) 1)
             ((> c1 c2) -1)
             (t 0)))
        (points nil))
    (while (or (/= r1 r2) (/= c1 c2))
      (setq r1 (+ r1 dr)
            c1 (+ c1 dc))
      (push (cons r1 c1) points))
    (nreverse points)))

(defun graph--place-edge (grid edge)
  "Place EDGE into GRID."
  (let* ((pts (graph-edge-points edge))
         (fallback (or (graph-edge-char edge) graph-default-horizontal-char))
         (face (graph-edge-face edge))
         (tooltip (graph-edge-tooltip edge))
         (keymap (graph-edge-keymap edge)))
    (cl-loop
     for (p1 p2) on pts while p2 do
     (let* ((r1 (car p1))
            (c1 (cdr p1))
            (r2 (car p2))
            (c2 (cdr p2))
            (ch (graph--segment-char r1 c1 r2 c2 fallback)))
       (dolist (pt (graph--walk-segment r1 c1 r2 c2))
         (let* ((row (car pt))
                (col (cdr pt))
                (cell (graph--grid-ref grid row col))
                (new (list :kind 'edge
                           :edge edge
                           :char ch
                           :face face
                           :tooltip tooltip
                           :keymap keymap)))
           (graph--grid-set grid row col
                            (graph--merge-cell cell new))))))))

(defun graph--render-into-buffer (graph buffer)
  "Render GRAPH into BUFFER.

Return a cons (WIDTH . HEIGHT) that describes the grid size used."
  (cl-destructuring-bind (width . height)
      (graph--compute-bounds graph)
    (let ((grid (graph--make-grid width height)))
      ;; place nodes and edges
      (dolist (edge (graph-graph-edges graph))
        (graph--place-edge grid edge))
      (dolist (node (graph-graph-nodes graph))
        (graph--place-node grid node))
      ;; write into buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dotimes (row height)
            (let ((line-start (point)))
              (dotimes (col width)
                (let* ((cell (graph--grid-ref grid row col))
                       (ch (or (plist-get cell :char) ?\s))
                       (face (plist-get cell :face))
                       (tooltip (plist-get cell :tooltip))
                       (keymap (plist-get cell :keymap))
                       (props nil))
                  (when face
                    (setq props (plist-put props 'face face)))
                  (when tooltip
                    (setq props (plist-put props 'help-echo tooltip)))
                  (when keymap
                    (setq props (plist-put props 'keymap keymap)))
                  (let ((pos (point)))
                    (insert (char-to-string ch))
                    (when props
                      (add-text-properties pos (1+ pos) props)))))
              (insert "\n")
              ;; Record mapping from row to line beginning as a text property
              ;; so clients can jump easily.
              (add-text-properties
               line-start (point)
               (list 'graph-row row))))
          (setf (graph-graph-width graph) width
                (graph-graph-height graph) height))
        (cons width height)))))

;;; Major mode

(define-derived-mode graph-view-mode special-mode "Graph"
  "Major mode for viewing graphs created by `graph.el'.

Use `graph-view-refresh' after modifying the underlying graph
object."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun graph-view-refresh ()
  "Re-render the graph in the current `graph-view-mode' buffer."
  (interactive)
  (let ((graph (graph-current-graph)))
    (graph--render-into-buffer graph (current-buffer))))

(defun graph-view-set-graph (graph)
  "Associate GRAPH with current buffer and render it.

The current buffer must be in `graph-view-mode'."
  (unless (derived-mode-p 'graph-view-mode)
    (error "Current buffer is not in graph-view-mode"))
  (setq graph--current-graph graph)
  (graph-view-refresh))

(defun graph--goto-row (row)
  "Move point to line whose `graph-row' property equals ROW."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (let* ((line-start (point))
               (props (text-properties-at line-start))
               (this-row (plist-get props 'graph-row)))
          (when (and this-row (= this-row row))
            (setq found line-start))
          (forward-line 1))))
    (when found
      (goto-char found)
      t)))

(defun graph-open (graph &optional buffer-name)
  "Open GRAPH in a `graph-view-mode' buffer and return the buffer.

If BUFFER-NAME is non nil, use that, otherwise use *Graph*."
  (let ((buf (get-buffer-create (or buffer-name "*Graph*"))))
    (with-current-buffer buf
      (graph-view-mode)
      (graph-view-set-graph graph))
    (pop-to-buffer buf)
    buf))

(provide 'graph)

;;; graph.el ends here
