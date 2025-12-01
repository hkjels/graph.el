;;; graph-svg.el --- SVG renderer for graph.el  -*- lexical-binding: t; -*-

;; Author: Henrik Kjerringv+Ñg <henrik@kjerringvag.no>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (graph "0.1") (cl-lib "0.5"))
;; Keywords: graph, svg
;; URL: https://github.com/hkjels/graph.el

;;; Commentary:

;; This library provides an SVG backend for `graph.el`.
;;
;; It takes a `graph-graph` object and produces an SVG image where:
;;
;; - Each node is a circle on a regular grid (row, col).
;; - Each edge is drawn as a line segment.
;; - Nodes are spaced vertically in a way that roughly matches line height,
;;   so SVG nodes can visually align with buffer lines.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'graph)

(defgroup graph-svg nil
  "SVG rendering for graph.el graphs."
  :group 'tools)

(defcustom graph-svg-x-step 14
  "Horizontal spacing in pixels between graph columns."
  :type 'integer
  :group 'graph-svg)

(defcustom graph-svg-y-step 18
  "Vertical spacing in pixels between graph rows.

Try to make this close to your default line height
so SVG nodes visually align with text lines."
  :type 'integer
  :group 'graph-svg)

(defcustom graph-svg-padding 6
  "Padding in pixels around the whole graph."
  :type 'integer
  :group 'graph-svg)

(defcustom graph-svg-node-radius 3.5
  "Radius in pixels of node circles."
  :type 'number
  :group 'graph-svg)

(defcustom graph-svg-edge-width 1.2
  "Stroke width of edges in pixels."
  :type 'number
  :group 'graph-svg)

(defcustom graph-svg-edge-curve-radius 8
  "Radius in pixels used when rounding L shaped edges.

This is the distance along incoming and outgoing directions used
to place the cubic Bezier control points."
  :type 'number
  :group 'graph-svg)

;;; Face / color helpers

(defun graph-svg--normalize-face (face)
  "Normalize FACE to a face symbol or nil.

FACE may be:
  - a face symbol
  - a plist that contains :face or :foreground
  - nil."
  (cond
   ((facep face) face)
   ((and (listp face)
         (plist-member face :face))
    (let ((f (plist-get face :face)))
      (and (facep f) f)))
   (t nil)))

(defun graph-svg--face-color (face &optional attr default)
  "Return color string for FACE attribute ATTR or DEFAULT.

If ATTR is nil, use :foreground.
FACE may be a face symbol or a plist.  Fallback is DEFAULT or \"#000\"."
  (let* ((attr (or attr :foreground))
         (face-symbol (graph-svg--normalize-face face))
         (value
          (cond
           (face-symbol
            (face-attribute face-symbol attr nil 'default))
           ((and (listp face)
                 (plist-member face attr))
            (plist-get face attr))
           (t nil))))
    (cond
     ((and value
           (not (memq value '(unspecified unspecified-fg unspecified-bg)))
           (not (and (stringp value)
                     (string-prefix-p "unspecified" value))))
      value)
     (default default)
     (t "#000"))))

;;; Coordinate helpers

(defun graph-svg--bounds (graph)
  "Return (COLS . ROWS) for GRAPH from its nodes and edges."
  (graph--compute-bounds graph))

(defun graph-svg--node-pixel-coords (row col)
  "Convert grid ROW, COL into pixel coordinates (X . Y)."
  (cons (+ graph-svg-padding (* col graph-svg-x-step))
        (+ graph-svg-padding (* row graph-svg-y-step))))

(defun graph-svg--edge-point-pixel-coords (pts index)
  "Convert PTS at INDEX into pixel coordinates for edge rendering."
  (let* ((pt (nth index pts))
         (row (car pt))
         (col (cdr pt)))
    (graph-svg--node-pixel-coords row col)))

;;; Drawing

(defun graph-svg--draw-edge (svg edge)
  (let* ((pts (graph-edge-points edge))
         (face (graph-edge-face edge))
         (color (graph-svg--face-color face :foreground "#555"))
         (points (cl-loop for i from 0 below (length pts)
                          collect (graph-svg--edge-point-pixel-coords pts i))))
    ;; Draw the full edge as one polyline so diagonals stay continuous and do
    ;; not inherit per-segment cap overshoot.
    (svg-polyline svg points
                  :stroke color
                  :stroke-width graph-svg-edge-width
                  :stroke-linecap "butt"
                  :stroke-linejoin "round"
                  :fill "none")))

(defun graph-svg--draw-node (svg node)
  "Draw NODE into SVG as a filled circle."
  (let* ((row (graph-node-row node))
         (col (graph-node-col node))
         (xy  (graph-svg--node-pixel-coords row col))
         (x   (car xy))
         (y   (cdr xy) )
         (face  (graph-node-face node))
         (node-color (graph-svg--face-color face :foreground "#000")))
    (svg-circle svg x y graph-svg-node-radius
                :fill node-color
                :stroke node-color)))

;;; Top-level rendering and display

(defun graph-svg-render (graph)
  "Render GRAPH into an SVG object.

Return (SVG . (WIDTH . HEIGHT)) where WIDTH and HEIGHT are pixels."
  (cl-destructuring-bind (cols . rows)
      (graph-svg--bounds graph)
    (let* ((width  (max 1 (+ (* (max 1 cols) graph-svg-x-step)
                             (* 2 graph-svg-padding))))
           (height (max 1 (+ (* (max 1 rows) graph-svg-y-step)
                             (* 2 graph-svg-padding))))
           (svg (svg-create width height)))
      (dolist (edge (graph-graph-edges graph))
        (graph-svg--draw-edge svg edge))
      (dolist (node (graph-graph-nodes graph))
        (graph-svg--draw-node svg node))
      (cons svg (cons width height)))))

(defun graph-svg-image (graph)
  "Return an image object for GRAPH's SVG."
  (let* ((render (graph-svg-render graph))
         (svg    (car render)))
    (svg-image svg)))

;;; Geometric slicing: extract one row from a full SVG

(defun graph-svg--clone-node (x)
  "Deep clone of SVG dom structure X.

Handles:
  * cons cells
  * plists
  * alists ( (key . val) ... )
  * atomic values"
  (cond
   ;; Alist entry (key . val)
   ((and (consp x) (atom (car x)))
    (cons (car x) (graph-svg--clone-node (cdr x))))

   ;; General cons (trees, child node lists, plist pairs)
   ((consp x)
    (cons (graph-svg--clone-node (car x))
          (graph-svg--clone-node (cdr x))))

   ;; Atoms copied directly
   (t x)))

(defun graph-svg--clip-svg (svg y1 y2)
  "Return a new SVG that displays only the vertical region [Y1, Y2) of SVG.
SVG is the usual (svg ATTRS CHILD...) structure."
  (let* ((tag (car svg))
         (attrs (copy-tree (cadr svg))) ;; ((width . X) (height . Y) ...)
         (children (cddr svg))
         (slice-height (max 1 (- y2 y1)))
         (clip-id (format "clip-%d-%d" y1 y2))
         (defs
          `(defs nil
                 (clipPath ((id . ,clip-id))
                           (rect ((x . "0")
                                  (y . "0")
                                  (width . "100%")
                                  (height . ,(number-to-string slice-height)))))))
         (group
          `(g ((clip-path . ,(format "url(#%s)" clip-id)))
              (g ((transform . ,(format "translate(0,%s)" (- y1))))
                 ,@children))))
    (setf (alist-get 'height attrs nil nil #'eq) slice-height)
    (list tag attrs defs group)))

(defun graph-svg-slice-row (graph row &optional render)
  "Return an `svg-image` object showing only ROW of GRAPH.

If RENDER is non-nil it should be the result of `graph-svg-render`,
which avoids recomputing the full SVG."
  (let* ((render (or render (graph-svg-render graph)))
         (svg (car render))
         (y-step graph-svg-y-step)
         (center (+ graph-svg-padding (* row y-step)))
         ;; Use exact, non-overlapping row bands so each stroke segment is
         ;; rasterized once instead of being duplicated in adjacent row images.
         (half-top (/ y-step 2))
         (half-bottom (- y-step half-top))
         (y1 (- center half-top))
         (y2 (+ center half-bottom))
         (slice (graph-svg--clip-svg svg y1 y2)))
    (svg-image slice :ascent 'center)))

(provide 'graph-svg)

;;; graph-svg.el ends here
