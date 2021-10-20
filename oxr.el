;;; oxr.el --- experimental cross-refernces for org -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; An experimental package to enhance cross-reference support in org.
;;;
;;; Code:

(require 'org-element)

(declare-function 'org-element-property "ext:org")

(defcustom oxr-group-ui t
  "Group cross-reference targets by type.

In practice, if using Emacs 28, this allows you to turn off
grouping, and add the target type to the annotation instead."
  :group 'oxr
  :type 'boolean)

(defface oxr-target
  '((t :foreground "wheat1"))
  "Face for oxr targets."
  :group 'oxr)

(defface oxr-annotation
  '((t :inherit font-lock-doc-face))
  "Face for oxr annotations."
  :group 'oxr)

;; As this is just experimental, and I don't care about math, this currently
;; only supports figures and tables.
;;
;; Should be easy enough to extend the list of types though.
(defvar oxr-types '((figure . "fig")
                    (table . "tab")))
(defvar oxr-create-table-function 'org-table-create)
(defvar oxr-insert-ref-function 'oxr--insert-ref-internal)

;;; cache

(defvar-local oxr-targets (list))

;;; Cross-reference insert functions

(defun oxr-insert-ref ()
  "Insert cross-reference link in buffer."
  (interactive)
  (funcall oxr-insert-ref-function))

(defun oxr--insert-ref-internal ()
  "Insert cross-reference in buffer as internal link."
  (let ((target (oxr-select-targets)))
    (org-insert-link 'internal target)))

(defun oxr--insert-ref-typed ()
  "Insert cross-reference in buffer as org-ref compatible typed link."
  (let ((type (completing-read "Cross-reference type: " oxr-types)))
    (let* ((target (oxr-select-targets type))
          (link-type (pcase type
                       ("table" "tab")
                       ("figure" "fig")
                       ("section" "sec")))
          (typed-target (concat link-type ":" target)))
      (org-insert-link 'typed typed-target typed-target))))

(defun oxr-insert-annotate (target)
  "Annotate the cross-reference TARGET with type and caption."
  (let* ((caption (get-text-property 0 'oxr--caption target))
         (target-type (get-text-property 0 'oxr--type target)))
    (propertize
     (concat (unless oxr-group-ui (truncate-string-to-width target-type 15 0 ?\s)) caption)
     'face 'oxr-annotation)))

(defun oxr--insert-group (target transform)
  "Function to group or TRANSFORM cross-reference candidates by TARGET type."
  (if transform target (get-text-property 0 'oxr--type target)))

(defun oxr-select-targets (&optional type)
  "Select cross-reference target for TYPE."
  (let* ((targets (oxr-parse-targets type))
         (choice
          (if targets
              (completing-read
               "Cross-reference target: "
               (lambda (string predicate action)
                 (if (eq action 'metadata)
                     `(metadata
                       (annotation-function . oxr-insert-annotate)
                       (group-function . ,(when oxr-group-ui #'oxr--insert-group))
                       (category . cross-reference))
                   (complete-with-action action targets string predicate))))
            (error "No cross-reference targets"))))
    (string-trim choice)))

(defun oxr--extract-string (thing)
  "Peel off 'car's from a nested list THING until the car is a string."
  ;; from https://orgmode.org/worg/exporters/beamer/beamer-dual-format.html
  (while (and thing (not (stringp thing)))
    (setq thing (car thing)))
  (substring-no-properties thing))

(defun oxr-parse-targets (&optional _type)
  "Parse cross-reference targets from org buffer, optionally by TYPE."
  (let ((org-tree (org-element-parse-buffer)))
    (org-element-map org-tree '(table link)
      (lambda (target)
        (let* ((el-type (org-element-type target))
               (target-type (pcase el-type
                              ('table "table")
                              (_ "figure")))
               (parent (car (cdr (org-element-property :parent target))))
               (name (pcase el-type
                       ('table (org-element-property :name target))
                       (_ (plist-get parent :name))))
               (caption (oxr--extract-string
                         (or (org-element-property :caption target)
                             (plist-get parent :caption)
                             ""))))
          (when name (oxr--make-candidate name caption target-type)))))))

(defun oxr--make-candidate (name caption type)
  "Make candidate string with NAME, CAPTION, TYPE."
  (propertize
   (truncate-string-to-width (concat " " name) 40 0 ?\s)
   'face 'oxr-target
   ;; Attach metadata to target candidate string.
   'oxr--caption caption
   'oxr--type type))

;;; Convenience functions for inserting new cross-reference targets (tables and figures)

(defun oxr-insert-table ()
  "Insert a new table, with name and caption."
  (interactive)
  (insert (oxr-metadata-prompt (oxr-get-name-prefix 'table)))
  (funcall oxr-create-table-function))

(defun oxr-insert-figure ()
  "Insert a new figure, with name and caption."
  (interactive)
  ;; TODO this inserts absolute paths ATM, which is not ideal.
  (let ((image_file (read-file-name "Image file: " "~/Pictures/")))
    (insert (oxr-metadata-prompt (oxr-get-name-prefix 'figure)))
    (org-insert-link 'file image_file)))

(defun oxr-insert-name ()
  "Insert name for cross-referencing."
  (interactive)
  (let* ((object-type
          (completing-read "Object type: " oxr-types))
         (name (read-from-minibuffer "Name: ")))
    (insert (concat (oxr-get-name-prefix object-type)) "-" name)))

(defun oxr-metadata-prompt (name-prefix)
  "Prompt user for name with NAME-PREFIX and caption."
  (let ((name (read-from-minibuffer "Name: "))
        (caption (read-from-minibuffer "Caption: ")))
    (concat "#+caption: " caption "\n#+name: " name-prefix "-" name "\n")))

(defun oxr-get-name-prefix (type)
  "Get the name prefix for TYPE from 'oxr-types'."
  (cdr (assoc type oxr-types)))

(provide 'oxr)
;;; oxr.el ends here
