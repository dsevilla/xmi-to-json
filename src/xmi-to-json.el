;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; xmi-to-json.el --- JSON producing from XML s-exp

;;; Commentary:
;;
(eval-when-compile
  (require 'cl))
(require 'xml)
(when (> emacs-major-version 23)
  (require 'ert))

(defun xj-json-print (xml)
  "Prints the XML s-exp (as returned by `xml-parse-file') as a JSON string.

The json package can be used to build an AST for it. It assumes
that there is only one root. I'll check that afterwards."
  (insert ?\{ ?\  ?\" (symbol-name (caar xml)) ?\" ?\  ?: ?\  )
  (xj--json-print-internal (car xml)) ;; only one root
  (insert ?\  ?}))

(defun xj--print-attributes (xml)
  (let ((attrs (xml-node-attributes xml)))
    (while attrs
      (insert ?\" (symbol-name (caar attrs)) ?\" ?\  ?:
              ?\  ?\" (cdar attrs) ?\" )
      (setq attrs (cdr attrs))
      (when attrs
        (insert ?\  ?\, ?\  )))))

(defun xj-walk (path xml)
  "Walk an XMI tree and return the element named by PATH. PATH will be /x/y/z."
  (xj--walk (split-string path "/" t) (car xml)))

(defun xj--walk (path xml)
  (if (null path)
      (xj--json-print-one xml)
    (let* ((children (xml-node-children xml))
           (collection-name (intern (pop path)))
           (collection ;; This is slow, but works as a prototype
            (remove-if #'(lambda (e)
                           (not (and (listp e)
                                     (eq (car e) collection-name)))) children))
           (element-name (when path (string-to-number (pop path)))))
      (if element-name
          (xj--walk path (elt collection element-name))
        ;; Else, we reached the end of the path,
        ;; so return the complete collection
        (insert ?\[ ?\  )
        (xj--json-print-one (car collection))
        (mapc #'(lambda (e)
                  (insert ?\n ?, ?\  )
                  (xj--json-print-one e))
              (cdr collection))
        (insert ?\  ?\])))))

(defun xj--json-print-one (xml)
  (let (children child-name insert-comma)
    ;; Start an object
    (insert ?\{ ?\  )

    ;; Attributes
    (xj--print-attributes xml)

    ;; Separator. Due to the fact that we mix two sets that may be
    ;; empty, this test must be done
    (setq insert-comma (xml-node-attributes xml))

    ;; Children
    (setq children (xml-node-children xml))

    ;; Process each child in order. Order is important.
    (while children
      (let ((child (car children)))
        (when (listp child)
          (unless (eq (xml-node-name child) child-name)
            (when insert-comma
              (insert ?\n ?\,))
            (setq child-name (xml-node-name child))
            (insert ?\" (symbol-name child-name) ?\" ?\  ?: ?\  )
            (insert ?\{ ?\  ?\" "_link" ?\" ?:
                    ?\" (symbol-name child-name) ?\" ?} ?\  ))

          ;; Always insert comma after the first element
          (setq insert-comma t)))
      (setq children (cdr children)))

    ;; End of an object
    (insert ?\  ?} ?\  )))

(defun xj--json-print-internal (xml)
  (let (children child-name insert-comma)
    ;; Start an object
    (insert ?\{ ?\  )

    ;; Attributes
    (xj--print-attributes xml)

    ;; Separator. Due to the fact that we mix two sets that may be
    ;; empty, this test must be done
    (setq insert-comma (xml-node-attributes xml))

    ;; Children
    (setq children (xml-node-children xml))

    ;; Process each child in order. Order is important.
    (while children
      (let ((child (car children)))
        (when (listp child)
          (if (eq (xml-node-name child) child-name)
              (when insert-comma
                (insert ?\n ?\,))
            ;; End the array if started. Start of a new reference
            (when child-name
              (insert ?\] ?\n))

            (when insert-comma
              (insert ?\n ?\,))

            (setq child-name (xml-node-name child))
            (insert ?\" (symbol-name child-name) ?\" ?\  ?: ?\  )
            (insert ?\[ ?\  ))

          (xj--json-print-internal child)
          ;; Always insert comma after the first element
          (setq insert-comma t)))
      (setq children (cdr children)))

    ;; If there were any children, end the last square bracket
    (when (xml-node-children xml)
      (insert ?\] ?\  ))

    ;; End of an object
    (insert ?\  ?} ?\  )))

(when (> emacs-major-version 23)
  (defmacro xj-ert-deftest (name &rest opt-docstring-body)
    (declare (indent 2)
             (doc-string 3))
    (let ((opt-db opt-docstring-body))
      `(ert-deftest ,name ()
         ,@(when (stringp (car opt-db))
             (list (pop opt-db)))
         (require 'json)
         ,@opt-db)))

  (xj-ert-deftest test1
      "docstring!"
    (should
     (equal

      1
      1)))
  )

(provide 'xmi-to-json)

;;; xmi-to-json.el ends here
