;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; xmi-json-rest-server.el --- JSON producing from XML s-exp

;;; Commentary:
;;
(eval-when-compile
  (require 'cl))
(require 'xmi-to-json)
(require 'elnode)

(defvar *xjrs--model-mapping* (make-hash-table)
  "Holds the map between the symbol defining this model and the XML s-exp serving it.")

(defun xjrs-add-mapping (xmi)
  "Adds a mapping between a symbol (the root of a model) and an XML s-exp."
  (puthash (caar xmi) xmi *xjrs--model-mapping*))

(defun xjrs-del-mapping (sym)
  (remhash sym *xjrs--model-mapping*))

(defvar *xjrs--app-routes*
  '(("^.*//m/\\(.*\\)" . xjrs--rest-handler)))

(defun xjrs--rest-handler (httpcon)
  (let* ((resource (elnode-http-mapping httpcon 1))
         (elements (split-string resource "/" t))
         json sym xml)
    (when elements
      (setq sym (intern (car elements)))
      (setq xml (gethash sym *xjrs--model-mapping*))
      (setq json (with-temp-buffer
                   (xj-walk (cdr elements) xml)
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (if (and json (not (string-equal ""  json)))
        (progn
          (elnode-http-start httpcon 200 '("Content-type" . "application/json"))
          (elnode-http-return httpcon json))
      (elnode-send-404 httpcon "Model or node not found!"))))

(defun xjrs--root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon *xjrs--app-routes*))

(defun xjrs-start-rest-server ()
  "Start a JSON REST server for models at port 4321."
  (interactive)
  (elnode-start 'xjrs--root-handler :port 4321))

(defun xjrs-stop-rest-server ()
  "Stop the JSON REST server for models."
  (interactive)
  (elnode-stop 4321))

(provide 'xmi-json-rest-server)

;;; xmi-json-rest-server.el ends here
