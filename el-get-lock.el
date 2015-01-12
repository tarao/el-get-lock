;;; el-get-lock.el --- Lock El-Get package repository versions

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: https://github.com/tarao/el-get-lock
;; Version: 0.1
;; Package-Requires: ((el-get "5.1"))
;; Keywords: emacs package install

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'el-get)
(eval-when-compile (require 'cl))

;; customization

(defgroup el-get-lock nil "el-get-lock"
  :group 'el-get)

(defcustom el-get-lock-file
  (expand-file-name "el-get.lock" user-emacs-directory)
  "File to store the information of previously installed versions."
  :type 'file
  :group 'el-get-lock)

;; internals

(defvar el-get-lock-package-versions nil)
(defvar el-get-lock-locked-packages nil)
(defvar el-get-lock-unlocked-packages nil)

(defun el-get-lock-save ()
  (with-temp-buffer
    (let ((indent-tabs-mode nil)
          (file (expand-file-name el-get-lock-file)))
      (pp `(setq el-get-lock-package-versions
                 ',el-get-lock-package-versions)
          (current-buffer))
      (pp `(setq el-get-lock-locked-packages
                 ',el-get-lock-locked-packages)
          (current-buffer))
      (pp `(setq el-get-lock-unlocked-packages
                 ',el-get-lock-unlocked-packages)
          (current-buffer))
      (write-region nil nil file))))

(defun el-get-lock-save-package-version (name version)
  (let* ((name (if (stringp name) (intern name) name))
         (pair (assq name el-get-lock-package-versions)))
    (if pair (setcdr pair version)
      (push (cons name version) el-get-lock-package-versions)))
  (el-get-lock-save))

(defun el-get-lock-wrap-package (package)
  (let* ((name (intern (el-get-source-name package)))
         (source (if (listp package) package (el-get-package-def package)))
         (version (cdr (and (or (null el-get-lock-locked-packages)
                                (memq name el-get-lock-locked-packages))
                            (not (memq name el-get-lock-unlocked-packages))
                            (assq name el-get-lock-package-versions)))))
    (if version (append version source) source)))

(defun el-get-lock-wrap-packages (packages)
  (let ((packages
          (loop for p in packages when (listp p) append p else collect p)))
    (append (mapcar #'el-get-lock-wrap-package packages) el-get-sources)))

(defun el-get-lock-read-package-name (action)
  (let* ((packages (el-get-read-all-recipe-names))
         (package (completing-read (format "%s package (default: all): " action)
                                   packages nil t nil nil (list nil))))
    (if (stringp package) (intern package) package)))

;; integration

(defun el-get-lock-track-installed-version (package)
  (let* ((name (el-get-source-name package))
         (checksum (el-get-checksum name))
         (version (and checksum (list :checksum checksum))))
    (when version (el-get-lock-save-package-version name version))))
(add-hook 'el-get-post-install-hooks #'el-get-lock-track-installed-version)
(add-hook 'el-get-post-update-hooks #'el-get-lock-track-installed-version)

(defadvice el-get (around el-get-lock-install-with-lock
                          (&optional sync &rest packages) activate)
  "Fix up PACKAGES to lock their repository versions."
  (let ((el-get-sources (el-get-lock-wrap-packages packages)))
    ad-do-it))

(defadvice el-get-update (around el-get-lock-update-without-lock activate)
  "Disable the effect of `el-get-lock-install-with-lock' advice."
  (let ((el-get-lock-package-versions nil))
    ad-do-it))

;; commands

;;;###autoload
(defun el-get-lock-load ()
  "Load `el-get-lock-file'."
  (interactive)
  (let ((file (expand-file-name el-get-lock-file)))
    (when (file-exists-p file)
      (load file))))

;;;###autoload
(defun el-get-lock (&optional packages with-dependents)
  "Lock El-Get repository versions of PACKAGES.

IF PACKAGES are specified, those PACKAGES are marked to be
locked.  Otherwise, the all installed packages are locked.

IF WITH-DEPENDENTS is non-nil, or the function is called
interactively, packages depended by PACKAGES are also locked.
These packages are marked \"automatically locked\" so that
unlocking the depended package will unlock these packages too.

When `el-get' installs a package for the first time, the
repository version is saved to `el-get-lock-file'.  Next time you
call `el-get' for the package, the repository version of the
package is locked according to the value in the
`el-get-lock-file'."
  (interactive (list (el-get-lock-read-package-name "Lock") t))
  ;; TODO dependents
  (setq packages (el-get-as-list packages))
  (el-get-lock-load)
  (cond
   ;; lock all
   ((null packages)
    ;; lock even if explicitly unlocked
    (setq el-get-lock-unlocked-packages nil
          el-get-lock-locked-packages nil))
   ;; lock specified packages
   (t
    (dolist (package packages)
      ;; lock even if explicitly unlocked
      (setq el-get-lock-unlocked-packages
            (delq package el-get-lock-unlocked-packages))
      (add-to-list 'el-get-lock-locked-packages package))))
  (el-get-lock-save))

;;;###autoload
(defun el-get-lock-unlock (&optional packages)
  "Unlock El-Get repository versions of PACKAGES.

IF PACKAGES are specified, those PACKAGES are marked to be
unlocked.  Otherwise, the all installed packages are unlocked."
  (interactive (list (el-get-lock-read-package-name "Unlock")))
  (setq packages (el-get-as-list packages))
  (el-get-lock-load)
  (cond
   ((null packages)
    (setq el-get-lock-package-versions nil
          el-get-lock-unlocked-packages nil
          el-get-lock-locked-packages nil))
   (t
    (let ((locked-packages el-get-lock-locked-packages))
      (dolist (package packages)
        (setq locked-packages
              (delq package locked-packages))
        (add-to-list 'el-get-lock-unlocked-packages package))
      (when (and el-get-locked-packages (null locked-packages))
        ;; there is no package locked any more; unlock all
        (el-get-lock-unlock)))))
  (el-get-lock-save))

(provide 'el-get-lock)
;;; el-get-lock.el ends here
