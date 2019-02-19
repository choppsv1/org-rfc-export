;;
;; February 19 2019, Christian Hopps <chopps@gmail.com>
;;
;; Copyright (c) 2019 by Christian E. Hopps
;; All rights reserved.
;;

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)                ;; Initialize & Install Package
(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(package-initialize)                ;; Initialize & Install Package
(package-install 'org-plus-contrib)            ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package
