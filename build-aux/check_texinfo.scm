#! /usr/bin/env guile
!#
;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

;; NOTE: It's not fair to check GNU Texinfo in configure, since users
;;       don't have to install it unless they want to generate manuals.

(use-modules (ice-9 popen) (ice-9 rdelim) (ice-9 regex))

(let ((m (string-match "texindex [(]GNU texinfo[)] ([0-9.]+)"
                       (read-line (open-pipe "texindex --version" OPEN_READ)))))
  (if m
      (if (string>=? (match:substring m 1) "5.2")
          0
          (error "Please upgrade GNU Texinfo for generating manuals!"))
      (error "Please install the latest GNU Texinfo!")))
