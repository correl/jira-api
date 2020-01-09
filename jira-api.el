;;; JIRA-API -- JIRA REST API

;; Copyright (c) 2015 Correl Roush

;; Author: Correl Roush <correl@gmail.com>
;; Version: 0.1
;; Created: 2015-06-18

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'url)
(require 'json)

(defcustom jira-api-host "jira.company.com"
  "JIRA Hostname"
  :group 'jira-api
  :type 'string)

(defcustom jira-api-user ""
  "JIRA username"
  :group 'jira-api
  :type 'string)

(defcustom jira-api-use-ssl t
  "Use HTTPS"
  :group 'jira-api
  :type 'boolean)

(defcustom jira-api-org-story-points-field "StoryPoints"
  "Org story points property"
  :group 'jira-api
  :type 'string)

(defcustom jira-api-org-time-estimate-field "Effort"
  "Org time estimate field"
  :group 'jira-api
  :type 'string)

(defvar jira-api-agile-sprint-field 'customfield_10007
  "JIRA Agile sprint field")

(defvar jira-api-agile-story-points-field 'customfield_10004
  "JIRA Agile story points field")

(defvar jira-api-agile-definition-of-done-field 'customfield_11300
  "JIRA Agile Definition of Done field")

(defconst jira-api-timetracking-units
  '(("m" . 60)                          ; 60 minutes in an hour
    ("h" . 8)                           ; 8 hours in a day
    ("d" . 5)                           ; 5 days in a week
    ("w" . nil)))

(defun jira-api-seconds-to-duration (seconds)
  (let ((minutes (ceiling seconds 60)))
    (mapconcat (lambda (pair) (format "%d%s" (car pair) (cdr pair)))
               (--filter (not (zerop (car it)))
                         (-zip
                          (mapcar #'cdr (--reduce-from
                                         (let ((remainder (or (caar acc) minutes)))
                                           (cons (cons (floor remainder it)
                                                       (if it
                                                           (% remainder it)
                                                         remainder))
                                                 acc))
                                         (list)
                                         (mapcar #'cdr jira-api-timetracking-units)))
                          (reverse (mapcar #'car jira-api-timetracking-units))))
               " ")))

(defun jira-api--get-credentials ()
  (let ((info (nth 0 (auth-source-search :host jira-api-host
                                         :port (if jira-api-use-ssl 443 80)
                                         :require '(:user :secret)
                                         :create t))))
    (if info
        (let ((user (plist-get info :user))
              (secret (plist-get info :secret)))
          (cons user
                (if (functionp secret)
                    (funcall secret)
                  secret))))))

(defun jira-api-post (endpoint &optional data)
  (let ((url-request-method "POST"))
    (jira-api-get endpoint data)))

(defun jira-api-get (endpoint &optional data)
  (let* ((url-request-method (or url-request-method "GET"))
         (url-request-data
          (json-encode data))
         (credentials (jira-api--get-credentials))
         (jira-api-user (car credentials))
         (jira-api-password (cdr credentials))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic "
                                        (base64-encode-string (concat jira-api-user ":" jira-api-password))))
            ("Content-Type" . "application/json")))
         (protocol (if jira-api-use-ssl "https" "http"))
         (result-buffer (url-retrieve-synchronously (concat protocol "://" jira-api-host endpoint))))
    (when result-buffer
      (with-current-buffer result-buffer
        (goto-char (point-min))
        (while (not (looking-at "^$"))
          (forward-line))
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read))))))

(defun jira-api-get-issue (issue-id)
  (jira-api-get (concat "/rest/api/latest/issue/"
                        issue-id
                        "?expand=names")))

(defun jira-api-get-attribute (issue &rest names)
  (-reduce-from (lambda (acc value)
                  (cdr (assoc value acc)))
                issue
                names))

(defun jira-api-parse-sprint-attribute (attribute-array)
  (let* ((attribute-string (elt attribute-array 0))
         (sprint-matches (if (s-starts-with? "[" attribute-string)
                             (cdr (s-match-strings-all "\\[\\(.*?\\)\\]" attribute-string))
                           (s-match-strings-all "\\[\\(.*?\\)\\]" attribute-string)))
         (sprint-strings (mapcar #'cadr sprint-matches))
         (sprint-attributes (-tree-map (lambda (s) (let* ((pair (split-string s "=")))
                                                     (cons (intern (car pair)) (cadr pair))))
                                       (-map (lambda (s) (split-string s ",")) sprint-strings))))
    sprint-attributes))

(defun jira-api-get-issue-sprints (issue)
  (jira-api-parse-sprint-attribute
   (jira-api-get-attribute issue
                           'fields jira-api-agile-sprint-field)))


(defun jira-api-agile-get-sprints (board-id)
  (jira-api-get (format "/rest/greenhopper/experimental-api/latest/board/%d/sprint"
                        board-id)))

(defun jira-api-get-issue-story-points (issue)
  (round
   (jira-api-get-attribute issue
                           'fields jira-api-agile-story-points-field)))

(defun jira-api-get-issue-original-estimate (issue)
  (let ((seconds (jira-api-get-attribute issue
                                         'fields 'timetracking 'originalEstimateSeconds)))
    (ceiling (/ seconds 60))))

(defun jira-api-get-worklog (issue-id)
  (jira-api-get (concat "/rest/api/latest/issue/"
                        issue-id
                        "/worklog")))

(defun jira-api-org-update-entry ()
  (let* ((jira-id (org-entry-get (point) "JIRA_ID"))
         (jira-issue (jira-api-get-issue jira-id)))
    (org-set-property
     "StoryPoints"
     (number-to-string (jira-api-get-issue-story-points jira-issue)))
    (org-set-property
     "Effort"
     (let ((jira-estimate
            (jira-api-get-issue-original-estimate jira-issue)))
       (format "%d:%02d"
               (floor (/ jira-estimate 60))
               (% jira-estimate 60))))))

(defun jira-api-log-work (issue-id seconds)
  (jira-api-post
   (concat "/rest/api/latest/issue/"
           issue-id
           "/worklog")
   `((timeSpentSeconds . ,seconds))))

(defun jira-api-get-project-issue-types (project-key)
  (let* ((metadata (jira-api-get
                    (s-concat "/rest/api/latest/issue/createmeta?projectKeys="
                              project-key
                              "&expand=projects.issuetypes.fields")))
       (projects (jira-api-get-attribute metadata 'projects))
       (issuetypes
        (-map (lambda (p)
                (cons (jira-api-get-attribute p 'key)
                      (-map (lambda (issuetype)
                              (let ((fields (jira-api-get-attribute issuetype 'fields)))
                                (cons (jira-api-get-attribute issuetype 'name)
                                      (-map (lambda (field) (cons (jira-api-get-attribute field 'name)
                                                                  (car field))) fields)
                                      )))
                            (jira-api-get-attribute p 'issuetypes))))
              projects)))
    (jira-api-get-attribute issuetypes project-key)))

(defun jira-api-create-issue-from-heading ()
  (interactive)
  (let ((title (org-get-heading t t t t))
        (todo (org-entry-get (point) "TODO"))
        (issue (org-entry-get (point) "JIRA_ID"))
        (epic (org-entry-get (point) "JIRA_EPIC" t))
        (component (org-entry-get (point) "COMPONENT" t))
        (content (let ((org-export-with-drawers nil)
                       (org-export-with-properties nil))
                   (org-export-as 'jira t nil t (list :with-drawers nil)))))
    (when issue
      (error "Jira issue is already set"))
    (unless todo
      (error "Must be a TODO"))
    (let* ((request `((fields . ((project . ((key . "CCPANEL")))
                                 (summary . ,title)
                                 (description . ,content)
                                 (issuetype . ((name . "Task")))
                                 (components . (((name . ,component))))
                                 (customfield_10008 . ,epic)))))
           (response (jira-api-post "/rest/api/latest/issue"
                                   request))
           (issue (jira-api-get-attribute response 'key)))
      (if issue (org-set-property "JIRA_ID" issue)
        (error "Something's gone horribly wrong: %s" response)))))

(provide 'jira-api)
;;; jira-api.el ends here
