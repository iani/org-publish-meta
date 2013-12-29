
(defvar org-pm-project-data-file-path
  (let ((home (file-truename "~/.emacs.d")))
      (cond
       ((file-exists-p home)
        (setq home (concat home "/savefile"))
        (unless (file-exists-p home) (make-directory home))
        (concat home "/org-pm-project.data.el"))
       (t (concat home "/.org-pm-project.data.el"))))
  "Path of file for storing org-publish-project-alist and
org-pm-files.  If nil, the path is deduced from the existence
of .emacs.d folder in user's home directory.
If .emacs.d exists, use ~/.emacs.d/savefile/org-publish-project-alist
else use ~/.org-publish-project-alist.
Create savefile folder if it does not exist.")

(defvar org-pm-files nil
"List of files to be copied to projects.
For each file, store a list starting with the full path of the file, and
followed by the list of projects specified in the file
or any of its sections:
  (full-path-of-file project1 project2 ...)
Function org-pm-register-project-components scans a file, creates its list
and puts it in org-pm-files.
Function org-pm-copy-to-project searches through all lists in org-pm-files,
collects the list of files that belong to a project,
parses each file to find which components should be copied,
and copies the found components to the project.
")

(defvar org-pm-project-def-duplicates nil
  "List of links to files/sections which contains project definitions
that were overwritten because another definition with the same name was found.
Auto-saved together with org-publish-project-alist and org-pm-files.
Used to create org-mode buffer with links to these locations.
See functions:
- org-pm-check-add-project
- org-pm-list-dupicate-project-defs
- org-pm-list-project-defs
- pm/edit-duplicate-project-def"
)

(defvar org-pm-auto-parse t
  "If not nil, automatically parse a org-mode buffer
 for org-pm data before saving it.")

(defvar org-pm-auto-copy 'on-save
"If not nil, automatically copy file components to a project to the
project's source folder before publishing.")

(defvar org-pm-project-template-file-name
  (concat (file-name-directory (or load-file-name (buffer-file-name)))
          "org-pm-project-template.org")
"Full path of file containing template of project definition for
projects generated automatically with org-pm-make-project-template.
The path is initialized at code loading time by function org-pm-init-project-template-name.
org-pm-make-project-template uses it to make project templates.")

(defvar org-pm-default-project-name "org-pm-default"
"Name of default, auto-generated project.")

(defvar org-pm-default-project-org-folder "~/pm-org"
"Path of folder for source files of default project.")

(defvar org-pm-default-project-html-folder "~/pm-html"
  "Path of folder for html (published website) files of default project.")

(defvar org-pm-default-project-plist
  '(
    :base-extension "org"
    :recursive t
    :publishing-function org-publish-org-to-html
    :headline-levels 5
    :auto-preamble t
  )
"The defalt properties for publishing a project with html.
Used to provide initial contents when creating a project plist in
org-pm-make-default-project-plist. "
)

(defun org-get-drawer (drawer-name)
  "Get the contents of the drawer named 'drawer-name', at current section."
  (save-excursion
    (org-back-to-heading)
    (let* ((plist (cadr (org-element-at-point)))
           (node-end (plist-get plist :end))
           drawer-begin)
      (re-search-forward (format "^:%s:" drawer-name) node-end)
      (forward-char)
      (setq drawer-begin (point))
      (re-search-forward "^:END:" node-end)
      (beginning-of-line)
      (backward-char)
      (buffer-substring drawer-begin (point)))))

(defun org-set-comment ()
  "Change the COMMENT state of an entry to COMMENT.
Do *not* remove COMMENT state if already present.
This function is derived from org-toggle-coment."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let (case-fold-search)
      (cond
       ((looking-at (format org-heading-keyword-regexp-format
                            org-comment-string))
        ;; if comment was found, then do nothing:
        )
       ((looking-at org-outline-regexp)
        (goto-char (match-end 0))
        (insert org-comment-string " "))))))

(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c C-;") 'org-set-comment))

(defun org-pm-create-excerpt ()
    "If an org-mode-style comment with contents: READMORE is found,
then insert a heading after the comment, and add COMMENT status to that heading.
This excludes the rest of the section from being exported.
If no READMORE is found, then COMMENT the entire section.
This is an easy way of creating excerpts when copying a flle to a project,
and parts of this file are also copied to the project, so we don't want to
export them with the main file."
    (interactive)
    (save-excursion
      (org-back-to-heading)
      (let* ((plist (cadr (org-element-at-point)))
             (node-begin (plist-get plist :begin))
             (node-end (plist-get plist :end)))
        (re-search-forward "^ # READMORE" node-end t)
        (unless (equal (point) node-begin)
          (org-insert-heading)
          (org-set-comment))
        (message "%d %d %d" (point) node-begin node-end)
        )))

(defun assoc-add (alist key element)
  "Add element to the sublist of alist which starts with key."
  (let ((sublist (assoc key alist)))
    (if sublist
        (setcdr sublist (cons element (cdr sublist)))
      (if alist
          (setcdr alist (cons (list key element) (cdr alist)))
        (setq alist (list (list key element))))))
  alist)

(defun assoc-remove (alist key element)
  "Remove element from the sublist of alist whose car is equal to key."
  (when alist
    (let ((sublist (assoc key alist)))
      (when sublist
        (setcdr sublist(remove element (cdr sublist)))
        (if (equal 1 (length sublist)) (setq alist (remove sublist alist))))
      alist)))

(defun assoc-remove-key (alist key)
  "Remove all sublists of alist whose car is equal to key."
  (setq alist (remove* key alist :test 'equal :key 'car)))

  ;;; older version
(defun assoc-remove-key-simple-style (alist key)
  "Remove all sublists of alist whose car is equal to key."
  (let (found)
    (while (setq found (assoc key alist))
      (setq alist (delq found alist)))
    alist))

(defun assoc-replace (alist key newlist)
  "Remove all sublists of alist whose car is equal to key, and then
     add (cons key newlist) to alist."
  (setq alist (assoc-remove-key alist key))
  (setq alist (cons (cons key newlist) alist)))

(defun org-pm-make-default-project-plist ()
  "Construct default plist for publishing a project in html."
  (let ((plist (copy-sequence org-pm-default-project-plist)))
    (setq plist (plist-put plist :base-directory
                           (file-truename org-pm-default-project-org-folder)))
    (setq plist (plist-put plist :publishing-directory
                           (file-truename org-pm-default-project-html-folder)))))

(defun org-pm-add-project-file (project-name file)
  "In list org-pm-files, add the project-name to the list
of projects that file bel ongs. "
  (setq org-pm-files
        (assoc-add org-pm-files file project-name)))

(defun org-pm-remove-project-file (project-name file)
  "In list org-pm-files, add the project-name to the list
of projects that file belongs. "
  (setq org-pm-files
        (assoc-add org-pm-files file project-name)))

(defun org-pm-add-project-to-file-header (project-name)
  "Add property PROJECT with value project-name at beginning of file."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-buffer)
      (insert (format "#+PROJECT: %s\n" project-name)))))

(defun org-pm-get-section-projects ()

)

(defun org-pm-edit-project-template ()
  "Edit the file containing the global project template.
Note that edits may cause conflicts when updating org-pm from git."
  (interactive)
  (find-file org-pm-project-template-file-name))

(defun org-pm-edit-saved-project-data ()
  "Edit the file containing the global project data."
  (interactive)
  (find-file org-pm-project-data-file-path))

(defun org-pm-show-project-definition-section ()
  "Mark all sections tagged PROJECT_DEFS.
  Additionally go to the first section tagged PROJECT_DEFS, if it exists."
  (interactive)
  (let ((defs (org-map-entries '(cadr (org-element-at-point)) "PROJECT_DEFS")))
    (cond
     (defs
       (org-match-sparse-tree nil "PROJECT_DEFS")
       (goto-char (plist-get (car defs) :begin))
       (recenter-top-bottom '(4))
       (message "Showing location of first project definition section found."))
     (t (message "No project definitions were found in this file.")))))

(defun org-pm-make-project-template (&optional project-name no-name-query no-query)
  "Create a project definition template and insert it into current file.
Input project name, base directory and publishing directory from user.
Skip input step if called with prefix argument.
Read file containing template of project definition
from org-pm-project-template-file-name
If arguments present, replace relevant parts of the template with
custom name, base-directory, publishing-directory
Insert the resulting template in the current file.
Create the project as well as its static project and component project.
Store all 3 in org-publish-project-alists.
Save updated project, file and duplicate lists to disk."
  (interactive "P")
  (let* ((base-directory (file-truename "~/org-pm/"))
         (publishing-directory
          (file-truename "~/Sites/org-pm/"))
         (def-node
           (car (org-map-entries '(cadr (org-element-at-point)) "PROJECT_DEFS")))
         (buffer (get-buffer-create "*def*"))
         plist template-string)
    (unless project-name (setq project-name "org-pm-default"))
    (unless no-name-query
      (setq project-name (read-string "Enter project name: " project-name)))
    (unless no-query
      (setq base-directory (query-make-folder base-directory))
      (setq publishing-directory (query-make-folder publishing-directory)))
    (save-excursion
      (set-buffer buffer)
      (insert-file-contents org-pm-project-template-file-name)
      (beginning-of-buffer)
      (replace-string "PROJECTNAME" project-name)
      (beginning-of-buffer)
      (replace-string "BASEDIRECTORY" base-directory)
      (beginning-of-buffer)
      (replace-string "PUBLISHINGDIRECTORY" publishing-directory)
      (setq template-string (buffer-string))
      (kill-buffer buffer))
    (cond (def-node
           (goto-char (plist-get def-node :begin))
           (end-of-line)
           (insert "\n")
           (org-paste-subtree (+ 1 (plist-get def-node :level)) template-string))
          (t
           (end-of-buffer)
           (insert "\n* COMMENT Project Definitions              :PROJECT_DEFS:\n")
           (org-paste-subtree 2 template-string)))
    (org-id-get-create)
    (org-pm-check-add-project (org-pm-parse-project-def (cadr (org-element-at-point))))
    (org-pm-save-all-project-data)))

(defun org-html-provide-relative-path (string backend info)
  "Provide relative path for link."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "{{.}}"
     (org-make-relpath-string
      (plist-get info :publishing-directory)
      (plist-get info ':input-file))
     string)))

;;; Add relative path filter to export final output functions
(add-to-list 'org-export-filter-final-output-functions
             'org-html-provide-relative-path)

(defun org-make-relpath-string (base-path file-path)
  "create a relative path for reaching base-path from file-path ('./../..' etc)"
  (let (
        (path ".")
        (depth (-
                (length (split-string (file-name-directory file-path) "/"))
                (length (split-string base-path "/")))))
    (dotimes (number
              (- depth 1)
              path)
      (setq path (concat path "/..")))))

(defun org-pm-load-all-project-data ()
  "Load project alist, project file lists, duplicate project def lists
from previously saved date on disk."
  (interactive)
  (if (file-exists-p org-pm-project-data-file-path)
      (load-file org-pm-project-data-file-path)))

(defun org-pm-save-all-project-data ()
  "Load project alist, project file lists, duplicate project def lists
from previously saved date on disk."
  (interactive)
  (dump-vars-to-file
   '(org-publish-project-alist org-pm-files org-pm-project-def-duplicates)
   org-pm-project-data-file-path))

(defun dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dump varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(defun org-pm-reset-project-list ()
  "Set org-publish-project-alist to nil.  Save"
  (interactive)
  (cond ((y-or-n-p "Really erase all projects and save?")
         (setq org-publish-project-alist)
         (org-pm-save-all-project-data))))

(defun org-pm-make-projects ()
  "Construct the projects for all project definitions found in current file.
Project definitions are those nodes which are contained in nodes tagged as
PROJECT_DEFS.
Note about project definition node-IDs:
Section IDs of project definitions are used only as links
to point to the position in the file where a project definition is, located.
They do nod identify a project.  A project is identified by its name.
Therefore:
The node-id of a project is set to <full-file-path>::#<section id>.
When a duplicate section id is found in a definition, it is replaced by a new one,
and the new id is stored in the project."
  (interactive)
  (unless org-publish-project-alist (org-pm-load-all-project-data))
  (let ((template (org-pm-make-default-project-plist))
        levels id ids projects)
    (org-map-entries
     '(let
          ((entry (cadr (org-element-at-point))))
        (if (member "PROJECT_DEFS" (plist-get entry :tags))
            (setq levels (cons (+ 1 (plist-get entry :level)) levels)))
        (when (equal (car levels) (plist-get entry :level))
          (setq id (org-id-get-create))
          (when (member id ids)
            (org-delete-property "ID")
            (setq id (org-id-get-create))
            (setq entry (plist-put entry :ID id)))
          (setq ids (cons id ids))
          (setq projects (cons (org-pm-parse-project-def entry template) projects))))
     "PROJECT_DEFS")
    (mapcar 'org-pm-check-add-project projects)
    (org-pm-save-all-project-data)))

(defun org-pm-parse-project-def (proj-node &optional template)
  "Create a project definition list based on the contents of the
section described in proj-node plist. Convert headings
to property names and contents to their values.
Add useful identification data.
Argument template is a plist with additional properties,
but may be left out if the section contains all the properties needed
to define the project."
  (unless org-publish-project-alist (org-pm-load-all-project-data))
  (let ((pdef (copy-sequence template))
        (pname (plist-get proj-node :raw-value))
        (begin (plist-get proj-node :contents-begin))
        (node-id (plist-get proj-node :ID))
        (file-name (buffer-file-name (current-buffer))))
    (setq pdef (plist-put pdef :project-name pname))
    (setq pdef (plist-put pdef :node-id node-id))
    (setq pdef (plist-put pdef :node-filename file-name))
    (setq pdef (plist-put pdef :project-id (concat file-name "::#" node-id)))
    (setq pdef (plist-put pdef :last-updated (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (cond
     (begin
      (save-excursion
        (save-restriction
          (narrow-to-region begin (plist-get proj-node :contents-end))
          (org-map-entries
           '(let* (
                   (element (cadr (org-element-at-point)))
                   (heading (plist-get element :raw-value))
                   (space (string-match " .*" heading))
                   prop-name prop-value contents-begin)
              (cond
               (space
                (setq prop-name (substring heading 0 space))
                (setq prop-value (eval (read (substring heading space))))
                (if (and
                     (equal prop-name "include-containing-file")
                     prop-value)
                    (org-pm-add-component
                     pname (buffer-file-name (current-buffer)) prop-value)))
               (t (setq prop-name heading)
                  (setq contents-begin (plist-get element :contents-begin))
                  (if contents-begin
                      (setq
                       prop-value
                       (buffer-substring-no-properties
                        contents-begin
                        (plist-get element :contents-end))))))
              (setq pdef
                    (plist-put pdef (intern (concat ":" prop-name)) prop-value))))))))
    (cons pname pdef)))

(require 'dash)
(defun org-pm-check-add-project (project)
  "Add the project definition contained in plist 'project' to org-publish-project-alist,
replacing any previously existing definition there.  Before replacing, save any
previously existing project whose definition is in a different file component in
the variable org-pm-project-def-duplicates:
If a project with the same name already exists in org-publish-project-alist,
and that project has a different ID (file path + section ID), then the previously
existing project definition is added to the list in org-pm-project-def-duplicates.
Also create static and combined project components.
Create alternate ids for the latter, by appending -static and -combined
to the id of the main project."
  (unless org-publish-project-alist (org-pm-load-all-project-data))
  (let* ((p-name (car project))
         (p-def (cdr project))
         (prev-proj (assoc p-name org-publish-project-alist))
         (prev-proj-id (plist-get (cdr prev-proj) :project-id))
         (duplicates (assoc p-name org-pm-project-def-duplicates))
         static-project static-project-name combined-project)
    (cond
     ((not prev-proj))
     ((equal prev-proj-id (plist-get p-def :project-id)))
     (t (setq
         org-pm-project-def-duplicates
         (assoc-replace org-pm-project-def-duplicates p-name
                        (add-to-list 'duplicates prev-proj-id)))))
    (setq org-publish-project-alist
          (assoc-replace org-publish-project-alist p-name p-def))
    (setq static-project
          (-flatten
           (-map
            (lambda (pair)
              (list (intern (replace-regexp-in-string "^:static-" ":"
                                                      (symbol-name (car pair))))
                    (cadr pair)))
                     (-filter
                      (lambda (pair) (string-match "^:static-"
                                                   (symbol-name (car pair))))
                      (-partition 2 p-def)))))
    (setq static-project-name (concat "static-" p-name))
    (setq org-publish-project-alist
          (assoc-replace org-publish-project-alist
                         static-project-name static-project))
    (setq org-publish-project-alist
          (assoc-replace org-publish-project-alist
                         (concat "combined-" p-name)
                         (list :components
                               p-name static-project-name))))

  project)

(defun org-pm-query-select-project (new-project old-project)
  "Check if new project definition is from a different source than old-project.
  If yes, then ask the user which of the project definitions to keep.
  Post info about the rejected definition so that user can remove or edit it.
  Return the selected project so that it is added by org-pm-add-project,
  replacing the previous entry for this project."
  (let ((selection new-project))
    (unless (equal (plist-get (cdr new-project) :source-id)
                   (plist-get (cdr old-project) :source-id))
      (setq selection (must-write-the-code-for-query-selection new-project old-project))
      (must-write-the-code-for-message-about-rejected
       (if (eq selection new-project) old-project new-project)))
    selection))

(defun org-pm-add-project (project)
  "Add project to org-pm-project-alist.
  If previous project with same name exist, replace it."
  (setq org-publish-project-alist
        (assoc-replace org-publish-project-alist (car project) (cdr project))))

(defun org-pm-add-file-to-project ()
  "Add the file of the current buffer to a project selected or input by the user.
    If the project selected/input by the user is not already in the file's project list:
    - If no project of that name exists, request that the project be defined using
    org-pm or other methods.
    - If no project at all exists, then offer to create default project.
    - Add the selected project to the file's list in org-pm-files.
    - Save org-pm-files.
    - Add the project name to property PROJECT in file's header."
  (interactive)
  (unless (buffer-file-name (current-buffer))
    (error "This buffer is not associated with a file.  Please save first."))
  (let* ((org-completion-use-ido t)
         (projects
          (if org-publish-project-alist
              (mapcar org-publish-project-alist 'car)
            (list org-pm-default-project-name)))
         (project-name
          (org-icompleting-read "Choose or input a project name: " projects)))
    (if (member project-name (org-pm-get-file-projects))
        (error "This file is already part of project '%s'" project-name))
    (setq project (org-pm-query-make-default-project project-name))
    (org-pm-add-project-to-file-header project-name)
    (org-pm-add-project-file project-name (buffer-file-name (current-buffer)))
    (org-pm-save-all-project-data)
    (org-pm-make-project-template project)
    (message
     "Added project named: %s to file: %s\nBase directory is: %s\nPublishing directory is: %s"
     project-name
     (file-name-nondirectory (buffer-file-name (current-buffer)))
     (plist-get (cdr project) :base-directory)
     (plist-get (cdr project) :publishing-directory))))

(defun query-make-folder (path &optional prompt-string)
  "If folder at path does not exist, then show dialog offering to user
    the option to create the indicated folder or to choose another path.
    If the path selected does not exist, create folder."
  (setq path (file-truename path))
  (unless prompt-string (setq prompt-string "Folder select or create:"))
  (let ((answer
         (read-file-name
          (format
           "%s\nSelect or input folder (folder will be created if needed):\n"
           prompt-string)
          path)))
    (unless (equal (file-truename answer) (buffer-file-name (current-buffer)))
      (setq path answer))
    (unless (file-exists-p path) (make-directory path))
    path))

(defun org-pm-register-project-components ()
  "Parse current buffer, looking for projects added for the whole file (with property
=#+PROJECT:= or for sections (with tags enclosed in =_=). Collect names of all projects
found in a list. Put the list in the assoc list stored in =org-pm-files=, using the
full path of the file as key. Function =org-pm-copy-to-project= scans this list to find
if the file contains any components that should be copied to the project, and copies
 them."
  (interactive)

  (let (projects (filename (buffer-file-name (current-buffer))))
    (mapcar (lambda (project)
              (add-to-list 'projects (org-pm-get-project-name project)))
            (org-get-header-property "PROJECT" t))
    (org-map-entries
     '(let ((tags (plist-get (cadr (org-element-at-point)) :tags)))
        (dolist (tag tags)
          (if (string-match  "^_.*_$" tag)
              (add-to-list 'projects (org-pm-get-project-name tag))))
        ))
    (setq org-pm-files (assoc-replace org-pm-files filename projects))
    (message "Result: %s" projects)))

(defun org-pm-get-project-name (name-and-folder)
  (car (split-string name-and-folder "@")))

;; Note: for setting the project name to the car of the split, and the
;; folder to the cdr of the split, see
;; http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node94.html
;; and http://clhs.lisp.se/Body/m_mult_2.htm
;; on how to do multiple-value-setq
;; Here example from site 2 above:
;; (multiple-value-setq (a b c) (values 1 2))

(defun org-pm-copy-components ()
  "Identify and copy the components that are marked to belong to projects."

)

;; This version does both the registering and the copying.
;; Will replace org-pm-register-project-components.
(defun org-pm-process-file ()
  "Identify and copy the components that are marked to belong to projects.
Parse current buffer, looking for projects added for the whole file (with property
=#+PROJECT:= or for sections (with tags enclosed in =_=). Collect names of all projects
found in a list. Put the list in the assoc list stored in =org-pm-files=, using the full
path of the file as key. Function =org-pm-copy-to-project= scans this list to find if
the file contains any components that should be copied to the project, and copies them."
    (interactive)

    (let ((filename (buffer-file-name (current-buffer)))
          projects components project file folder)
      (mapcar (lambda (project)
                (add-to-list 'projects (org-pm-get-project-name project)))
              (org-get-header-property "PROJECT" t))
      (org-map-entries
       '(let ((tags (plist-get (cadr (org-element-at-point)) :tags)))
          (dolist (tag tags)
            (if (string-match  "^_.*_$" tag)
                (add-to-list 'projects (org-pm-get-project-name tag))))
          ))
      (setq org-pm-files (assoc-replace org-pm-files filename projects))
      (message "Result: %s" projects)))

(defun org-pm-list-duplicate-project-defs ()
  "List project definitions of same name that are found in more than one file or section.
Do this in a separate org-mode buffer, and provide links to both file and section."

  (interactive)

  (if (equal 0 (length org-pm-project-def-duplicates))
      (error "There are no duplicate project definitions at all.\n!!! ... YAyyy ... !!!"))

  (let ((buffer (get-buffer-create "*org-pm-project-def-duplicates*")))
    (switch-to-buffer buffer)
    (org-mode)
    (delete-region (point-min) (point-max))
    (org-insert-heading)
    (insert "DUPLICATE PROJECT DEFINITIONS")
    (dolist (project org-pm-project-def-duplicates)
      (let ((project-name (car project)))
        (insert "\n** " project-name "\n")
        (dolist (def (cdr project))
          (let ((path-and-id (split-string def "::#")))
            (insert "file: file:" (car path-and-id) "\n")
            (insert "node: " "id:" (cadr path-and-id) "\n")))))
    ))

(defun org-pm-list-project-defs ()
  "Build list of projects with links to file and node containing the project definition,
in a separate org-mode buffer, and provide links to both file and section.
Also list duplicate project definitions,
i.e. definitions of same name that are found in more than one file or section.
Note: static and combined projects created by the system
are not checked and added as duplicates by org-pm-check-add-project.
But they are in org-publish-project-alist, which we use for this list.
So we filter them out."

  (interactive)

  (if (equal 0 (length org-publish-project-alist))
      (error "There are no project definitions at all."))

  (let ((buffer (get-buffer-create "*org-pm-project-definitions*"))
        node-id dir)
    (switch-to-buffer buffer)
    (org-mode)
    (delete-region (point-min) (point-max))
    (org-insert-heading)
    (insert "PROJECT DEFINITIONS")
    (dolist (project (-remove (lambda (proj)
                                (or (string-match "^combined-" (car proj))
                                    (string-match "^static-" (car proj))))
                              org-publish-project-alist))
      (setq node-id (plist-get (cdr project) :node-id))
      (insert "\n** "
              (car project)
              " (click [[elisp:(org-pm-search-link \""
              (plist-get (cdr project) :project-id)
              "\")][*HERE*]] to edit definition)\n")
      (setq dir (plist-get (cdr project) :base-directory))
      (insert "base dir: [[elisp:(dired\"" dir "\")][" dir "]]\n" )
      (setq dir (plist-get (cdr project) :publishing-directory))
      (insert "publishing dir: [[elisp:(dired\"" dir "\")][" dir "]]\n" )
      (insert "file: file:" (plist-get (cdr project) :node-filename) "\n")
      (insert "node: id:" node-id "\n")
      (let ((duplicates (cdr (assoc (car project) org-pm-project-def-duplicates))))
        (if duplicates
            (dolist (def duplicates)
              (let ((path-and-id (split-string def "::#")))
                (insert "\n*** duplicate: ")
                (insert
                 " (click [[elisp:(org-pm-search-link \""
                 def
                 "\")][*HERE*]] to edit)"
                 )
                (insert "\nfile: file:" (car path-and-id) "\n")
                (insert "node: " "id:" (cadr path-and-id) "\n")))
          (insert "\nThere no duplicate definitions for this project!\n"))))))

(defun org-pm-search-link (link)
  (let ((file-and-id (split-string link "::#")))
    (find-file (car file-and-id))
    (beginning-of-buffer)
    (re-search-forward (concat ":ID: +" (cadr file-and-id)))
    (org-back-to-heading)
    (org-show-subtree)
    (org-mark-element)
    (recenter-top-bottom 1)
    (message "
---> Marked the entire section containing project definition.
Type C-space C-space to de-select region and deactivate mark.")))

(defun pm/edit-duplicate-project-def ()
  "Select a project definition from the list of found duplicates, and
go to the containing file at the selected location, so as to edit the
duplicate definition (or to remove it)."

  (interactive)

  (if (equal 0 (length org-pm-project-def-duplicates))
      (error "There are no project definitions to edit."))
  (let ((definitions (mapcar (lambda (p) (car p)) org-pm-project-def-duplicates))
        definition def-address)
    (setq project
          (completing-read "Select project: " definitions nil t (car definitions)))
    (setq definitions (cdr (assoc project org-pm-project-def-duplicates)))
    (setq project
          (completing-read "Select definition: " definitions nil t (car definitions)))
    (setq def-address (split-string project "::#"))
    (find-file (car def-address))
    (beginning-of-buffer)
    (re-search-forward (concat ":ID: +" (cadr def-address)))
    (org-back-to-heading)
    (org-show-subtree)
    (org-mark-element)
    (message "
Marked the entire section containing duplicate project definition.
Type C-space C-space to de-select region and deactivate mark")
    ))
