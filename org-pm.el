
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
and copies the found components to the project.")

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

(defvar org-pm-auto-parse nil
  "If not nil, automatically parse a org-mode buffer
 for org-pm data before saving it.")

(defvar org-pm-auto-copy nil ;; 'on-save
"If not nil, automatically copy file components to a project to the
project's source folder before publishing.")

(defvar org-pm-project-template-file-name
  (concat (file-name-directory (or load-file-name (buffer-file-name)))
          "org-pm-project-template.org")
"Full path of file containing template of project definition for
projects generated automatically with org-pm-make-project-template.
The path is initialized at code loading time by function org-pm-init-project-template-name.
org-pm-make-project-template uses it to make project templates.")

(defun org-get-header-property (property &optional all)
  "Get property from buffer variable.  Returns only fist match except if ALL is defined.
NOTE: Also works if editing subtree narrowed or in separate narrowed buffer. "
  (with-current-buffer
      (current-buffer)
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (let (values)
            (while (re-search-forward (format "^#\\+%s:?[ \t]*\\(.*\\)" property) nil t)
              (add-to-list 'values (substring-no-properties (match-string 1))))
            (if all
                values
              (car values))))))))

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
    (unless project-name (setq project-name "org_pm_default"))
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
    (let ((base-dir (plist-get info :base-directory))
          (input-file (plist-get info :input-file)))
      (when (and base-dir input-file)
        (replace-regexp-in-string
         "{{.}}"
         (org-make-relpath-string
          (plist-get info :base-directory)
          ;; distance of input file from base-directory = relative path!
          (plist-get info ':input-file))
         string)))))

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

(defun org-html-toc (depth info)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is a
plist used as a communication channel.  Return the table of
contents as a string, or nil if it is empty."
  (let ((toc-heading (plist-get info :toc-heading))
        (toc-entries
         (mapcar (lambda (headline)
                   (cons (org-html--format-toc-headline headline info)
                         (org-export-get-relative-level headline info)))
                 (org-export-collect-headlines info depth)))
        (outer-tag (if (and (org-html-html5-p info)
                            (plist-get info :html-html5-fancy))
                       "nav"
                     "div")))
    (when toc-entries
      (unless toc-heading (setq toc-heading "Table of Contents"))
      (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
              (format "<h%d>%s</h%d>\n"
                      org-html-toplevel-hlevel
                      (org-html--translate toc-heading info)
                      org-html-toplevel-hlevel)
              "<div id=\"text-table-of-contents\">"
              (org-html--toc-text toc-entries)
              "</div>\n"
              (format "</%s>\n" outer-tag)))))

(add-hook 'after-save-hook 'org-pm-maybe-parse-and-copy)

(defun org-pm-maybe-parse-and-copy ()
  "This function is run whenever a file is saved.
If org-pm-auto-parse is true, make projects whose definitions are in this buffer.
If org-pm-auto-copy is set to 'on-save, then copy the file and sections
specified to their project base directory folders."
  (when (equal major-mode 'org-mode)
    (if org-pm-auto-parse
        ;; if org-pm-auto-copy is not nil, then don't save here:
        (org-pm-make-projects org-pm-auto-copy))
    (if (equal org-pm-auto-copy 'on-save)
        ;; Always save if running this.
        (org-pm-copy-components-to-projects))))

(defun org-pm-toggle-auto ()
  (interactive)
  (setq org-pm-auto-parse (not org-pm-auto-parse))
  (if org-pm-auto-parse ;; stay in sync with auto parse!
      (setq org-pm-auto-copy 'on-save)
    (setq org-pm-auto-copy nil))
  (if org-pm-auto-parse
      (message "Org-pm auto-save and copy activated.")
    (message "Org-pm auto-save and copy deactivated.")))

(defun org-pm-save-and-update ()
  (interactive)
  (org-edit-src-save)
  (org-pm-make-projects)
  (org-pm-copy-components-to-projects))

(defun org-pm-toggle-verbose ()
  (interactive)
  (setq org-pm-report-after-copying-p (not org-pm-report-after-copying-p))
  (if org-pm-report-after-copying-p
      (message "Reporting after copying activated")
    (message "Reporting after copying deactivated")))

(defun org-pm-do-auto ()
  (interactive)
  (setq org-pm-auto-parse t)
  (setq org-pm-auto-copy 'on-save))

(defun org-pm-dont-auto ()
  (interactive)
  (setq org-pm-auto-parse nil)
  (setq org-pm-auto-copy nil))

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

(defun org-pm-make-projects (&optional do-not-save-now)
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
  (let (
        ;; abandoning template IZ Jan 5, 2014 (6:28 PM)
        ;; (template (org-pm-make-default-project-plist))
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
          ;; (setq projects (cons (org-pm-parse-project-def entry template) projects))
          (setq projects (cons (org-pm-parse-project-def entry) projects))
          ))
     "PROJECT_DEFS")
    (mapcar 'org-pm-check-add-project projects)
    (unless do-not-save-now (org-pm-save-all-project-data))
    (message "Org-pm defined %d projects" (length projects))))

(defun org-pm-parse-project-def (proj-node &optional template)
  "TEmp note: template is no longer used IZ Jan 5, 2014 (6:27 PM)
Create a project definition list based on the contents of the
section described in proj-node plist. Convert headings
to property names and contents to their values.
Add useful identification data.
Argument template is a plist with additional properties,
but may be left out if the section contains all the properties needed
to define the project."
  (unless org-publish-project-alist (org-pm-load-all-project-data))
  (let (
        ;; (pdef (copy-sequence template))
        pdef
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
              (list (intern (replace-regexp-in-string "^:static_" ":"
                                                      (symbol-name (car pair))))
                    (cadr pair)))
                     (-filter
                      (lambda (pair) (string-match "^:static_"
                                                   (symbol-name (car pair))))
                      (-partition 2 p-def)))))
    (setq static-project-name (concat "static_" p-name))
    (setq org-publish-project-alist
          (assoc-replace org-publish-project-alist
                         static-project-name static-project))
    (setq org-publish-project-alist
          (assoc-replace org-publish-project-alist
                         (concat "combined_" p-name)
                         (list :components
                               p-name static-project-name))))
  project)

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

;; Will replace org-pm-register-project-components.
(defvar *org-pm-missing-projects* nil
"Names of projects referenced in a file, whose definition is not found.
For reporting.")

(defvar *org-pm-updated-projects* nil
  "Names of projects to which components in a file were copied.
For reporting")

(defvar org-pm-report-after-copying-p t
"If not-nil, org-pm-copy-components-to-projects will post a report
of projects not found or of projects targeted when finishing.")

(defun org-pm-copy-components-to-projects (&optional do-not-save-now)
  "Find which parts of the file go to which to projects, and copy them
to the base-directories of these projects.  Also save the projects found in
list org-pm-files for this project, using the full path of this file as key.
This list is saved and can be used later to update the contents of any project
by finding all the files that contribute to this project.

Parse current buffer, looking for projects added for the whole file (with property
=#+PROJECT:= or for sections (with tags enclosed in =_=). Collect names of all projects
found in a list. Put the list in the assoc list stored in =org-pm-files=, using the full
path of the file as key. Function =org-pm-copy-to-project= scans this list to find if
the file contains any components that should be copied to the project, and copies them.

components: List of file and/or ids of any sections that are copied to projects.
              Each element is of the form:
              (component (project folder file) (project folder file)...)
Components is added to org-pm-files and auto-saved."

  (interactive)
  (let* ((fullpath (buffer-file-name (current-buffer)))
        (filename (file-name-nondirectory fullpath))
        components file-components (origin-buffer (current-buffer)))
    (setq file-components
          (-map (lambda (component) (org-pm-parse-component component filename))
                (org-get-header-property "PROJECT" t)))
    (org-map-entries
     '(let* (name
             (node (cadr (org-element-at-point)))
             (pspecs (-filter (lambda (tag) (string-match "^_.*_$" tag))
                              (plist-get node :tags))))
        (when pspecs
          (setq name (plist-get node :raw-value))
          (setq date (plist-get node :START_TIME))
          (setq components
                (cons
                 (cons
                  (plist-get node :begin)
                  (-map
                   (lambda (component) (org-pm-parse-component component name date))
                   pspecs))
                 components)))))
    (if file-components
        (setq components (cons (cons "FILE" file-components) components)))
    (setq org-pm-files (assoc-replace org-pm-files fullpath components))
    ;; first save, then do the copying:
    (unless do-not-save-now (org-pm-save-all-project-data))
    (setq *org-pm-missing-projects* nil)
    (setq *org-pm-updated-projects* nil)
    (dolist (comp components)
      (let ((pos (car comp))
            (target-buffer (get-buffer-create "*org-pm-copy-buf*")))

        (cond ((equal "FILE" pos)
               (set-buffer target-buffer)
               (insert-buffer origin-buffer)
               (dolist (proj (cdr comp)) (org-pm-save-buffer proj target-buffer)))
             (t
              (set-buffer origin-buffer)
              (goto-char pos)
              (org-copy-subtree)
              (set-buffer target-buffer)
              (org-paste-subtree 1)
              (dolist (proj (cdr comp)) (org-pm-save-buffer proj target-buffer))))
       (kill-buffer target-buffer)))
    (if org-pm-report-after-copying-p
        (if *org-pm-missing-projects*
         (grizzl-completing-read "These projects could not be found"
                                 (grizzl-make-index *org-pm-missing-projects*))
       (grizzl-completing-read "Copied components to these projects"
                               (grizzl-make-index *org-pm-updated-projects*))))
    (message "Result: %s" components)))

(defun org-pm-parse-component (component filename &optional date)
  "Parse strings component and filename, and provide project, folder, filename strings."
  (let ((parts
         (-take 3 (split-string
                   (concat (replace-regexp-in-string "#" "." component) "@@") "@"))))
    (setq parts
          (cons (replace-regexp-in-string
                 "^_" "" (replace-regexp-in-string "_$" "" (car parts)))
                (cdr parts)))
    (unless (equal 0 (length (caddr parts)))
        (setq filename (caddr parts)))
    (setq filename (org-pm-make-filename filename date))
    (unless (string-match ".org$" filename)
      (setq filename (concat filename ".org")))
    (setq parts (-replace-at 2 filename parts))
    parts))

(defun org-pm-make-filename (title &optional date)
  "Convert title of entry into filename.
Remove non alphanumeric characters.
Replace spaces by dashes (-).
Lowercase everything.
If date is provided, convert date into jekyll- (hexo-, etc.) compatible
blog entry format, and prepend it.
Entry title 'Thoughts on [pre-]processing',
with date <2014-01-05 Sun 10:56>
becomes: '2014-01-05-thoughts-on-pre-processing' "
  (let ((filename
         (downcase
          (replace-regexp-in-string
           "-+" "-"
           (replace-regexp-in-string "[^[:alnum:]]" "-" title)))))
    (when (and date (string-match
                     "^<\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\)"
                     date))
      (setq filename (concat (substring date 1 11) "-" filename)))))

(defun org-pm-save-buffer (specs buffer)
  (let ((target-path (org-pm-make-target specs)))
    (make-directory (file-name-directory target-path) t)
    (write-region nil nil target-path)
    ))

(defun org-pm-make-target (specs)
  (let* ((project-name (car specs))
         (folder (cadr specs))
         (slash (if (string-match "/$" folder) "" "/"))
         (project (assoc project-name org-publish-project-alist)))
    (cond (project
           (add-to-list '*org-pm-updated-projects* project-name)
           (concat (plist-get (cdr project) :base-directory)
                   folder slash (caddr specs)))
          (t
           (add-to-list '*org-pm-missing-projects* project-name)
           nil))))

;; Fix grizzl-completing-read to display custom prompt
(require 'grizzl)
(defun grizzl-completing-read (prompt index)
  "Performs a completing-read in the minibuffer using INDEX to fuzzy search.
Each key pressed in the minibuffer filters down the list of matches."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq *grizzl-current-result* nil)
        (setq *grizzl-current-selection* 0)
        (grizzl-mode 1)
        (lexical-let*
            ((hookfun (lambda ()
                        (setq *grizzl-current-result*
                              (grizzl-search (minibuffer-contents)
                                             index
                                             *grizzl-current-result*))
                        (grizzl-display-result index prompt)))
             (exitfun (lambda ()
                        (grizzl-mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer (if prompt prompt ">>> "))
    (grizzl-selected-result index)))

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
                                    (string-match "^static_" (car proj))))
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

(add-hook 'org-mode-hook
  (lambda ()
    (define-key org-mode-map (kbd "H-m a") 'org-pm-toggle-auto)
    (define-key org-mode-map (kbd "H-m s") 'org-pm-save-and-update)
    (define-key org-mode-map (kbd "H-m v") 'org-pm-toggle-verbose)
    (define-key org-mode-map (kbd "H-m l") 'org-pm-list-project-defs)
    (define-key org-mode-map (kbd "H-m m") 'org-pm-make-projects)
    (define-key org-mode-map (kbd "H-m c") 'org-pm-copy-components-to-projects)
    (define-key org-mode-map (kbd "H-m t") 'org-pm-make-project-template)))
