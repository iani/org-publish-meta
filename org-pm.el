
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
If .emacs.d exists, use ~/.emacs.d/savefile/org-pm-project-data.el
else use ~/.org-pm-project-data.el.
Create savefile folder if it does not exist.")

(defvar org-pm-section-exports nil
  "List of sections of files copied to projects.
For each file, store a list starting with the full path of the file, and
followed by the list of section position and sublist path-project pairs
specified in the file:

  (full-path-of-file
         (position-section-1 (path . project1) (path . project2) ...)
         (position-section-2 (path . project1) (path . project2) ...)
  )
Function org-pm-get-section-project-paths updates this list
whenever it scans a buffer.
The value is saved on disc in file specified by org-pm-project-data-file-path")

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

(defvar org-pm-project-template-file-name
  (concat (file-name-directory (or load-file-name (buffer-file-name)))
          "org-pm-project-template-jekyll.org")
"Full path of file containing template of project definition for
projects generated automatically with org-pm-insert-new-project.
The path is initialized at code loading time by function
org-pm-init-project-template-name.
org-pm-insert-new-project uses it to make project templates.
The default template is for exporting to jekyll:
org-pm-project-template-jekyll.org
An alternative template for full html export with header is provided:
org-pm-project-template-plain.org" )

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

(defun org-get-section-properties (properties)
  "Return values of each of the properties in list properties,
as separate values.  Can be used with multiple-value-bind to set
each one of several variables to the value of each property in properties list."
  (save-excursion
    (org-back-to-heading)
    (let ((plist (cadr (org-element-at-point))))
      (values-list
       (-map (lambda (p) (plist-get plist (intern (concat ":" p)))) properties)))))

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

(defun org-pm-edit-project-template ()
  "Edit the file containing the global project template.
Note that edits may cause conflicts when updating org-pm from git."
  (interactive)
  (find-file org-pm-project-template-file-name))

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
       (message "Showing location of first project definition section."))
     (t (message "No project definitions were found in this file.")))))

(defun org-pm-insert-new-project (&optional project-name no-name-query no-query)
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

(defun org-pm-reset-project-list ()
  "Set org-publish-project-alist to nil.  Save"
  (interactive)
  (cond ((y-or-n-p "Really erase all projects and save?")
         (setq org-publish-project-alist)
         (org-pm-save-all-project-data))))

(defun org-pm-make-projects (&optional do-not-save-now)
  "Construct the projects for all project definitions found in current file.
Save the upated org-publish-project-alist.

Project definitions are those nodes which are contained in nodes tagged as
PROJECT_DEFS.
Note about project definition NODE-IDs:
Section IDs of project definitions are used only as links
to point to the position in the file where a project definition is, located.
They do not identify a project.  A project is identified by its name.
Therefore:
The node-id of a project is set to <full-file-path>::#<section id>.
When a duplicate section id is found in a definition, it is replaced by a new one,
and the new id is stored in the project."
  (interactive)
  (unless org-publish-project-alist (org-pm-load-all-project-data))
  (let (levels id ids projects)
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
          (setq projects (cons (org-pm-parse-project-def entry) projects))))
     "PROJECT_DEFS")
    (mapcar 'org-pm-check-add-project projects)
    (unless do-not-save-now (org-pm-save-all-project-data))
    (message "Org-pm defined %d projects" (length projects))))

(defun org-pm-parse-project-def (proj-node &optional template)
  "Temp. note: template is no longer used IZ Jan 5, 2014 (6:27 PM)
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
  "Add project definition contained in plist 'project' to org-publish-project-alist,
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

(defun query-make-folder (path &optional prompt-string)
  "If folder at path does not exist, then show dialog offering to user
    the option to create the indicated folder or to choose another path.
    If selected path does not exist, create folder."
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

(defun org-pm-select-project-from-menu ()
  "Present menu for selecting one project from the list of known projects."
  (interactive)
  (let* ((projects
          (-filter
           (lambda (pn) (not (string-match "^combined_" pn)))
           (-map 'car org-publish-project-alist)))
         index selected-project-name)
    (unless projects (setq projects '("new_project")))
    (setq index (grizzl-make-index projects))
    (setq selected-project-name
          (grizzl-completing-read "Choose a project:" index))
    (unless (assoc selected-project-name org-publish-project-alist)
      (org-pm-insert-new-project selected-project-name t))
    selected-project-name))

(defun org-pm-add-section-to-project ()
  "Present menu of existing project definitions.
Add selected project as tag to current section."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((project-name (org-pm-select-project-from-menu))
           (tags (plist-get (cadr (org-element-at-point)) :tags))
           (existing-projects
            (-map (lambda (p) (car (org-pm-parse-tag p)))
                  (-filter (lambda (tag) (string-match "^_.*_$" tag)) tags))))
      (unless (member project-name existing-projects)
        (org-set-tags-to (cons (concat "_" project-name "_") tags))))))

(defun org-pm-get-non-project-tags (section-plist)
  "Get those tags which are not enclosed in dash (=-=).
Function org-pm-make-yaml-matter inserts these tags as part of the YAML matter
in the file header for use by Jekyll/Octopress."
  (-reject (lambda (tag) (string-match "^_.*_$" tag)) (plist-get section-plist :tags)))

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
   '(org-publish-project-alist
     ;; org-pm-file-exports
     org-pm-section-exports
     org-pm-project-def-duplicates)
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

(defun org-pm-get-section-projects ()
  "Return list of projects found in the tags of the current section"
 (save-excursion
   (org-back-to-heading)
   (-map (lambda (p) (car (org-pm-parse-tag p)))
         (-filter (lambda (tag) (string-match "^_.*_$" tag))
                  (plist-get (cadr (org-element-at-point)) :tags)))))

(defun org-pm-parse-tag (tag &optional filename date)
  "Process property or tag, name of file containing component,
and date property of file or section to provide project, folder, filename strings.
Split tag to project, folder, filename if separated by @.
Construct blog entry style filename if date is provided.

If date is provided, convert date into jekyll- (hexo-, etc.) compatible
blog entry format, and prepend it.
Entry title 'thoughts-on-pre-processing', with date <2014-01-05 Sun 10:56>
becomes: '2014-01-05-thoughts-on-pre-processing'

Do not convert filename from title format.  That is done by function
org-pm-make-filename, which is called by org-pm-get-section-project-components."

  ;; strip enclosing underscores _
  (setq tag (replace-regexp-in-string
                   "^_" "" (replace-regexp-in-string "_$" "" tag)))
  ;; replace # by .
  (setq tag (replace-regexp-in-string "#" "." tag))
  ;; split into project, folder, filename
  ;; and provide "" as folder, filename where @ separators are missing
  (setq tag (-take 3 (split-string (concat tag "@@") "@")))
  ;; if tag had filename, use that instead of filename argument
  (if (> (length (caddr tag)) 0) (setq filename (caddr tag)))
  ;; provide extension
  (unless filename (setq filename "index"))
  (unless (file-name-extension filename)
    (setq filename (concat filename ".org")))
  ;; if date present, prepend date in jekyll blog-entry format
  (when (and date
             (string-match
              "^<\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\)"
              date))
    (setq filename (concat (substring date 1 11) "-" filename)))
  ;; return project, folder, new filename as list
  (setcdr (cdr tag) (list filename))
  tag)

;; Create final path to copy file, from list (project folder file)
;; received from org-pm-parse-tag.
;; Return (path . path-or-project)
;; path is used for copying.  path-or-project for display/menus.
;; If project def not found, path is nil.

(defun org-pm-make-target-path (proj-folder-file)
  "Create path of file for copying contents of current buffer to a project.
Combine base directory + folder + file from list proj-folder-file
to make target-path.
Return (path . project-name)
The car of the result is used to copy the component to the path.
The cdr of the result (project-name) is used for display and debugging."
  (let* ((pname (car proj-folder-file))
         (project (cdr (assoc pname org-publish-project-alist)))
         (folder (cadr proj-folder-file))
         (slash (if (string-match "/$" folder) "" "/"))
         (target-path
          (if project
              (concat (plist-get project :base-directory)
                      folder slash (caddr proj-folder-file)))))
    (cons target-path pname)))

;; Convert title of org-mode section entry into filename
;; Used by org-pm-get-section-project-components
(defun org-pm-make-filename (title &optional date)
  "Convert title of org-mode section entry into filename.
Remove non alphanumeric characters.
Replace spaces by dashes (-).
Strip initial or ending dashes.
Lowercase everything.
Strip : mm/dd/yy ... part from the end.
Entry title:
'Watching: Sacha_Chua Emacs_chat_with_magnar_sven (emacs_rocks): 12/08/13_14:54:11'
Becomes:
'watching-sacha-chua-emacs-chat-with-magnar-sven-emacs-rocks'"
  (let (filename
        (title-date-pos
         (string-match
          ": [[:digit:]]\\{2\\}/[[:digit:]]\\{2\\}/[[:digit:]]\\{2\\}"
          title)))
    (if title-date-pos
        (setq filename (substring title 0 title-date-pos))
      (setq filename title))
    (setq filename (downcase
                    (replace-regexp-in-string
                     "-+" "-"
                     (replace-regexp-in-string "[^[:alnum:]]" "-" filename))))
    (setq filename
          (replace-regexp-in-string
           "^-" "" (replace-regexp-in-string "-$" "" filename)))
    (when (and date
               (string-match
                "^<\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\)"
                date))
      (setq filename (concat (substring date 1 11) "-" filename)))
    filename))

(defun org-pm-get-section-project-paths ()
  "Build list of projects-folders-files to export sections of this buffer to.
The list is created from those sections whose tags specify projects,
i.e . tags enclosed in underscores: _projectname_
The list is passed to org-pm-copy-section-project-components for copying.
Each element in the list has the form:
<start-point of section> (project projectname folder filename)
                         (project projectname folder filename)
                         ... "
  (interactive)
  (let (components)
   (org-map-entries
    '(let* ((node (cadr (org-element-at-point)))
            (pspecs (-filter (lambda (tag) (string-match "^_.*_$" tag))
                             (plist-get node :tags)))
            name date)
       (message "pspecs: \n%s" pspecs)
       (if pspecs
         (let (section-entries)
          (setq name (plist-get node :raw-value))
          (setq date (plist-get node :DATE))
          (dolist (spec pspecs)
            (setq section-entries
                  (cons (org-pm-make-target-path
                         (org-pm-parse-tag
                          spec
                          (org-pm-make-filename name)
                          date)) section-entries)))
          (setq components (cons (cons (point) section-entries) components))))))
  ;;  (message "COMPONENTS: \n%s" components)
   ;; FIXME: TODO: save paths to disc in org-pm save file
   components))

;;; The next function should be reviewed.  May be scrapped.
(defun org-pm-get-section-project-targets ()
  "Return list of paths of files to which sections of current file are copied."
  (interactive)
  (let (sections
        (components
         (-map (lambda (clist)
                 (-map (lambda (c) (org-pm-make-displayable-target-path c))
                       clist))
               (-map (lambda (aclist) (cdr aclist))
                     (org-pm-get-section-project-components)))))
    (message "%s" (-flatten components))
    (-flatten components)))

;; pass components for posting (reporting) by calling function

(defun org-pm-export ()
  "Top level function for exporting file and sections to projects.
Copy both file and any sections specified by properties, tags
to the designated projects and folders.
Before copying, re-scan buffer to build list of targets for copying.
Add list of sections and target file paths to ... and save to disk."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((sections-with-paths (org-pm-get-section-project-paths)))
       (org-pm-export-sections-to-projects sections-with-paths)
       (setq org-pm-section-exports
             (assoc-replace org-pm-section-exports
                            (buffer-file-name (current-buffer))
                            sections-with-paths))
       (org-pm-save-all-project-data)))))

(defun org-pm-export-buffer-to-file (path-project)
  "path-project has the form (path . project-name).
If path is not nil, save current buffer to path."
  (let ((path (car path-project)))
    (when path
      (make-directory (file-name-directory path) t)
      (write-region nil nil path))))

(defun org-pm-export-sections-to-projects (&optional sections-with-paths)
  "Copy sections of this file to paths specified by tags.
List sections-with-paths is constructed by org-pm-get-section-project-paths."
  (interactive)
  (unless sections-with-paths
    (setq sections-with-paths (org-pm-get-section-project-paths)))
  (let ((buffer (current-buffer)))
    (dolist (section sections-with-paths)
      (org-pm-export-1-section-to-projects section buffer))))

(defun org-pm-export-1-section-to-projects (section-with-paths origin-buffer)
  "Copy section to temporary buffer, then save it to all
paths in the rest of section-with-paths."
  (let ((target-buffer (get-buffer-create "*org-pm-copy-buf*")))
    (set-buffer origin-buffer)
    (goto-char (car section-with-paths))
    (org-copy-subtree)
    (set-buffer target-buffer)
    (org-paste-subtree 1)
    (dolist (path-project (cdr section-with-paths))
      (org-pm-export-buffer-to-file path-project))
    (kill-buffer target-buffer)
    (message "exported section: %s" section-with-paths)))

(defun org-pm-save-buffer (specs buffer)
  "Save current buffer "
  (let ((target-path (org-pm-make-target specs)))
    (make-directory (file-name-directory target-path) t)
    (write-region nil nil target-path)))

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

(defun org-pm-list-exported-files ()
 "List files exported to projects."

)

(defun org-pm-make-yaml-front-matter (project-plist section-plist)
  "Make YAML front matter for Jekyll or Octopress.

If the value of property body-only in the project-plist is t, then add YAML
front matter at the beginning of the file when exporting.  This causes the file
to be processed by Jekyll or Octopress.

The following items are provided, depending on the values of corresponding properties
from global emacs variables, the project's p-list or the section's properties,
or the section's tags:

- author :: value of eamcs/orgmode variable author
- categories :: value of property CATEGORIES
- commments :: value of property COMMENTS
- date :: value of property DATE
- external-url :: value of property EXTERNAL-URL
- layout :: 'default' if no DATE property is set. 'blog' if DATE property is set.
            Value can be customized by setting property LAYOUT.
- permalink :: value of property PERMALINK
- published :: value of property PUBLISHED
- tags :: tags of section or values of property TAGS
- title :: from header of section.
- sharing :: from property SHARING
- footer :: from property FOOTER
"
  (when (plist-get project-plist :body-only)
    (insert "---\n")
    (let*
        ((title (plist-get section-plist :raw-value))
         (tags (org-pm-get-non-project-tags section-plist))
         (author (plist-get section-plist :AUTHOR))
         (categories (plist-get section-plist :CATEGORIES))
         (comments (plist-get section-plist :COMMENTS))
         (date (plist-get section-plist :DATE))
         (external-url (plist-get section-plist :EXTERNAL-URL))
         (layout (plist-get section-plist :LAYOUT))
         (permalink (plist-get section-plist :PERMALINK))
         (published (plist-get section-plist :PUBLISHED))
         (sharing (plist-get section-plist :SHARING))
         (footer (plist-get section-plist :FOOTER)))
      (unless layout (setq layout (if date "blog" "default")))
      (unless date (setq date (...)))
      (unless author (setq author (user-full-name)))
      (insert (format "title: %s\n" title))
      (insert (format "layout: %s\n" layout))
      (insert (format "author: %s\n" author))
      (insert (format "date: %s\n" date))
      (if external-url (insert (format "external-url: %s\n" external-url)))
      (if permalink (insert (format "permalink: %s\n" permalink)))
      (if published (insert (format "published: %s\n" published)))
      (if comments (insert (format "comments: %s\n" comments)))
      (if sharing (insert (format "sharing: %s\n" sharing)))
      (if footer (insert (format "footer: %s\n" footer)))
      (when categories
        (insert "categories:\n")
        (dolist (category (split-string categories ", "))
          (insert (format "- %s\n" category))))
      (when tags
        (insert "tags:\n")
        (dolist (tag (split-string tags ", "))
          (insert (format "- %s\n" tag)))))
    (insert "---\n")))

(defun org-pm-show-target-file-list (&optional section-components)
  "Create a list of paths of all files which the current file and its sections
outputs to.  Present this as a grizzl list for auto-complete search.
Open selected file."
  (interactive)
  (unless section-components
    (setq section-components (org-pm-get-section-project-targets)))
  (let* ((target-list
         (mapcar (lambda (t) (cdr t)) section-components))
         (index (grizzl-make-index target-list))
         answer)
    ;; (message "%s" target-list)
    (setq answer (grizzl-completing-read "Choose file to open: " index))
    (if (string-match "(undefined project)$" answer)
        (message "No file: %s" answer)
       (find-file answer))))

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
                                (or (string-match "^combined_" (car proj))
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

(defun org-pm-post-project-def ()
  "Select a project interactively and post its definition."
  (interactive)
  (let ((project-name
         (grizzl-completing-read
          "Which project? "
          (grizzl-make-index (mapcar 'car org-publish-project-alist)))))
    (message "THIS IS THE DEFINITION OF PROJECT %s:\n%s"
             project-name
             (assoc project-name org-publish-project-alist))))

(defun org-pm-edit-saved-project-data ()
  "Edit the file containing the global project data."
  (interactive)
  (find-file org-pm-project-data-file-path))

(defun org-pm-menu ()
  "Select and run an org-pm command from a grizzl-minibuffer menu list."
  (interactive)
  (setq  *grizzl-read-max-results* 32)
  (let* ((commands '(
                     org-pm-insert-new-project
                     org-pm-make-projects
                     org-pm-add-section-to-project
                     org-pm-export
                     org-pm-remove-section-from-project
                     org-pm-list-project-defs
                     org-pm-list-exported-files
                     org-pm-list-duplicate-project-defs
                     pm/edit-duplicate-project-def
                     org-pm-load-all-project-data
                     org-pm-reset-project-list
                     org-pm-edit-saved-project-data
                     org-pm-load-project-data
                     org-pm-save-project-data
                     org-pm-list-project-defs
                     org-pm-show-target-file-list
                     ))
         (menu (grizzl-make-index
               (-map (lambda (c)
                       (replace-regexp-in-string
                        "-"
                        " "
                        (replace-regexp-in-string
                         "^org-pm-" "" (symbol-name c))))
                     commands)))
        selection)
    (setq selection (grizzl-completing-read  "Select command: " menu))
    (eval
     (read (concat
            "(org-pm-"
            (replace-regexp-in-string " " "-" selection)
            ")")))))

;; Add org-mode hook for org-pm-key bindings.

(let ((org-pm-key-bindings
       (lambda ()
         (define-key org-mode-map (kbd "H-m H-m") 'org-pm-menu)
         (define-key org-mode-map (kbd "H-m t") 'org-pm-insert-new-project)
         (define-key org-mode-map (kbd "H-m m") 'org-pm-make-projects)
         (define-key org-mode-map (kbd "H-m e") 'org-pm-export)
         (define-key org-mode-map (kbd "H-m l") 'org-pm-list-project-defs)
)))

(add-hook 'org-mode-hook org-pm-key-bindings))

;; To initialize if present file is compiled after start time, run hook now.
(funcall org-pm-key-bindings)
