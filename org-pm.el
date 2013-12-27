
(defun org-babel-load-current-file ()
  (interactive)
  (org-babel-load-file (buffer-file-name (current-buffer))))  
(global-set-key (kbd "C-c C-=") 'org-babel-load-current-file)

(defvar org-pm-project-components nil
"Store the projects that each file or its sections belong to.
For each file it store a list: (full-path project1 project2 ...)")

(defvar org-pm-auto-parse nil
  "If not nil, automatically parse a org-mode buffer
 for org-pm data before saving it.")

(defvar org-pm-auto-copy nil
"If not nil, automatically copy file components to a project to the 
project's source folder before publishing.")

(defvar org-pm-project-data-file-path nil
"Path of file for storing org-publish-project-alist and 
org-pm-project-components.  If nil, the path is deduced from the existence
of .emacs.d folder in user's home directory. 
If .emacs.d exists, use ~/.emacs.d/savefile/org-publish-project-alist
else use ~/.org-publish-project-alist.  
Create savefile folder if it does not exist.")

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

(defvar org-pm-project-template-name 
  (concat (file-name-directory (or load-file-name (buffer-file-name)))
          "org-pm-project-template.org")
"Full path of file containing template of project definition for 
projects generated automatically with org-pm-make-project-template-file.
The path is initialized at code loading time by function org-pm-init-project-template-name.
org-pm-make-project-template-file uses it to make project templates.")

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
  (setq alist (cons (cons key newlist) alist))
  )

(defun org-pm-make-default-project-plist ()
  "Construct default plist for publishing a project in html."
  (let ((plist (copy-sequence org-pm-default-project-plist)))
    (setq plist (plist-put plist :base-directory
                           (file-truename org-pm-default-project-org-folder)))
    (setq plist (plist-put plist :publishing-directory 
                           (file-truename org-pm-default-project-html-folder)))))

(defun org-pm-add-project-file (project-name file)
  "In list org-pm-project-components, add the project-name to the list 
of projects that file bel ongs. "
  (setq org-pm-project-components
        (assoc-add org-pm-project-components file project-name)))

(defun org-pm-remove-project-file (project-name file)
  "In list org-pm-project-components, add the project-name to the list 
of projects that file belongs. "
  (setq org-pm-project-components
        (assoc-add org-pm-project-components file project-name)))

(defun org-pm-save-projects-and-components ()

)

(defun org-pm-add-project-to-file-header (project-name)

)

(defun org-pm-get-file-projects ()

)

(defun org-pm-get-section-projects ()

)

(defun org-pm-make-project-template-file (project)
  "Read file containing template of project definition for 
  projects generated automatically with org-pm-make-project-template-file,
  from the synonymous file in the org-pm project source
  code folder.  Replace relevant parts of the template with 
  information specific to the project.  
  Finally, save the template the project's base-directory."
  
  ;; Consider using with-temp-buffer for constructing the template file contents

  (save-excursion
    (find-file org-pm-project-template-name)
    (let* ((filename (file-name-nondirectory org-pm-project-template-name))
           (plist (cdr project))
           (path (concat (plist-get plist :base-directory) "/" filename)))
      (unless (file-exists-p path)
        (beginning-of-buffer)
        (replace-string "PROJECTNAME" (car project))
        (beginning-of-buffer)
        (replace-string "BASEDIRECTORY" (plist-get plist :base-directory))
        (beginning-of-buffer)
        (replace-string "PUBLISHINGDIRECTORY" (plist-get plist :publishing-directory))
        (beginning-of-buffer)
        (replace-string "EXCLUDEFILE" filename)
        (write-file path)
        ;; leave buffer open for edits ?
        ;; (kill-buffer (current-buffer))
        ))))

;; (org-pm-make-project-template-file "test")

(defun d1-org-pm-parse-file ()
  "DRAFT Dec 20, 2013 (9:11 PM)"
  ()
)

(defun org-pm-make-default-project-plist ()
  "Construct default plist for publishing a project in html."
  (let ((plist (copy-sequence org-pm-default-project-plist))
        (root (file-name-directory (buffer-file-name (current-buffer)))))
    (plist-put plist :base-directory (concat root "org"))
    (plist-put plist :publishing-directory (concat root "html"))))

(defun org-pm-make-projects ()
  "Construct the projects for all project definitions found in current file.
Project definitions are those nodes which are contained in nodes tagged as
PROJECT_DEFS."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (org-pm-check-project-config-nodes)
      (let
          (project-def 
           id-list
           id
           (template (org-pm-make-default-project-plist))
           (supernodes
            (org-map-entries '(cadr (org-element-at-point)) "PROJECT_DEFS"))
           project-def-list tags begin level)
        (dolist (node supernodes)
          (cond ((and
                  (plist-get node :tags)
                  (setq begin (plist-get node :contents-begin)))
                 (setq level (+ 1 (plist-get node :level)))
                 (save-excursion
                   (save-restriction
                     (narrow-to-region begin (plist-get node :contents-end))
                     (dolist 
                         (proj-node (org-map-entries '(cadr (org-element-at-point))))
                       (cond ((equal level (plist-get proj-node :level))
                              (setq id (plist-get proj-node :id))
                              (when (member id id-list)
                                (error "DUPLICATE ID FOUND")) ;; FIXME: Better error!
                              (setq id-list (cons id id-list))
                              (setq project-def
                                    (org-pm-parse-project-def proj-node template))
                              (push project-def 
                                    project-def-list)))))))))
        project-def-list))))

(defun org-pm-check-add-projects (projects)
  "Check each project in projects: If it is defined in a component with a different ID,
then ask user to select which project to keep. 
NOTE: 
      Possibly offer to remove project definitions?  
      Offer to remove projects after checking if they are in a different place?
      Or post info about replaced projects so that user can decide how do deal with them?
      The posting can be done one project at a time.  No need to build a list."
  (dolist (project projects) (org-pm-query-select-project project)
    (let ((duplicate (assoc (car project) org-publish-project-alist)))
      (if duplicate (setq project (org-pm-query-select-project project duplicate)))
      (org-pm-add-project project))))

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

(defun org-pm-check-project-config-nodes ()
  "Check if a node tagged POJECT_CONFIGS has invalid content before first subnode."
  (org-map-entries 
   '(let
        (begin (node (cadr (org-element-at-point))))
      (if (and 
           (setq begin (plist-get node :contents-begin))
           (member "PROJECT_DEFS" (plist-get node :tags))
           )
          ;; only works with if statement in the following way. Why?
          (if (equal 0 (string-match 
                        "\\*+ " 
                        (buffer-substring-no-properties begin (plist-get node :contents-end))))
              "ok"
            (error (format 
                    "ERROR: Node named '%s' should be empty before first subnode.\n%s"
                    (plist-get node :raw-value)
                    "Please remove all content before first subnode.")))
        ))
   "PROJECT_DEFS"))

(defun org-pm-parse-project-def (proj-node template)
  "Return a project definition plist for the node represented by proj-node
org-element plist."
  (let ((pdef (copy-sequence template))
        (pname (plist-get proj-node :raw-value))
        (begin (plist-get proj-node :contents-begin)))
    (setq pdef (plist-put pdef :project-name pname))
    (setq pdef (plist-put pdef :node-id (org-id-get-create)))
    (setq pdef (plist-put pdef :node-filename
                          (buffer-file-name (current-buffer))))
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
                    (plist-put pdef (intern (concat ":" prop-name)) prop-value))
              ))))))
    (cons pname pdef)))

(defun org-pm-add-file-to-project ()
  "Add the file of the current buffer to a project selected or input by the user.
    If the project selected/input by the user is not already in the file's project list:
    - If no project of that name exists, request that the project be defined using
    org-pm or other methods.
    - If no project at all exists, then offer to create default project.
    - Add the selected project to the file's list in org-pm-project-components.
    - Save org-pm-project-components.
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
    (org-pm-save-projects-and-components)
    (org-pm-make-project-template-file project)
    (message 
     "Added project named: %s to file: %s\nBase directory is: %s\nPublishing directory is: %s"
     project-name
     (file-name-nondirectory (buffer-file-name (current-buffer)))
     (plist-get (cdr project) :base-directory)
     (plist-get (cdr project) :publishing-directory))))

(defun org-pm-query-make-default-project (project-name)
  "Make a project using default settings and project-name as name."
  (unless (y-or-n-p (format "Create project '%s'? " project-name))
    (error "Project creation cancelled."))
  (let (plist)
    (setq plist (org-pm-make-default-project-plist))
    (setq plist
          (plist-put 
           plist :base-directory
           (query-make-folder (plist-get plist :base-directory) 
                              "Base directory:")))
    (setq plist 
          (plist-put 
           plist :publishing-directory
           (query-make-folder (plist-get plist :publishing-directory) 
                              "Publishing directory:")))
    (cons project-name plist)))

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
