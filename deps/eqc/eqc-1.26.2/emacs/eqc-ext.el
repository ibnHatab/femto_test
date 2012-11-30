;; General TODO
;; * 'gen should do different things if the selection is empty or not!?
(require 'tempo)

(defun eqc-erlang-mode-hook ()
  (eqc-mode-init)
  )

(defvar eqc-menu-items
  '(
    eqc-prop-menu-items
    eqc-gen-menu-items
    eqc-statem-menu-items
    eqc-fsm-menu-items
    eqc-c-menu-items
    eqc-separator-menu-item
    eqc-man-menu-items)
  "The EQC menu")

(defvar eqc-prop-menu-items
  '(("Properties"
     (("Full module header" tempo-template-eqc-full-header)
      ("Include eqc" tempo-template-eqc-import-eqc)
      ("Export all" tempo-template-eqc-export_all)
      nil
      ("Property" tempo-template-eqc-property)
      
      ;; MACROS
      nil
      ('mac "IMPLIES"   "prop-implies"   ("Precondition" 'prop))
      ('mac "FORALL"    "prop-forall"    ("Value" "Generator" 'prop))
      ('mac "WHENFAIL"  "prop-whenfail"  ("Action" 'prop))
      ('mac "TRAPEXIT"  "prop-trapexit"  ('prop) )
      ('mac "ALWAYS"    "prop-always"    ("Number of times to test" 'prop))
      ('mac "SOMETIMES" "prop-sometimes" ("Number of times to test" 'prop))
      
      ;; Functions
      nil
      ('fun "aggregate" "prop-aggregate" ("List of terms to aggregate" 'prop))
      ('fun "classify"  "prop-classify"  ("Boolean expression" "Label" 'prop))
      ('fun "collect"   "prop-collect"   ("Collected term" 'prop))
      ('fun "fails"     "prop-fails"     ('prop))
      ('fun "measure"   "prop-measure"   ("Label" "Term (or list of terms) to measure" 'prop))
      
      )))
  "Properties")

(defvar eqc-gen-menu-items
  '(("Generators"
     (("Sized generator" tempo-template-eqc-sized_gen)
      
      ;; MACROS
      nil
      ('mac "LET"             "gen-let"           ("Bound variable" "Generator for variable" 'gen))
      ('mac "SIZED"           "gen-sized"         ("Size variable" 'gen))
      ('mac "SUCHTHAT"        "gen-suchthat"      ("Bound variable" "Generator for variable" "Condition"))
      ('mac "SUCHTHATMAYBE"   "gen-suchthatmaybe" ("Bound variable" "Generator for variable" "Condition"))
      ('mac "SHRINK"          "gen-shrink"        ('gen "List of generators"))
      ('mac "SHRINKWHILE"     "gen-shrinkwhile"   ("Bound variable" "Generator for variable" "Condition"))
      ('mac "LETSHRINK"       "gen-letshrink"     ("Bound variable" "Generator (should be a list)" 'gen))
      ('mac "LAZY"            "gen-lazy"          ('gen))

      ;; Functions
      nil
      ('fun "bool"        "gen-bool"        nil)
      ('fun "char"        "gen-char"        nil)
      ('fun "choose"      "gen-choose"      ("Low value" "High value"))
      ('fun "default"     "gen-default"     ("Default value" 'gen))
      ('fun "elements"    "gen-elements"    ("List of elements"))
      ('fun "eval/1"      "gen-eval-1"      ('gen))
      ('fun "eval/2"      "gen-eval-2"      ("Environment" 'gen))
      ('fun "fault"       "gen-fault"       ("Fault generator" 'gen))
      ('fun "fault-rate"  "gen-fault-rate"  ("M (in M out of N)" "N (in M out of N)" 'gen))
      ('fun "frequency"   "gen-frequency"   ('gen))
      ('fun "function0"   "gen-function0"   ('gen)) 
      ('fun "function1"   "gen-function1"   ('gen)) 
      ('fun "function2"   "gen-function2"   ('gen)) 
      ('fun "function3"   "gen-function3"   ('gen)) 
      ('fun "function4"   "gen-function4"   ('gen)) 
      ('fun "growingelements" "gen-growingelements" ("List of elements"))
      ('fun "int"         "gen-int"         nil)
      ('fun "largeint"    "gen-largeint"    nil)
      ('fun "less_faulty" "gen-less_faulty" ("Decreasing factor" 'gen))
      ('fun "list"        "gen-list"        ('gen))
      ('fun "more_faulty" "gen-more_faulty" ("Increasing factor" 'gen))
      ('fun "nat"         "gen-nat"         nil)
      ('fun "no_faults"   "gen-no_faults"   ('gen))
      ('fun "noshrink"    "gen-noshrink"    ('gen))
      ('fun "oneof"       "gen-oneof"       ('gen))
      ('fun "orderedlist" "gen-orderedlist" ('gen))
      ('fun "parameter/1" "gen-parameter-1" ('gen))
      ('fun "parameter/2" "gen-parameter-2" ('gen "Default value"))
      ('fun "prop_shrinks_without_duplicates" "gen-prop_shrinks_without_duplicates" ('gen))
      ('fun "real"        "gen-real"        ('gen))
      ('fun "resize"      "gen-resize"      ("New size" 'gen))
      ('fun "return"      "gen-return"      ('gen))
      ('fun "sample"      "gen-sample"      ('gen))
      ('fun "sampleshrink" "gen-sampleshrink" ('gen))
      ('fun "shrink_int"  "gen-shrink_int"  ("N (range N to M)" "M (range N to M)" "Value X"))
      ('fun "shrink_list" "gen-shrink_list" ("List (shrinks to any sublist)"))
      ('fun "shrink_without_duplicates" "gen-shrink_without_duplicates" ('gen))
      ('fun "timeout"     "gen-timeout"     ("Timeout (in ms)" 'gen))
      ('fun "vector"      "gen-vector"      ("Vector size" 'gen))
      ('fun "wheighted_default" "gen-weighted_default" ("Default ({wheight,Value})" 'gen))
      ('fun "with_parameter"    "gen-with_parameter"   ("Parameter" "Value" 'gen))
      ('fun "with_parameters"   "gen-with_parameters"  ("List of {Parameter,Value}" 'gen))

      )))
  "Generators")

(defvar eqc-statem-menu-items
  '(("State machine specs"
     (("Complete eqc_statem spec" tempo-template-eqc-eqc_statem)
      nil
      ("Full statem module header" tempo-template-eqc-full-statem-header)
      nil
      ("Include eqc_statem" tempo-template-eqc-import-eqc_statem)
      ("initial_state" tempo-template-eqc-initial_state)
      ("command" tempo-template-eqc-command)
      ("next_state" tempo-template-eqc-next_state)
      ("precondition" tempo-template-eqc-precond)
      ("postcondition" tempo-template-eqc-postcond)
      ("State Machine Property" tempo-template-eqc-statem_property)
      nil
      ("Symbolic function call" tempo-template-eqc-symbolic-call)
      ("Duplicate function clause" duplicate-function-clause)
      ("Duplicate function clause header" duplicate-function-clause-header)

      ;; MACROS
                                        ;nil

      ;; Functions
      nil
      ('fun "commands/1"         "statem-commands-1"      ("Module"))
      ('fun "commands/2"         "statem-commands-2"      ("Module" "Initial state"))
      ('fun "more_commands"      "statem-more_commands"  ("Increase by factor" 'gen))
      ('fun "run_commands/2"     "statem-run_commands-2"  ("Module" "List of commands"))
      ('fun "run_commands/3"     "statem-run_commands-3"  ("Module" "List of commands" "Environment"))
      
      )))
  "State machine specifications")


(defvar eqc-fsm-menu-items
  '(("Finite State Machine (FSM) specs"
     (("Complete eqc_fsm spec" tempo-template-eqc-eqc_fsm)
      nil
      ("Full eqc_fsm module header" tempo-template-eqc-full-eqc_fsm-header)
      nil
      ("Include eqc_fsm" tempo-template-eqc-import-eqc_fsm)
      ("initial_state" tempo-template-eqc-eqc_fsm-initial_state)
      ("initial_state_data" tempo-template-eqc-eqc_fsm-initial_state_data)
      ;; ("command" tempo-template-eqc-command-fsm)
      ("next_state_data" tempo-template-eqc-eqc_fsm-next_state_data)
      ("precondition" tempo-template-eqc-eqc_fsm-precond)
      ("postcondition" tempo-template-eqc-eqc_fsm-postcond)
      ("FSM Property" tempo-template-eqc-eqc_fsm-property)
      ("Transition weight" tempo-template-eqc-eqc_fsm-weight)
      nil
      ("Symbolic function call" tempo-template-eqc-symbolic-call)
      ("Duplicate function clause" duplicate-function-clause)
      ("Duplicate function clause header" duplicate-function-clause-header)

      ;; MACROS
      ;; nil

      ;; Functions
      nil
      ('fun "analyze"            "fsm-analyze"            ("Module"))
      ('fun "automate_weights/1" "fsm-automate_weights-1" ("Module"))
      ('fun "automate_weights/2" "fsm-automate_weights-2" ("Module" "ImageType"))
      ('fun "commands/1"         "fsm-commands-"          ("Module"))
      ('fun "commands/2"         "fsm-commands-2"         ("Module" "Initial state"))
      ('fun "dot"                "fsm-dot"                ("Module"))
      ('fun "run_commands/2"     "fsm-run_commands-2"     ("Module" "List of commands"))
      ('fun "run_commands/3"     "fsm-run_commands-3"     ("Module" "List of commands" "Environment"))
      ('fun "state_names"        "fsm-state_names"        ("Module"))
      ('fun "visualize/1"        "fsm-visualize-1"        ("Module"))
      ('fun "visualize/2"        "fsm-visualize-2"        ("Module" "ImageType"))
      )))
  "State machine specifications")

(defvar eqc-c-menu-items
  '(("C code specifications"
     (
      ;; ("Complete eqc_fsm spec" tempo-template-eqc-eqc_fsm)
      ;; nil
      ("Full eqc_c module header" tempo-template-eqc-full-eqc_c-header)
      nil
      ("Include eqc_c" tempo-template-eqc-import-eqc_c)

      ;; MACROS
      ;; nil

      ;; Functions
      nil
      ('fun "add_to_ptr"         "c-add_to_ptr"           ("Pointer" "N"))
      ('fun "alloc"              "c-alloc"                ("Type" "Value"))
      ('fun "array_index/2"      "c-array_index-2"        ("Pointer" "Index"))
      ('fun "array_index/3"      "c-array_index-3"        ("Pointer" "Index" "New value"))
      ('fun "cast_ptr"           "c-cast_ptr"             ("Type" "Pointer"))
      ('fun "create_array"       "c-create_array"         ("Type" "Values (list)"))
      ('fun "create_string"      "c-create_string"        ("String"))
      ('fun "deref"              "c-deref"                ("Pointer"))
      ('fun "free"               "c-free"                 ("Pointer"))
      ('fun "read_array"         "c-read_array"           ("Pointer" "Length"))
      ('fun "read_string"        "c-read_string"          ("Pointer"))
      ('fun "restart"            "c-restart"              nil)
      ('fun "set_timeout"        "c-set_timeout"          ("Timeout (ms)"))
      ('fun "sizeof"             "c-sizeof"               ("Type"))
      ('fun "start/1"            "c-start-1"              ("Module"))
      ('fun "start/2"            "c-start-2"              ("Module" "Source-file"))
      ('fun "start/3"            "c-start-3"              ("Module" "Sourse-file" "Options"))
      ('fun "start_functions"    "c-start_functions"      ("Module" "Functions"))
      ('fun "stop"               "c-stop"                 nil)
      ('fun "store"              "c-store"                ("Pointer" "New value"))
      ('fun "store_array"        "c-store_array"          ("Pointer" "Values (list)"))
      )))
  "QuickCheck C-code")

(defvar eqc-separator-menu-item
  '((nil)))

(defvar eqc-man-menu-items
  '(("Manual pages"
     (("Not yet available" nil)
      ;; ("eqc_gen" eqc-man-eqc_gen)
      ;; ("eqc_statem" eqc-man-eqc_statem)
      )))
  "Manual pages")

;; Initialize the eqc-mode
(defun eqc-mode-init ()
  "Initialize the eqc-mode"
  (require 'tempo)
  (setq debug-on-error t)
  (setq tempo-interactive t)
  (setq max-lisp-eval-depth 500)
  (unless (boundp 'eqc-max-menu-length)
    (setq eqc-max-menu-length 30))
  (eqc-mode-keymap-init)
  (expand-menu eqc-menu-items)
  (eqc-menu-init "QuickCheck" eqc-menu-items (make-sparse-keymap "QuickCheck"))
  )

;; Top-level function for menu expansion (expansion of 'prop, 'gen, etc)
(defun expand-menu (items)
  (setq items (reverse items))
  (if (< eqc-max-menu-length 15) (setq eqc-max-menu-length 15) nil)
  (while items
    (set (car items) (expand-menu-macros (symbol-value (car items))))
    (set (car items) (eqc-split-long-menus (symbol-value (car items))))   
    
    (setq items (cdr items)))
  )

(defun eqc-split-long-menus (item)
  "Splitting long (sub-)menus"
  (setq item (car item))
  (cond 
   ( (null item) ;; Separator
     (list item))
   ( (symbolp (nth 1 item)) ;; Menu item
     (list item))
   ( t ;; Submenu
     (list (list (car item) (eqc-split-long-menus-inner (car (cdr item)))))
     )
   )
  )

(defun eqc-split-long-menus-inner (menu)
  (cond
   ( (< (length menu) eqc-max-menu-length) ;; Shorter than max-length
     menu
     )
   ( t 
     (let ((menu1 (nthcar eqc-max-menu-length menu)) (menu2 (nthcdr eqc-max-menu-length menu)))
       (append menu1 (list (list "More..." (eqc-split-long-menus-inner menu2))))
       )
     )
   ))

;; (defun eqc-split-long-menus (menu nbr)
;;   (cond
;;    ( (consp menu)
;;       (cond
;;        ( (< nbr eqc-max-menu-length)
;;              (let ((item (car menu)) (items (cdr menu)))
;;                (cond 
;;                 ( (symbolp (nth 1 item))
;;                       (cons item (eqc-split-long-menus items (+ nbr 1))))
;;                 ( (null item)
;;                       (cons item (eqc-split-long-menus items (+ nbr 1))))
;;                 ( t ;; Submenu
;;                       (cons (list (car item) (eqc-split-long-menus (car (cdr item)) 0))
;;                                 (eqc-split-long-menus items (+ nbr 1))))
;;                 )))
;;        ( t ;
;;              (list (list "More..." (eqc-split-long-menus menu 0))))))
;;    ( t nil )))

(defun nthcar (n list)
  "Return first N elements of LIST.
If LIST length below N, return entire list.
If LIST is nil, return nil."
  (reverse (nthcdr (- (length list) n) (reverse list))))

;; Expand menu-item macros ('mac and 'fun)
(defun expand-menu-macros (items)
  (let ((menu '()))
    (setq items (reverse items))
    (while items
      (cond 
       ( (equal (nth 0 (car items)) ''mac)
         (setq menu (cons (expand-macro-def (car items)) menu)) )
       ( (equal (nth 0 (car items)) ''fun)
         (setq menu (cons (expand-fun-def (car items)) menu)) )
       ( (null (car items))
         (setq menu (cons nil menu)))
       (t ;; Either a Name Fun or a Name and a submenu
        (cond 
         ( (symbolp (nth 1 (car items)))
           (setq menu (cons (car items) menu)))
         (t ;;  (listp (nth 1 (car items))) 
          (setq menu (cons (list (car (car items)) (expand-menu-macros (nth 1 (car items)))) menu)))
         )
        )
       )
      (setq items (cdr items)))
    menu
    )
  )     

(defun expand-macro-def (macro)
  (expand-def macro "macro" "Macro" "?")
  )

(defun expand-fun-def (fun)
  (expand-def fun "fun" "Function" "")
  )

(defun expand-def (def type typename prefix)
  (let ((name (nth 1 def)) (funname (nth 2 def)) (args (nth 3 def)))
    (funcall (symbol-function 'tempo-define-template)
             (concat "eqc-" type "-" funname)
             (build-def-str name args prefix)
             (concat type "-" funname))
    (list (concat typename " " name) 
          (intern (concat "tempo-template-eqc-" type "-" funname)))
    )
  )

(defun build-def-str (name args prefix)
  (cons (concat prefix name "(")
        (nconc (build-macro-args-str args nil) (list ")")))
  )

(defun build-macro-args-str (args comma)
  (cond
   ( (null args)
     nil)
   ( (equal comma t)
     (cons "," (build-macro-args-str args nil)))
   ( t
     (let ((rest (build-macro-args-str (cdr args) t)))
       (cond 
        ( (equal (car args) ''prop)
          (nconc (list '> 'n '> 'r>) rest))
        ( (equal (car args) ''gen)
          (nconc (list '> 'r>) rest))
        ( t
          (cons (list 'p (concat (car args) ": ") (intern (car args))) rest)
          ))))
   ))    

;; Definition of keymap for eqc-mode
(defun eqc-mode-keymap-init ()
  (if (symbolp 'erlang-mode-map)
      (let ((map erlang-mode-map))
        (define-key map "\C-qf"   'tempo-template-eqc-macro-prop-forall)
        (define-key map "\C-qi"   'tempo-template-eqc-macro-prop-implies)
        (define-key map "\C-qp"   'tempo-template-eqc-property)
        (define-key map "\C-qh"   'tempo-template-eqc-full-header)
        (define-key map "\C-ql"   'tempo-template-eqc-macro-gen-let)
        (define-key map "\C-qc"   'tempo-template-eqc-symbolic-call)
        (define-key map "\C-qs"   'tempo-template-eqc-statem-property)
        (define-key map "\C-qt"   'tempo-template-eqc-macro-prop-trapexit)
        (define-key map "\C-qw"   'tempo-template-eqc-macro-prop-whenfail)
        (define-key map "\C-q\C-fc"   'duplicate-function-clause)
        (define-key map "\C-q\C-fh"   'duplicate-function-clause-header)
        )
    nil)
  )

;; Adding the menu in (hopefully) the right place
(defun eqc-menu-init (name items keymap)
  "Init the menu"
  (cond 
   (erlang-xemacs-p
    (let ((menu (erlang-menu-xemacs name items nil)))
      ;; Trying to mimic the Erlang menu behavior...
      (if (symbolp 'erlang-mode-map)
          (let ((erl-map erlang-mode-map))
            (let ((expr (list 'or
                              (list 'eq erl-map 'global-map)
                              (list 'eq erl-map (list 'current-local-map))
                              (list 'symbol-value
                                    (list 'car-safe
                                          (list 'rassq
                                                erl-map
                                                'minor-mode-map-alist))))))
              (setq menu (cons (car menu)
                               (cons ':included (cons expr (cdr menu)))))
              )))
      (funcall (symbol-function 'add-submenu) nil menu "Erlang")))
   ((>= erlang-emacs-major-version 19)
    (let ((menu (erlang-menu-make-keymap name items)))
      (define-key (current-local-map) (vector 'menu-bar (intern name)) menu)))
   (t nil)))

;;
;;
;; Specialized tempo macros
;;
;;

;; Function that makes it possible to includ a tempo macro inside
;; another tempo macro
(defun eqc-tempo-include (&rest args)
  "Include a template inside another template.
Technically, this function returns the `tempo' attribute`(l ...)' which
can contain other `tempo' attributes. "
  (let ((res '())
        entry)
    (while args
      (setq entry (car args))
      (while entry
        (setq res (cons (car entry) res))
        (setq entry (cdr entry)))
      (setq args (cdr args)))
    (cons 'l (nreverse res))))

;; Header block + includes etc...
(tempo-define-template 
 "eqc-full-header"
 '(o 
   (eqc-tempo-include erlang-skel-normal-header)
   (eqc-tempo-include tempo-template-eqc-import-eqc)
   n
   (eqc-tempo-include tempo-template-eqc-export_all) 
   )
 "full-header"
 )

(tempo-define-template 
 "eqc-full-statem-header"
 '(o 
   (eqc-tempo-include erlang-skel-normal-header)
   (eqc-tempo-include tempo-template-eqc-import-eqc)
   (eqc-tempo-include tempo-template-eqc-import-eqc_statem)
   n
   (eqc-tempo-include tempo-template-eqc-export_all) 
   )
 "full-statem-header"
 )

(tempo-define-template 
 "eqc-full-eqc_fsm-header"
 '(o 
   (eqc-tempo-include erlang-skel-normal-header)
   (eqc-tempo-include tempo-template-eqc-import-eqc)
   (eqc-tempo-include tempo-template-eqc-import-eqc_fsm)
   n
   (eqc-tempo-include tempo-template-eqc-export_all) 
   )
 "full-statem-header"
 )

(tempo-define-template 
 "eqc-full-eqc_c-header"
 '(o 
   (eqc-tempo-include erlang-skel-normal-header)
   (eqc-tempo-include tempo-template-eqc-import-eqc)
   (eqc-tempo-include tempo-template-eqc-import-eqc_c)
   n
   (eqc-tempo-include tempo-template-eqc-export_all) 
   )
 "full-c-header"
 )


;; TODO should use a 'user'-set variable
(tempo-define-template "eqc-import-eqc"
                       '(& "-include_lib(\"eqc/include/eqc.hrl\")." > n)
                       "import-eqc")

(tempo-define-template "eqc-import-eqc_statem"
                       '(& "-include_lib(\"eqc/include/eqc_statem.hrl\")." > n)
                       "import-eqc_statem")

(tempo-define-template "eqc-import-eqc_fsm"
                       '(& "-include_lib(\"eqc/include/eqc_fsm.hrl\")." > n)
                       "import-eqc_fsm")

(tempo-define-template "eqc-import-eqc_c"
                       '(& "-include_lib(\"eqc/include/eqc_c.hrl\")." > n)
                       "import-eqc_c")

(tempo-define-template "eqc-export_all"
                       '(& "-compile(export_all)." > n)
                       "export_all")

;; Sized generator
(tempo-define-template 
 "eqc-sized_gen"
 '(& (p "generator name: " gen_name) "() -> " > n
     "?SIZED(Size," (s gen_name) "(Size))." > n n
     (s gen_name) "(0) ->" > n 
     > p ";" n
     (s gen_name) "(Size) -> " > n
     > p "." n)
 "sized_gen")

;; Property
(tempo-define-template
 "eqc-property"
 '( "prop_" (p "Property name (prop_NAME): " prop) "(" 
    (p "Property parameters (X,Y,...): " params) ") ->" > n
    > (eqc-tempo-include tempo-template-eqc-macro-prop-forall) "." n )
 "property")

;; State machine property
(tempo-define-template
 "eqc-statem_property"
 '( "prop_" (p "Property name (prop_NAME): " prop) "() ->" > n
    "?FORALL(Cmds,commands(" 
    (if (equal (tempo-lookup-named 'ask_for_module) "no")
        '(l (tempo-save-named 'module ""))
      '(l (p "Module for commands [?MODULE]: " module t)
          ))
    (default-insert 'module "?MODULE") 
    ")," > n 
    "begin" > n
    "{H,S,Res} = run_commands("
    (default-insert 'module "?MODULE")
    ",Cmds)," > n
    "?WHENFAIL(" > n
    "io:format(\"History: ~p\\nState: ~p\\nRes: ~p\\n\",[H,S,Res])," > n
    (if (is-region-active) 'r> "Res == ok")
    ")" > n
    "end)." > n)
 "statem_property")

;; FSM property
(tempo-define-template
 "eqc-eqc_fsm-property"
 '( "prop_" (p "Property name (prop_NAME): " prop) "() ->" > n
    "?FORALL(Cmds,commands(" 
    (if (equal (tempo-lookup-named 'ask_for_module) "no")
        '(l (tempo-save-named 'module ""))
      '(l (p "Module for commands [?MODULE]: " module t)
          ))
    (default-insert 'module "?MODULE") 
    ")," > n 
    "begin" > n
    "{H,S,Res} = run_commands("
    (default-insert 'module "?MODULE")
    ",Cmds)," > n
    "?WHENFAIL(" > n
    "io:format(\"History: ~p\\nState: ~p\\nRes: ~p\\n\",[H,S,Res])," > n
    (if (is-region-active) 'r> "Res == ok")
    ")" > n
    "end)." > n)
 "eqc_fsm-property")

;; Complete eqc_statem spec skeleton
(tempo-define-template 
 "eqc-eqc_statem"
 '( 
   (eqc-tempo-include tempo-template-eqc-full-statem-header)
   n 
   (let ((last-nonmenu-event t)) ;; Ensure use of kbd
     (tempo-save-named 'is_rec (if (y-or-n-p "Use a state record?") "yes" "no")))
   
   (if (equal (tempo-lookup-named 'is_rec) "yes") 
       '(l "-record(" (p "Record name: " rec) ",{" p "})." n n)
     (tempo-save-named 'rec ""))
   
   (eqc-tempo-include tempo-template-eqc-initial_state)
   n
   (eqc-tempo-include tempo-template-eqc-command)
   n
   (eqc-tempo-include tempo-template-eqc-next_state)
   n
   (eqc-tempo-include tempo-template-eqc-precond)
   n
   (eqc-tempo-include tempo-template-eqc-postcond)
   n
   (tempo-save-named 'ask_for_module "no")
   (eqc-tempo-include tempo-template-eqc-statem_property)
   )
 "eqc_statem"
 )


;; eqc_statem callback function defaults
;; TODO Use proper EDOC comments...
(tempo-define-template "eqc-initial_state"
                       '(& "%% Initialize the state" > n
                           "initial_state() ->" > n
                           > (if (equal (tempo-lookup-named 'rec) "") "[]" '(l "#" (s rec) "{}"))
                           "." n)       
                       "initial_state")

(tempo-define-template "eqc-command"
                       '(& "%% Command generator, S is the state" > n
                           "command(S) ->" > n
                           > "oneof([" p > n > "])." n)
                       "command")

(tempo-define-template "eqc-next_state"
                       '(& "%% Next state transformation, " 
                           "S is the current state" > n
                           "next_state(S,_V,{call,_,_,_}) ->" > n
                           > "S." p n)
                       "next_state")

(tempo-define-template "eqc-precond"
                       '(& "%% Precondition, checked before command " 
                           "is added to the command sequence" > n
                           "precondition(_S,{call,_,_,_}) ->" > n
                           > "true." p n)
                       "precondition")

(tempo-define-template "eqc-postcond"
                       '(& "%% Postcondition, checked after command has "
                           "been evaluated"> n
                           "%% OBS: S is the state before "
                           "next_state(S,_,<command>) " > n
                           "postcondition(_S,{call,_,_,_},_Res) ->" > n
                           > "true." p n)
                       "postcondition")

(tempo-define-template "eqc-symbolic-call"
                       '("{call," (p "Module name [?MODULE]: " module t)
                         (default-insert 'module "?MODULE")
                         ","
                         (p "Function name: ") ",[" r "]}"))


;; Complete eqc_fsm spec skeleton
(tempo-define-template 
 "eqc-eqc_fsm"
 '( 
   (eqc-tempo-include tempo-template-eqc-full-eqc_fsm-header)
   n 
   (let ((last-nonmenu-event t)) ; Ensure use of kbd
     (tempo-save-named 'is_rec (if (y-or-n-p "Use a record for state data?") "yes" "no")))
   
   (if (equal (tempo-lookup-named 'is_rec) "yes") 
       '(l "-record(" (p "Record name: " rec) ",{" p "})." n n)
     (tempo-save-named 'rec ""))
   
   ;; Prompt here use in several places below
   (p "Name of initial state [init_state]:" init_state t)

   (eqc-tempo-include tempo-template-eqc-eqc_fsm-example_state)
   n
   (eqc-tempo-include tempo-template-eqc-eqc_fsm-initial_state)
   n
   (eqc-tempo-include tempo-template-eqc-eqc_fsm-initial_state_data)
   n
   (eqc-tempo-include tempo-template-eqc-eqc_fsm-next_state_data)
   n
   (eqc-tempo-include tempo-template-eqc-eqc_fsm-precond)
   n
   (eqc-tempo-include tempo-template-eqc-eqc_fsm-postcond)
   n
   (tempo-save-named 'ask_for_module "no")
   (eqc-tempo-include tempo-template-eqc-eqc_fsm-property)
   n
   (eqc-tempo-include tempo-template-eqc-eqc_fsm-weight)
   )
 "eqc_fsm"
 )

;; eqc_fsm callback function defaults
;; TODO Use proper EDOC comments...
(tempo-define-template "eqc-eqc_fsm-example_state"
                       '(& "%% Definition of the states. Each state is represented by a function, " > n
                           "%% listing the transitions from that state, together with generators " > n
                           "%% for the calls to make each transition." > n
                           (default-insert 'init_state "init_state") "(S) ->" > n
                           "[ %% {target_state,{call,?MODULE,target_function,[]}}" > n
                           "]." > n)
                       "eqc_fsm-example_state")

(tempo-define-template "eqc-eqc_fsm-initial_state"
                       '(& "%% Identify the initial state" > n
                           "initial_state() ->" > n
                           > (p "Name of initial state [init_state]:" init_state t)
                           (default-insert 'init_state "init_state")
                           "." n)       
                       "eqc_fsm-initial_state")

(tempo-define-template "eqc-eqc_fsm-initial_state_data"
                       '(& "%% Initialize the state data" > n
                           "initial_state_data() ->" > n
                           > (if (equal (tempo-lookup-named 'rec) "") "[]" '(l "#" (s rec) "{}"))
                           "." n)       
                       "eqc_fsm-initial_state_data")

(tempo-define-template "eqc-eqc_fsm-next_state_data"
                       '(& "%% Next state transformation for state data." > n
                           "%% S is the current state, From and To are state names" > n
                           "next_state_data(_From,_To,S,_V,{call,_,_,_}) ->" > n
                           > "S." p n)
                       "eqc_fsm-next_state_data")

(tempo-define-template "eqc-eqc_fsm-precond"
                       '(& "%% Precondition (for state data)." > n
                           "%% Precondition is checked before command is added to the command sequence" > n
                           "precondition(_From,_To,_S,{call,_,_,_}) ->" > n
                           > "true." p n)
                       "eqc_fsm-precondition")

(tempo-define-template "eqc-eqc_fsm-postcond"
                       '(& "%% Postcondition, checked after command has "
                           "been evaluated"> n
                           "%% OBS: S is the state before "
                           "next_state_data(From,To,S,_,<command>) " > n
                           "postcondition(_From,_To,_S,{call,_,_,_},_Res) ->" > n
                           > "true." p n)
                       "eqc_fsm-postcondition")

(tempo-define-template "eqc-eqc_fsm-weight"
                       '(& "%% Weight for transition (this callback is optional)." > n
                           "%% Specify how often each transition should be chosen" > n
                           "weight(_From,_To,{call,_,_,_}) ->" > n
                           > "1." p n)
                       "eqc_fsm-weight")


;;
;; Functions that manipulate the code i useful(?) ways
;;
(defun duplicate-function-clause ()
  "Using erlang-mark-clause and some copy, move cursor, paste, to
   create a copy of the current function clause. Useful when adding
   another pre/post/next_state..."
  (interactive)
  (erlang-mark-clause)
  (let (pos1 pos2 clause)
    (setq pos1 (region-beginning) pos2 (region-end))
    (setq clause (buffer-substring pos1 (- pos2 1)))
    (goto-char pos1)
    (insert clause)
    (insert ";\n")
    ))


(defun duplicate-function-clause-header ()
  "Using erlang-generate-new-clause to copy the function clause header"
  (interactive)
  ;; Since the erlang-beginning-of-clause, and thereby also 
  ;; erlang-generate-new-clause, is broken (see comment in erlang.el
  ;; by andersl) this little trick makes the result less surprising...
  (if (and (bolp) (not (eolp)))
      (forward-char 1)
    nil)
  (let (tmp_var)
    (setq tmp_var erlang-new-clause-with-arguments)
    (setq erlang-new-clause-with-arguments t)
    (erlang-generate-new-clause)
    (setq erlang-new-clause-with-arguments tmp_var)
    ))


;; Help functions 
(defun default-insert (var default)
  (if (equal (tempo-lookup-named var) "")
      default
    (tempo-lookup-named var))
  )

(defun is-region-active ()
  "Checks if there is a region selected"
  (interactive)
  (let 
      (pos1 pos2) 
    (if (and (is-transient-mark-mode) (is-mark-active))
        (setq pos1 (region-beginning) pos2 (region-end))
      (setq pos1 0 pos2 0))
    (not (equal pos1 pos2))))

;; Emacs vs. Xemacs stuff
(defun is-transient-mark-mode ()
  (if erlang-xemacs-p
      t
    transient-mark-mode
    ))

(defun is-mark-active ()
  (if erlang-xemacs-p
      (not (not (mark)))
    mark-active))

;; EQC man pages
;; TODO

