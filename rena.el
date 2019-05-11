; -*- lexical-binding: t -*-
(defun rena (&optional opt)
  (defun rena-split-string (str)
    (mapcar (lambda (str1) (make-string 1 str1))
            (string-to-list str)))
  (defun make-trie-step (keys)
    (let ((result '()))
      (dolist (key keys)
        (let ((trieptr result) (triebase result) (oldch nil))
          (dolist (ch (rena-split-string key))
            (let ((ptrnew (assoc ch trieptr)))
              (cond (ptrnew
                     (setq triebase trieptr)
                     (setq trieptr (cdr ptrnew)))
                    ((eq trieptr triebase)
                     (let ((ptradd (cons (cons ch (cons nil nil))
                                         result)))
                       (setq result ptradd)
                       (setq triebase ptradd)
                       (setq trieptr (cdar ptradd))))
                    (t
                     (let ((ptradd (cons (cons ch (cons nil nil))
                                         (cdr trieptr))))
                       (setcdr (cdr (assoc oldch triebase)) ptradd)
                       (setq triebase ptradd)
                       (setq trieptr (cdar ptradd))))))
            (setq oldch ch))
          (setcar trieptr t)))
      result))
  (defun ignore-opt ()
    (let ((ignore (assq 'ignore opt)))
      (if (and ignore (cdr ignore)) ignore nil)))
  (defun indexignore (str index0)
    (let ((ignore (ignore-opt)))
      (cond (ignore
             (let ((matched (funcall (cdr ignore) str index0 nil)))
               (cond (matched (index matched))
                     (t index0))))
            (t index0))))
  (defun make-trie ()
    (let ((keys (assq 'keys opt)))
      (cond ((and keys (cdr keys)) (make-trie-step (cdr keys)))
            (t nil))))
  (defun index (matched) (cadr matched))
  (defun attr (matched) (caddr matched))
  (let ((trie (make-trie)))
    (defun get-trie (match lastindex)
      (let ((i lastindex) (result "") (trieptr trie))
        (while trieptr
          (cond ((>= i (length match)) (setq trieptr nil))
                (t (let ((ch (substring match i (+ i 1))))
                     (setq trieptr (cdr (assoc ch trieptr)))
                     (if trieptr (setq result (concat result ch)))
                     (setq i (+ i 1))))))
        result))
    (defun renastr (str)
      (lambda (match lastindex attr)
        (let ((strlen (+ lastindex (length str))))
          (cond ((> strlen (length match)) nil)
                ((string= str (substring match lastindex strlen))
                 (list str strlen attr))
                (t nil)))))
    (defun renaregex (pattern)
      (lambda (match lastindex attr)
        (cond ((string-match pattern match lastindex)
               (list (match-string 0 match) (match-end 0) attr))
              (t nil))))
    (defun renawrap (wrapee)
      (cond ((stringp wrapee) (renastr wrapee))
            (t wrapee)))
    (defun renathen (&rest args)
      (lambda (match lastindex attr)
        (let ((rest args)
              (matched (list "" lastindex attr)))
          (while (and matched (consp rest))
            (setcar (cdr matched) (indexignore match (index matched)))
            (setq matched (funcall (renawrap (car rest))
                                   match
                                   (index matched)
                                   (attr matched)))
            (setq rest (cdr rest)))
          (if matched
              (list (substring match lastindex (index matched))
                    (index matched)
                    (attr matched))
            nil))))
    (defun renaor (&rest args)
      (lambda (match lastindex attr)
        (let ((rest args)
              (matched nil))
          (while (and (not matched) (consp rest))
            (setq matched
                  (funcall (renawrap (car rest)) match lastindex attr))
            (setq rest (cdr rest)))
          matched)))
    (defun times (mincount maxcount exp &optional action)
      (let ((wrapped (renawrap exp))
            (actionw (if action action (lambda (match syn inh) syn))))
        (lambda (match lastindex attr)
          (let ((count 0)
                (matched (list "" lastindex attr))
                (break t))
            (while (and break
                        (or (not maxcount) (< count maxcount))
                        matched)
              (setcar (cdr matched) (indexignore match (index matched)))
              (let ((matchnew (funcall wrapped
                                       match
                                       (index matched)
                                       (attr matched))))
                (cond (matchnew
                       (setq matched
                             (list (car matchnew)
                                   (index matchnew)
                                   (funcall actionw
                                            (car matchnew)
                                            (attr matchnew)
                                            (attr matched))))
                       (setq count (+ count 1)))
                      (t (cond ((< count mincount) (setq matched nil))
                               (t (setq break nil)))))))
            (if matched
                (list (substring match lastindex (index matched))
                      (index matched)
                      (attr matched))
              nil)))))
    (defun at-least (mincount exp &optional action)
      (times mincount nil exp action))
    (defun at-most (maxcount exp &optional action)
      (times 0 maxcount exp action))
    (defun one-or-more (exp &optional action)
      (times 1 nil exp action))
    (defun zero-or-more (exp &optional action)
      (times 0 nil exp action))
    (defun maybe (exp) (times 0 1 exp))
    (defun delimit (exp delimiter &optional action)
      (let ((wrapped (renawrap exp))
            (wrapped-delimiter (renawrap delimiter))
            (actionw (if action action (lambda (match syn inh) syn))))
        (lambda (match lastindex attr)
          (let ((matched nil)
                (matched-loop (list "" lastindex attr))
                (break t))
            (while (and break matched-loop)
              (setcar (cdr matched-loop)
                      (indexignore match (index matched-loop)))
              (let ((matchnew (funcall wrapped
                                       match
                                       (index matched-loop)
                                       (attr matched-loop))))
                (cond (matchnew
                       (setq matched
                             (list (car matchnew)
                                   (index matchnew)
                                   (funcall actionw
                                            (car matchnew)
                                            (attr matchnew)
                                            (attr matched-loop))))
                       (setq matchnew
                             (list (car matched)
                                   (indexignore match (index matched))
                                   (attr matched)))
                       (setq matched-loop
                             (funcall wrapped-delimiter
                                      match
                                      (index matchnew)
                                      (attr matchnew))))
                      (t (setq break nil)))))
            (if matched
                (list (substring match lastindex (index matched))
                      (index matched)
                      (attr matched))
              nil)))))
    (defun lookahead (exp signum)
      (let ((wrapped (renawrap exp)))
        (lambda (match lastindex attr)
          (let ((matched (funcall wrapped match lastindex attr)))
            (cond ((or (and signum matched)
                       (and (not signum) (not matched)))
                   (list "" lastindex attr))
                  (t nil))))))
    (defun set-attr (attr)
      (lambda (match lastindex attr-old)
        (list "" lastindex attr)))
    (defun renacond (pred)
      (lambda (match lastindex attr)
        (cond ((funcall pred attr) (list "" lastindex attr))
              (t nil))))
    (defun rena-key (key)
      (lambda (match lastindex attr)
        (let ((matchedkey (get-trie match lastindex)))
          (cond ((string= matchedkey key)
                 (list matchedkey
                       (+ lastindex (length matchedkey))
                       attr))
                (t nil)))))
    (defun rena-notkey ()
      (lambda (match lastindex attr)
        (let ((matchedkey (get-trie match lastindex)))
          (cond ((string= matchedkey "") (list "" lastindex attr))
                (t nil)))))
    (defun equals-id (key)
      (let ((wrapped (renawrap key)))
        (lambda (match lastindex attr)
          (let ((matched (funcall wrapped match lastindex attr)))
            (cond ((not matched) nil)
                  ((>= (index matched) (length match)) matched)
                  ((and (not (ignore-opt)) (not trie)) matched)
                  ((and (ignore-opt)
                        (> (indexignore match (index matched))
                           (index matched)))
                   matched)
                  ((and trie
                        (not (string= (get-trie match (index matched))
                                      "")))
                   matched)
                  (t nil))))))
    (defun action (exp &optional action)
      (let ((wrapped (renawrap exp))
            (wrapped-action
             (if action action (lambda (match syn inh) syn))))
        (lambda (match lastindex attr)
          (let ((matched (funcall wrapped match lastindex attr)))
            (cond (matched (list (car matched)
                                 (index matched)
                                 (funcall wrapped-action
                                          (car matched)
                                          (attr matched)
                                          attr)))
                  (t nil))))))
    (defun rena-end ()
      (lambda (match lastindex attr)
        (cond ((>= lastindex (length match)) (list "" lastindex attr))
              (t nil))))
    (defun real ()
      (let ((re (concat "[-+]?"
                        "\\(?:[0-9]+\\(?:\\.[0-9]+\\)?\\|\\.[0-9]+\\)"
                        "\\(?:[eE][-+]?[0-9]+\\)?")))
        (lambda (match lastindex attr)
          (cond ((string-match re match lastindex)
                 (list (match-string 0 match)
                       (match-end 0)
                       (string-to-number (match-string 0 match))))
                (t nil)))))
    (defun y-letrec (&rest args)
      (letrec
          ((f (lambda (g) (funcall g g)))
           (h (lambda (p)
                (let ((result '()))
                  (dolist (arg args)
                    (setq result
                          (cons (lambda (match index attr)
                                  (funcall (apply arg (funcall p p))
                                           match
                                           index
                                           attr))
                                result)))
                  (reverse result)))))
        (car (funcall f h))))
    (lambda (&rest args)
      (let ((farg (car args)))
        (cond ((eq farg 'str) (renastr (cadr args)))
              ((eq farg 're) (renaregex (cadr args)))
              ((eq farg 'then) (apply #'renathen (cdr args)))
              ((eq farg 'or) (apply #'renaor (cdr args)))
              ((eq farg 'times) (apply #'times (cdr args)))
              ((eq farg 'at-least) (apply #'at-least (cdr args)))
              ((eq farg 'at-most) (apply #'at-most (cdr args)))
              ((eq farg 'one-or-more) (apply #'one-or-more (cdr args)))
              ((eq farg 'zero-or-more) (apply #'zero-or-more (cdr args)))
              ((eq farg 'maybe) (apply #'maybe (cdr args)))
              ((eq farg 'delimit) (apply #'delimit (cdr args)))
              ((eq farg 'lookahead) (lookahead (cadr args) t))
              ((eq farg 'lookahead-not) (lookahead (cadr args) nil))
              ((eq farg 'attr) (set-attr (cadr args)))
              ((eq farg 'cond) (renacond (cadr args)))
              ((eq farg 'key) (rena-key (cadr args)))
              ((eq farg 'not-key) (rena-notkey))
              ((eq farg 'equals-id) (equals-id (cadr args)))
              ((eq farg 'end) (rena-end))
              ((eq farg 'real) (real))
              ((eq farg 'br) (renaregex "\r\n\\|\r\\|\n"))
              ((eq farg 'y) (apply #'y-letrec (cdr args)))
              (t (apply #'action args)))))))
