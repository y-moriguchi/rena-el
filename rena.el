; -*- lexical-binding: t -*-
(defun rena (&optional opt)
  (defun filter-if (filter lst)
    (cond ((null lst) '())
          ((funcall filter (car lst))
           (cons (car lst) (filter-if filter (cdr lst))))
          (t (filter-if filter (cdr lst)))))

  (setq keys (cdr (assq 'keys opt)))

  (defun ignore-opt ()
    (let ((ignore (assq 'ignore opt)))
      (if (and ignore (cdr ignore)) (renawrap (cdr ignore)) nil)))

  (defun indexignore (str index0)
    (let ((ignore (ignore-opt)))
      (cond (ignore
              (let ((matched (funcall ignore str index0 nil)))
                (cond (matched (index matched))
                      (t index0))))
            (t index0))))

  (defun noindexignore (str index0) index0)

  (defun index (matched) (cadr matched))
  (defun attr (matched) (caddr matched))

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

  (defun renathen (indexignore &rest args)
    (lambda (match lastindex attr)
      (let ((rest args)
            (matched (list "" lastindex attr)))
        (while (and matched (consp rest))
               (setcar (cdr matched)
                       (funcall indexignore match (index matched)))
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

  (defun one-or-more (exp &optional action)
    (times 1 nil exp action))

  (defun zero-or-more (exp &optional action)
    (times 0 nil exp action))

  (defun maybe (exp) (times 0 1 exp))

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
    (let ((ignkeys (mapcar (lambda (x) (lookahead x nil))
                           (filter-if (lambda (x)
                                        (< (length key)
                                           (length x)))
                                      keys))))
      (renathen #'indexignore
                (apply #'renathen (cons #'indexignore ignkeys))
                (renawrap key))))

  (defun rena-notkey ()
    (let ((ignkeys (mapcar (lambda (x) (lookahead x nil)) keys)))
      (apply #'renathen (cons #'indexignore ignkeys))))

  (defun equals-id (key)
    (cond ((and (not (ignore-opt)) (not keys))
           (renawrap key))
          ((and (ignore-opt) (not keys))
           (renathen #'noindexignore
                     (renawrap key)
                     (renaor (rena-end)
                             (lookahead (ignore-opt) t))))
          ((and (not (ignore-opt)) (not keys))
           (renathen #'noindexignore
                     (renawrap key)
                     (renaor (rena-end)
                             (lookahead (rena-notkey) nil))))
          (t (renathen #'noindexignore
                       (renawrap key)
                       (renaor (rena-end)
                               (lookahead (ignore-opt) t)
                               (lookahead (rena-notkey) nil))))))

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
    (let ((delays nil))
      (defun make-delays (args)
        (cond ((null args) '())
              (t
                (let ((memo nil))
                  (cons
                    (lambda (match index attr)
                      (progn
                        (cond ((null memo)
                               (setq memo
                                     (apply (car args) delays))))
                        (funcall memo match index attr)))
                    (make-delays (cdr args)))))))
      (setq delays (make-delays args))
      (car delays)))

  (lambda (&rest args)
    (let ((farg (car args)))
      (cond ((eq farg 'str) (renastr (cadr args)))
            ((eq farg 're) (renaregex (cadr args)))
            ((eq farg 'concat) (apply #'renathen
                                      (cons #'indexignore (cdr args))))
            ((eq farg 'choice) (apply #'renaor (cdr args)))
            ((eq farg 'one-or-more) (apply #'one-or-more (cdr args)))
            ((eq farg 'zero-or-more) (apply #'zero-or-more (cdr args)))
            ((eq farg 'opt) (apply #'maybe (cdr args)))
            ((eq farg 'lookahead) (lookahead (cadr args) t))
            ((eq farg 'lookahead-not) (lookahead (cadr args) nil))
            ((eq farg 'attr) (set-attr (cadr args)))
            ((eq farg 'cond) (renacond (cadr args)))
            ((eq farg 'key) (rena-key (cadr args)))
            ((eq farg 'not-key) (rena-notkey))
            ((eq farg 'equals-id) (equals-id (cadr args)))
            ((eq farg 'is-end) (rena-end))
            ((eq farg 'real) (real))
            ((eq farg 'y) (apply #'y-letrec (cdr args)))
            (t (apply #'action args))))))

