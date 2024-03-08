;;;; Jessica Theofilia 894476

;;;; -*- Mode: Lisp -*-

;;;; ool.lisp --

;; def-class/3

(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))

(defun def-class (class-name parents &rest part)
  (cond ((not (name-check class-name)) 
         (error "~w is not a valid name" class-name))
        ((or (not (listp parents))
             (member nil (mapcar #'name-check parents))
             (member nil (mapcar #'is-class parents)))
         (error "Parents error"))
        ((member nil (mapcar #'part-check part))
         (error "Part error"))
        ((is-class class-name)
         (error "Class ~w already exists" class-name))
        ((add-class-spec class-name 
                         (append (list class-name)
                                 (list parents)
                                 (list (sorted-part 
                                        parents                   
                                        (part-override parents 
                                                       part)))))
         class-name)))

;; name-check/1
;; Controlla che name sia un simbolo, escluso '().

(defun name-check (name)
  (and (symbolp name)
       (not (eql name '())))) 

;; part-check/1
;; Controlla la struttura degli attributi e dei metodi.

(defun part-check (part)
  (or (null part)
      (and (eql (car part) 'fields) 
           (not (member nil (applist 'field-check (cdr part)))))
      (and (eql (car part) 'methods) 
           (not (member nil (applist 'method-check (cdr part)))))))

;; field-check/3
;; Controlla la struttura di un attributo.

(defun field-check (field-name &optional (value nil) (type t))
  (and (name-check field-name)
       (cond ((and (not (null value))
                   (not (eql type T)) 
                   (is-class type)
                   (ignore-errors (eval value))
                   (not (is-instance (ignore-errors (eval value))
                                     type)))
              (error "Instance is not a ~w class" type))
             ((and (not (null value))
                   (not (eql type T)) 
                   (is-class type)
                   (null (ignore-errors (eval value))))
              (error "~w doesn't exists" value))
             ((and (not (null value))
                   (eql type T)
                   (is-instance (ignore-errors (eval value)))))
             ((and (not (null value))
                   (not (eql type T)) 
                   (is-class type)
                   (ignore-errors (eval value))
                   (is-instance (ignore-errors (eval value))
                                type)))
             ((and (not (null value))
                   (not (eql type T))
                   (ignore-errors (typep value type))))
             ((and (not (null value))
                   (not (eql type T))
                   (not (ignore-errors (typep value type))))
              (error "Expected ~w but received ~w" 
                     type 
                     (type-of value)))
             ((and (not (null value))
                   (eql type T)
                   (listp (ignore-errors (eval value)))
                   (not (is-class (car (ignore-errors 
                                         (eval value))))))
              (error "~w is not a valid instance" value))
             ((and (null value)
                   (not (eql type T)))
              (error "Value can't be nil"))
             (T))))

;; method-check/3
;; Controlla la struttura di un metodo.

(defun method-check (name arglist form)
  (and (name-check name)
       (listp arglist)
       (not (null form))))

;; part-override/3
;; Scorre tutte le liste di attributi e li ricostruisce.

(defun part-override (parent part)
  (cond ((null part) part)
        ((eql (caar part) 'fields)
         (cons (cons 'fields 
                     (new-field-list parent (cdar part)))
               (part-override parent (cdr part))))
        ((cons (car part) 
               (part-override parent (cdr part))))))

;; new-field-list/2
;; Confronta ogni attributo con la classe padre e 
;; eventualmente ricostruisce l'attributo.

(defun new-field-list (parent field)
  (let* ((field-name (caar field))
         (field-value (cadar field))
         (field-type (caddar field))
         (parent-field (car (member field-name 
                                    (parent-field-part parent) 
                                    :test #'eql :key #'car)))
         (parent-value (cadr parent-field))
         (parent-type (caddr parent-field)))
    (cond ((null field) nil)

          ;; FIGLIO
          ;; X
          ((and (null field-value)
                (null field-type)
                (or (null parent-field)
                    (and (null parent-value)
                         (eql parent-type T))))
           (cons (list field-name NIL T)
                 (new-field-list parent (cdr field))))
          
          ;; FIGLIO VALORE
          ;; X
          ((or (and (null field-type)
                    (null parent-field))
               (and (null field-type)
                    (null parent-value)
                    (eql parent-type T))
               (and (null field-type)
                    (not (null field-value))
                    (eql parent-type T))) 
           (cons (list field-name field-value T)
                 (new-field-list parent (cdr field))))
          
          ;; FIGLIO VALORE TIPO
          ;; X
          ((or (and (not (null field-value))
                    (not (null field-type))
                    (or (null parent-field)
                        (and (null parent-value)
                             (eql parent-type T))
                        (and (not (null parent-value))
                             (eql parent-type T))))
               (and (null field-value)
                    (not (null field-type))
                    (null parent-field)))
           (cons (car field)
                 (new-field-list parent (cdr field))))
          
          ;; FIGLIO
          ;; PADRE VALORE
          ((and (null field-value)
                (null field-type)
                (not (null parent-value))
                (eql parent-type T))
           (cons (list field-name parent-value T)
                 (new-field-list parent (cdr field))))
          
          ;; FIGLIO 
          ;; PADRE VALORE TIPO.
          ((and (null field-value)
                (null field-type))
           (cons (list field-name parent-value parent-type)
                 (new-field-list parent (cdr field))))
          
          ; FIGLIO VALORE
          ; PADRE VALORE TIPO
          ; Controlla che il valore di field sia del tipo del
          ; padre.
          
          ; CLASSE
          ((and (not (eql parent-type T))
                (null field-type)
                (not (null field-value))
                (is-class parent-type)
                (is-instance (ignore-errors (eval field-value)) 
                             parent-type)) 
           (cons (list field-name field-value parent-type)
                 (new-field-list parent (cdr field)))) 
          
          ; VALORE
          ((and (not (eql parent-type T))
                (null field-type)
                (not (null field-value))                                      
                (typep field-value parent-type)) 
           (cons (list field-name field-value parent-type)
                 (new-field-list parent (cdr field)))) 
          
          ; FIGLIO VALORE TIPO
          ; PADRE VALORE TIPO
          ; Il tipo del figlio e' sottotipo del padre.
          
          ; CLASSE
          ((and (not (eql  parent-type T))
                (null field-type)
                (and (is-class parent-type)
                     (is-class field-type)
                     (not (null (member parent-type (parent-list 
                                                     field-type))))))
           (cons (car field) 
                 (new-field-list parent (cdr field))))
          
          ; VALORE
          ((and (not (eql parent-type T))
                (not (null field-type))
                (not (is-class parent-type))
                (not (is-class field-type))
                (subtypep field-type parent-type)) 
           (cons (car field) 
                 (new-field-list parent (cdr field))))
          
          ; Il tipo del figlio non e' sottotipo di quello
          ; del padre.
          ((and (not (eql parent-type T))
                (not (null field-type)))
           (error "~w is not a subtypep of ~w" 
                  field-type parent-type))
          
          ; Il valore di field non � del tipo del padre.
           ((error "~w is not a valid field" field-name)))))

;; sorted-part/2
;; Ricostruisce la lista degli attributi e metodi ereditando
;; dalle superclassi.

(defun sorted-part (parent part)
  (list (cons 'fields 
              (remove-duplicates 
               (append 
                (flatten (mapcar (lambda (x) 
                                   (remove 'fields x))
                                 (remove 'methods             
                                         part 
                                         :test #'eql 
                                         :key #'car)))
                (parent-field-part parent)) 
               :test #'eql 
               :key #'car 
               :from-end t))
        (cons 'methods
              (remove-duplicates 
               (append  
                (applist 'process-method 
                         (flatten (mapcar (lambda (x) 
                                            (remove 'methods x)) 
                                          (remove 'fields 
                                                  part 
                                                  :test #'eql 
                                                  :key #'car)))) 
                (parent-method-part parent))
               :test #'eql 
               :key #'car 
               :from-end t))))

(defun flatten (x)
  (cond ((or (null x)
             (null (car x))) nil)
        ((and (listp (car x)) 
              (listp (caar x)))
         (append (cons (caar x) (flatten (cdar x))) 
                 (flatten (cdr x))))
        ((append (list (car x)) 
                 (flatten (cdr x))))))
        

(defun process-method (method-name &rest method-spec)
  (cond ((and (null method-name) (null method-spec)))
        ((keywordp method-name)
         (setf (fdefinition (read-from-string (symbol-name 
                                               method-name)))
               (lambda (this &rest args)
                 (apply (coerce (field this (read-from-string 
                                             (symbol-name 
                                              method-name))) 
                                'function)
                        (append (list this) args))))
         (rewrite-method-code (read-from-string (symbol-name 
                                                 method-name)) 
                              method-spec)) 
        ((setf (fdefinition method-name)              
               (lambda (this &rest args)
                 (apply (coerce (field this method-name) 
                                'function)
                        (append (list this) args))))               
         (rewrite-method-code method-name method-spec))))

(defun rewrite-method-code (method-name method-spec)
 (list method-name (list 'lambda (append (list 'this) 
                                          (car method-spec))
                          (cadr method-spec))))

;; get-parent/1
;; Restituisce la lista delle superclassi.

(defun get-parent (name)
  (cadr (class-spec name)))

;; get-field-part/1
;; Restituisce la lista degli attributi della classe name.

(defun get-field-part (name)
  (remove 'fields 
          (car (remove 'methods 
                       (caddr (class-spec name)) 
                       :test #'eql 
                       :key #'car))))

;; get-field/2
;; Restituisce le informazioni di un attributo field-name.

(defun get-field (name field-name)
  (car (member field-name (get-field-part name)
               :test #'eql 
               :key #'car)))

;; get-method-part/1
;; Restituisce la lista dei metodi della classe name.

(defun get-method-part (name)
  (remove 'methods 
          (car (remove 'fields 
                       (caddr (class-spec name)) 
                       :test #'eql 
                       :key #'car))))

;; get-method/2
;; Restituisce il corpo del metodo method-name.

(defun get-method (name method-name)
  (cadar (member method-name (get-method-part name) 
                 :test #'eql 
                 :key #'car)))

;; parent-field-part/1
;; Restituisce tutti gli attributi della lista delle 
;; superclassi senza duplicati.

(defun parent-field-part (parent)
  (cond ((null parent) NIL)
        ((remove-duplicates 
          (append (get-field-part (car parent)) 
                  (parent-field-part (cdr parent)))   
          :test #'eql :key #'car :from-end t))))

;; parent-method-part/1
;; Restituisce tutti i metodi della lista delle superclassi 
;; senza duplicati.

(defun parent-method-part (parent)
  (cond ((null parent) NIL)
        ((remove-duplicates 
          (append (get-method-part (car parent)) 
                  (parent-method-part (cdr parent)))   
          :test #'eql :key #'car :from-end t))))

;; make/2

(defun make (class-name &rest field)
  (cond ((or (not (name-check class-name))
             (not (is-class class-name)))
         (error "~w is not a valid class" class-name))
        ((instance-field-check class-name field)
         (list class-name field))))

;; instance-field-check/2
;; Controlla se gli attributi dell'istanza esistono nella 
;; classe e sono del tipo corretto.

(defun instance-field-check (class-name field)
  (let* ((class-field (get-field class-name (car field)))
         (class-type (caddr class-field))
         (field-value (cadr field)))    
    (cond ((null field))
          ((null class-field) 
           (error "Field ~w does not exists in ~W" 
                  (car field) 
                  class-name))
          ((or (null class-type)
               (and (is-class class-type)
                    (is-instance field-value class-type))
               (typep field-value class-type))
           (instance-field-check class-name (cddr field)))
          ((error "Expected ~w but received ~w" 
                  class-type 
                  field-value)))))

;; is-class/1

(defun is-class (class-name)
  (and (name-check class-name)
       (not (null (class-spec class-name)))))

;; is-instance/2

(defun is-instance (value &optional (class-name t))
  (cond ((null value) nil)
        ((and (not (eql class-name t))
              (is-class class-name)
              (listp value)
              (member class-name (parent-list (car value)))
              (instance-field-check class-name 
                                    (cadr value))))
        ((and (eql class-name t)
              (listp value)
              (is-class (car value))
              (instance-field-check (car value) 
                                    (cadr value))))))

;; parent-list/1
;; Restituisce la lista di tutte le superclassi di class,
;; incluso s� stesso.

(defun parent-list (class)
  (cond ((null class) nil)
        ((symbolp class) (append (list class)
                                 (parent-list (get-parent class))))
        ((append (list (car class)) 
                 (append (get-parent (car class))
                         (parent-list (cdr class)))))))

;; field/2

(defun field (instance field-name)
  (let ((instance-field (member field-name (cadr instance)))
        (class-field (get-field (car instance) field-name))
        (class-method (get-method (car instance) field-name)))
    (cond ((null instance)
           (error "Instance is NIL"))
          ((or (null field-name)
               (not (name-check field-name)))
           (error "~w is not valid field name" field-name))
          ((not (is-instance instance))
           (error "Instance is not valid"))
          ((and (null instance-field)
                (null class-field)
                (null class-method))
           (error "Field ~w doesn't exists" field-name))
          ;; Field � nella classe
          ((and (null instance-field)
                (null class-method)
                (is-instance (ignore-errors
                               (eval (cadr class-field)))))
           (ignore-errors (eval (cadr class-field))))
          ((and (null instance-field)
                (null class-method))
           (cadr class-field))
          ((null class-method)
           (cadr instance-field))
          (class-method))))

;; field*/2

(defun field* (instance field-name)
  (cond ((and (eql (list-length field-name) 1)
              (is-instance instance))
         (field instance (car field-name)))
        ((field* (field instance (car field-name)) 
                 (cdr field-name)))))

;; applist/2
;; Applica funzione ad ogni sottolista della lista.

(defun applist (function list)
  (cond ((or (null list)
             (null (car list)))
         '())
        ((eql (list-length list) 1)  
         (list (apply function (car list))))
        ((append  (list (apply function (car list))) 
                  (applist function (cdr list))))))

;;;; end of file -- ool.lisp --
