(defun call-gc ()
  #+sbcl(sb-ext:gc :full t)
  #+clisp(gc) ;; for CLISP, this is too hard!
  #+ecl(gc)
)

(defun call-exit ()
  #+sbcl (sb-ext:exit)
  #+clisp (exit)
  #+ecl (si:quit))

;; ----------------- array ---------------------
(defmacro mem-array (size &optional origin offset)
  `(make-array ,size
               :element-type '(unsigned-byte 32)
               ,@(if origin
                     `(:displaced-to
                       ,origin
                       :displaced-index-offset ,offset))))

;; -------------code walker--------------------
(defun replace-refm (tree map-name array-name offset-name)
  "Offset: symbol"
  (if (consp tree)
      (if (equal 'refm (car tree))
          (if (= (length tree) 3)
              (if (equal map-name (nth 1 tree))
                  `(aref ,array-name (+ ,offset-name
                                        ,(nth 2 tree)))
                  nil)
              (error (format nil "refm: ~A" tree)))
          (mapcar (lambda (tree)
                    (replace-refm tree map-name
                                  array-name offset-name))
                  tree))
      tree))

(defun replace-set-offset (tree map-name offset-name)
  (if (consp tree)
      (if (equal 'set-offset (car tree))
          (if (= (length tree) 3)
              (if (equal map-name (nth 1 tree))
                  `(setf ,offset-name ,(nth 2 tree))
                  nil)
              (error (format nil "set-offset: ~A" tree)))
          (mapcar (lambda (tree)
                    (replace-set-offset tree map-name
                                        offset-name))
                  tree))
      tree))

(defmacro with-map-array (map array offset &rest body)
  (let ((offset-name (gensym)))
    `(let ((,offset-name ,offset))
       ,@(mapcar (lambda (tree)
                   (replace-set-offset
                    (replace-refm tree map array offset-name)
                    map offset-name))
                 body))))

;;----------------------------

(type-of (mem-array 10))

(defstruct arraymap
  (a * :type (simple-array (unsigned-byte 32) (*)))
  (offset 0 :type (unsigned-byte 32)))
;;(make-arraymap :a (mem-array 20) :offset 0)

;;-----------------------------

(defun map-array-displace ()
  (let ((a (mem-array (* 1024 1024 16))))
    (dotimes (i (/ (length a) 4))
      (let ((map (mem-array 4 a (* i 4))))
        (dotimes (i 4)
          (setf (aref map i) i))))
    (mem-array 20 a 562)))

(defun map-array-cons ()
  (let* ((a (mem-array (* 1024 1024 16)))
         (map (cons a 0)))
    (dotimes (i (/ (length a) 4))
      (setf (cdr map) (* i 4))
      (dotimes (i 4)
        (setf (aref (car map) (+ (cdr map) i)) i)))
    (mem-array 20 a 562)))

(defun map-array-struct-alloc ()
  (let* ((a (mem-array (* 1024 1024 16))))
    (dotimes (i (/ (length a) 4))
      (let ((map (make-arraymap :a a :offset (* i 4))))
        (declare (type arraymap map))
        (dotimes (i 4)
          (setf (aref (arraymap-a map) (+ (arraymap-offset map) i))
                i))))
    (mem-array 20 a 562)))

(defun map-array-struct-setf ()
  (let* ((a (mem-array (* 1024 1024 16)))
         (map (make-arraymap :a a :offset 0)))
    (dotimes (i (/ (length a) 4))
      (setf (arraymap-offset map) (* i 4))
      (dotimes (i 4)
        (setf (aref (arraymap-a map) (+ (arraymap-offset map) i))
              i)))
    (mem-array 20 a 562)))

(defun map-array-macro ()
  (let* ((a (mem-array (* 1024 1024 16))))
    (with-map-array map a 0
                    (dotimes (i (/ (length a) 4))
                      (set-offset map (* 4 i))
                      (dotimes (j 4)
                        (setf (refm map j) j))))
    (mem-array 20 a 562)))

(defun map-array-offset ()
  (let ((a (mem-array (* 1024 1024 16))))
    (dotimes (i (length a))
      (setf (aref a i) (mod i 4)))
    (mem-array 20 a 562)))

(call-gc)
(format t "~%~%(time (map-array-displace))~%")
(princ (time (map-array-displace)))

(call-gc)
(format t "~%~%(time (map-array-cons))~%")
(princ (time (map-array-cons)))

(call-gc)
(format t "~%~%(time (map-array-struct-alloc))~%")
(princ (time (map-array-struct-alloc)))

(call-gc)
(format t "~%~%(time (map-array-struct-setf))~%")
(princ (time (map-array-struct-setf)))

(call-gc)
(format t "~%~%(time (map-array-macro))~%")
(princ (time (map-array-macro)))

(call-gc)
(format t "~%~%(time (map-array-offset))~%")
(princ (time (map-array-offset)))

(if (not (find-package :swank))
    (call-exit))
