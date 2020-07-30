
(defun md5-current-string (&rest delimiter-list-arg)
  "utilitary function used to find the string around the point"
  (let* ((delimiter-list (if (not delimiter-list-arg) (list ?\") delimiter-list-arg))
         (current-point (point)))
    (save-excursion
      (beginning-of-buffer)
      (cl-loop with str-start = nil
               with str-char = nil
               with str-end = nil
               with prev-escape? = nil
               for i from (point-min) to (point-max)
               do (let ((c (char-after i)))
                    (let ((is-str-char? (and (not prev-escape?)
                                             (memq c delimiter-list))))
                      (if is-str-char?
                          (if str-start
                              (if (eq c str-char)
                                  (if (and (>= current-point str-start)
                                           (<= current-point i))
                                      (setq str-end i)
                                    (progn (setq str-start nil)
                                           (setq str-char nil))))
                            (progn (setq str-start i)
                                   (setq str-char c)
                                   (message (concat "str starting at " (number-to-string i) " with char " (string c)))))))
                    (setq prev-escape? (and (eq c ?\\)
                                            (not prev-escape?))))
               if str-end return (buffer-substring-no-properties (+ str-start 1) str-end)))))

(defun md5-s (step-index)
  (let ((table (vector 7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
                       5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
                       4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
                       6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21)))
    (elt table step-index)))

(defun md5-t (step-index)
  (floor (* (expt 2 32) (abs (sin (+ step-index 1))))))

(defun md5-f (x y z)
  (logior (logand x y)
          (logand (lognot x) z)))

(defun md5-g (x y z)
  (logior (logand x z)
          (logand y (lognot z))))

(defun md5-h (x y z)
  (logxor x y z))

(defun md5-i (x y z)
  (logxor y (logior x (lognot z))))

(defun md5-fun (step-index x y z)
  (let ((round (/ step-index 16))
        (functions (vector 'md5-f 'md5-g 'md5-h 'md5-i)))
    (funcall (elt functions round) x y z)))

(defun md5-leftrotate (x c)
  (logior (logand #xFFFFFFFF (lsh x c)) (lsh x (- c 32))))

(defun md5-block-index (step-index)
  (let ((round (/ step-index 16))
        (index-funs (vector  (lambda (i) (mod i 16))
                             (lambda (i) (mod (+ (* i 5) 1) 16))
                             (lambda (i) (mod (+ (* i 3) 5) 16))
                             (lambda (i) (mod (* i 7) 16)))))
    (funcall (elt index-funs round) (mod step-index 16))))

(defun md5-add (x &rest ys)
  (logand #xFFFFFFFF (apply '+ x ys)))

(setq md5-show-debug nil)
(defmacro md5-debug (str)
  (if md5-show-debug
      `(print ,str)
    nil))

(defun md5-apply-step (a0 b0 c0 d0 message)
  (if (not (= (length message) 64))
      (error (format "invalid message length: %d, should be 64 (512 bits)" (length message))))
  (md5-debug (format "Processing chunk: %s" (mapcar (lambda (x) (format "%X" x)) message)))
  (md5-debug (format "A0: %X B0: %X C0: %X D0: %X" a0 b0 c0 d0))
  (let ((a a0)
        (b b0)
        (c c0)
        (d d0))
    (dotimes (i 64)
      (let* ((block-index   (md5-block-index i))
             (message-index (* block-index 4)) ; 4 bytes per message 32 bit blocks
             (m (md5-add (lsh (elt message message-index) 0)
                         (lsh (elt message (+ message-index 1)) 8)
                         (lsh (elt message (+ message-index 2)) 16)
                         (lsh (elt message (+ message-index 3)) 24)))
             (tt (md5-t i))
             (f (md5-add (md5-fun i b c d) a m tt))
             (s (md5-s i)))
        (setq a d)
        (setq d c)
        (setq c b)
        (setq b (md5-add b (md5-leftrotate f s)))
        ;(md5-debug (format "[%d] i: %d m_i: %X A: %X B: %X C: %X D: %X T: %X s: %d" i block-index m a b c d tt s))
        ))
    (vector (md5-add a a0) (md5-add b b0) (md5-add c c0) (md5-add d d0))))

(defun md5-pad (msg)
  (let* ((bytified-msg (string-to-unibyte msg))
         (bitcount (* (length bytified-msg) 8))  ; assuming single byte characters
         (bitcount-pad (list (logand bitcount #xFF)
                             (lsh (logand bitcount #xFF00) -8)
                             (lsh (logand bitcount #xFF0000) -16)
                             (lsh (logand bitcount #xFF000000) -24)
                             (lsh (logand bitcount #xFF00000000) -32)
                             (lsh (logand bitcount #xFF0000000000) -40)
                             (lsh (logand bitcount #xFF000000000000) -48)
                             #x00))
         (missing-bits (- 448 (% bitcount 512)))) ; +1 for extra bit
    (if (< missing-bits 0) (setq missing-bits (+ missing-bits 512)))
    (let ((pad-zeroes (make-list (/ missing-bits 8) #x00)))
      (setcar pad-zeroes #x80)
      (append bytified-msg pad-zeroes bitcount-pad))))

(defun md5-inv-endian (x)
  (format "%02x%02x%02x%02x"
          (logand x #xFF)
          (lsh (logand x #xFF00) -8)
          (lsh (logand x #xFF0000) -16)
          (lsh (logand x #xFF000000) -24)))

(defun md5 (str &optional print-message?)
  "calculate md5 sum of str parmeter"
  (interactive (list (let* ((default-str (md5-current-string))
                            (input-str (read-string (concat "md5 string(default: \"" default-str "\"):"))))
                       (if (string= input-str "") default-str input-str))
                     t))
  (let* ((a0 #x67452301)
         (b0 #xefcdab89)
         (c0 #x98badcfe)
         (d0 #x10325476)
         (padded-msg (md5-pad str))
         (itr-count (/ (length padded-msg) 64)))
    (md5-debug (format "message: %s" (mapcar (lambda (x) (format "%02X" x)) str)))
    (md5-debug (format "padded message: %s" (mapcar (lambda (x) (format "%02X" x)) padded-msg)))
    (dotimes (i itr-count)
      (let* ((chunk-512 (seq-subseq padded-msg (* i 64) (* (+ i 1) 64)))
             (result (md5-apply-step a0 b0 c0 d0 chunk-512)))
        (setq a0 (elt result 0))
        (setq b0 (elt result 1))
        (setq c0 (elt result 2))
        (setq d0 (elt result 3))
        (md5-debug (format "chunk %d: %x %x %x %x" i a0 b0 c0 d0))
        ))
    (let ((md5-hash (concat (md5-inv-endian a0) (md5-inv-endian b0) (md5-inv-endian c0) (md5-inv-endian d0))))
      (if print-message?
          (progn (kill-new md5-hash)
                 (message (concat md5-hash " is the md5 hash of \"" (if (< (length str) 128) str "<long-input>")
                                  "\" [copied to killring]"))))
      md5-hash)))

(defun md5-file (filepath)
  (interactive "f")
  (let ((message (with-temp-buffer
                   (insert-file-contents-literally filepath)
                   (buffer-string))))
    (md5 message t)))

(defmacro md5-test-cases (cases)
  `(cond ,@(mapcar (lambda (case)
                     (let ((input (car case))
                           (expected-result (cadr case)))
                       `((not (string= (md5 ,input) ,expected-result))
                         (format "md5-tests failed on input: %s" ,input))))
                   cases)
         (t "all tests PASSED")))

(defun md5-tests ()
  (interactive)
  (let* ((msg (md5-test-cases (("" "d41d8cd98f00b204e9800998ecf8427e")
                               ("abc" "900150983cd24fb0d6963f7d28e17f72")
                               ("The quick brown fox jumps over the lazy dog" "9e107d9d372bb6826bd81d3542a419d6")
                               ("12345678901234567890123456789012345678901234567890123456789012345678901234567890"
                                (downcase "57EDF4A22BE3C955AC49DA2E2107B67A"))))))
    (message msg)))

(provide 'md5)
