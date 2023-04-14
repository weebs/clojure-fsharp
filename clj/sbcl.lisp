(defun println (msg) (format t "~A~C" msg #\linefeed))
(println (format nil "~A" (+ 1 1)))