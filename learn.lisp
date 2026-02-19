(setq machine-status `on)
(print machine-status)

(setq symptoms `(fever cough headache))
(print symptoms)

(defun hassymptom(s)
    (member s symptoms))

(if (hassymptom 'fever)
    (print "has fever")
    (print "does not have fever"))  



(if (and (hassymptom 'fever) (hassymptom 'cough))
    (print 'FLU))
