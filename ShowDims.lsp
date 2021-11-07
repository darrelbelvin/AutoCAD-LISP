(load "GrTextV1-1.lsp")
(load "msgbox.lsp")

(defun c:showdims () (command "measuregeom" "q"))
(defun c:showdimsold ( / *error* dis rot pt1 pt2 pt3 suf T_Segments T_Start T_End ss i ent)

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (if (not (setq ss (ssget "_I" '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "LINE") (-4 . "OR>")))))
        (setq ss (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "LINE") (-4 . "OR>"))))
    )
    (setq suf (if (< (getvar 'lunits) 3) "mm" "in"))
    (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i)))
            T_Object (vlax-ename->vla-object ent))
        (cond ((= (vla-get-ObjectName T_Object) "AcDbPolyline")
            (progn
                (setq   T_Start (vlax-curve-getStartParam T_Object)
                        T_End   (vlax-curve-getEndParam T_Object)
                )
                (while (< T_Start T_End)
                    (setq T_Start (1+ T_Start)
                        pt1 (vlax-curve-getPointAtParam T_Object (1- T_Start))
                        pt2 (vlax-curve-getPointAtParam T_Object T_Start)
                        dis (mapcar '- pt2 pt1)
                        rot (atan (cadr dis) (car dis))
                        rot (- (rem (+ (* pi 2.5) rot) pi) (* pi 2.5))
                        pt3 (mapcar '+ pt1 (mapcar '* '(0.5 0.5 0.5) dis))
                        T_Segments (append T_Segments (list (list
                            (distance pt1 pt2)
                            (trans pt3 2 1)
                            rot
                    ))))
                )
            ))
            ((= (vla-get-ObjectName T_Object) "AcDbLine")
            (setq
                ent (entget ent)
                pt1 (cdr (assoc 10 ent))
                pt2 (cdr (assoc 11 ent))
                dis (mapcar '- pt2 pt1)
                rot (atan (cadr dis) (car dis))
                rot (- (rem (+ (* pi 2.5) rot) pi) (* pi 2.5))
                pt3 (mapcar '+ pt1 (mapcar '* '(0.5 0.5 0.5) dis))
                T_Segments (append T_Segments (list (list
                    (distance pt1 pt2)
                    (trans pt3 2 1)
                    rot
                )))
            ))
        )
    )
    ;(print T_Segments)
    (while (= 5 (car (grread nil 13 1)))
        (redraw)
        (foreach segment T_Segments
            (LM:DisplayGrTextRot (cadr segment) (LM:GrText (strcat (rtos (car segment) 2 4) suf)) 3 0 0 (caddr segment));15 -31)
        )
    )
    (redraw) (princ)
)

(defun c:highlightoffaxis ( / *error* dis rot pt1 pt2 pt3 suf T_Segments T_Start T_End ss i ent)

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (if (not (setq ss (ssget "_I" '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "LINE") (-4 . "OR>")))))
        (setq ss (ssget '((-4 . "<OR") (0 . "LWPOLYLINE") (0 . "LINE") (-4 . "OR>"))))
    )
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i)))
          T_Object (vlax-ename->vla-object ent))
      (cond ((= (vla-get-ObjectName T_Object) "AcDbPolyline")
        (progn
            (setq   T_Start (vlax-curve-getStartParam T_Object)
                    T_End   (vlax-curve-getEndParam T_Object)
            )
            (while (< T_Start T_End)
                (setq T_Start (1+ T_Start)
                    pt1 (vlax-curve-getPointAtParam T_Object (1- T_Start))
                    pt2 (vlax-curve-getPointAtParam T_Object T_Start)
                    pt1 (3dPoint->2dPoint pt1)
                    pt2 (3dPoint->2dPoint pt2)
                    dis (mapcar '- pt2 pt1)
              )
              (if (not (or (= 0 (car dis)) (= 0 (cadr dis))))
                (setq T_Segments (append T_Segments (list pt1 pt2)))
              )
            )
          )
        )
        ((= (vla-get-ObjectName T_Object) "AcDbLine")
        (progn
          (setq
            ent (entget ent)
            pt1 (3dPoint->2dPoint (cdr (assoc 10 ent)))
            pt2 (3dPoint->2dPoint (cdr (assoc 11 ent)))
            dis (mapcar '- pt2 pt1)
          )
          (if (not (or (= 0 (car dis)) (= 0 (cadr dis))))
            (setq T_Segments (append T_Segments (list pt1 pt2)))
          )
        ))
      )
    )
    (if (= 0 (length T_Segments)) (lspOkOnly "All selected" "segments" "are on axis." "Result")
     (progn
      (sssetfirst nil nil)
      (while (= 5 (car (grread nil 13 1)))
        (redraw)
        (grvecs (append (list -10) T_Segments))
      )
     )
    )
    (redraw) (princ)
;    (grvecs (append (list -1) T_Segments))
)

(defun c:sd () (c:showdims))
(defun c:hoa () (c:highlightoffaxis))

(defun LM:DisplayGrTextRot ( pnt vec col xof yof ang / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
          vec (LM:RotateByMatrix vec '(0 0) ang)
    )
    (grvecs (cons col vec)
        (list
            (list scl 0.0 0.0 (+ (car  pnt) (* xof scl)))
            (list 0.0 scl 0.0 (+ (cadr pnt) (* yof scl)))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)

(defun LM:RotateByMatrix ( target p1 ang )
  
  (LM:ApplyMatrixTransformation target
    (setq m
      (list
        (list (cos ang) (- (sin ang)) 0.)
        (list (sin ang)    (cos ang)  0.)
        (list    0.           0.      1.)
      )
    )
    (mapcar '- p1 (mxv m p1))
  )
)

(defun LM:ApplyMatrixTransformation ( target matrix vector ) (vl-load-com)
  (cond
    ( (eq 'VLA-OBJECT (type target))
     
      (vla-TransformBy target
        (vlax-tMatrix
          (append (mapcar '(lambda ( x v ) (append x (list v))) matrix vector)
           '((0. 0. 0. 1.))
          )
        )
      )
    )
    ( (listp target)

      (mapcar
        (function
          (lambda ( point ) (mapcar '+ (mxv matrix point) vector))
        )
        target
      )
    )        
  )
)

(defun mxv ( m v )
  (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(defun 3dPoint->2dPoint (3dpt)(list (car 3dpt) (cadr 3dpt)))