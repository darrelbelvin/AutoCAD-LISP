;; GrText Demo Program 1  -  Lee Mac
;; Prompts the user to type a message and displays
;; the entered text on screen beside the cursor
;;
;; Requires: GrText.lsp

(load "GrTextV1-1.lsp")

(defun c:demo1 ( / *error* col gr1 gr2 str vec )

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (setq str ""
          col 2
    )
    (princ "\nType your message...")
    (while
        (progn
            (setq gr1 (grread nil 15 0)
                  gr2 (cadr gr1)
                  gr1 (car  gr1)
            )
            (cond
                (   (= 5 gr1)
                    (redraw)
                    (if vec (LM:DisplayGrText gr2 vec col 15 -31))
                    t
                )
                (   (= 2 gr1)
                    (cond
                        (   (= 08 gr2)
                            (if (< 0 (strlen str))
                                (setq str (substr str 1 (1- (strlen str))))
                            )
                        )
                        (   (= 13 gr2)
                            (setq str (strcat str "\n"))
                        )
                        (   (<= 32 gr2 255)
                            (setq str (strcat str (chr gr2)))
                        )
                    )
                    (setq vec (LM:GrText str))
                )
            )
        )
    )
    (redraw) (princ)
)

;; GrText Demo Program 2  -  Lee Mac
;; Displays the coordinates of the cursor position
;; as the cursor is moved across the screen.
;;
;; Requires: GrText.lsp

(defun c:demo2 ( / *error* pnt str )

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (while (= 5 (car (setq pnt (grread nil 13 0))))
        (redraw)
        (setq str (mapcar 'rtos (trans (cadr pnt) 1 0)))
        (LM:DisplayGrText (cadr pnt) (LM:GrText (strcat "X=" (car str) "\nY=" (cadr str))) 3 15 -31)
    )
    (redraw) (princ)
)

;; GrText Demo Program 3  -  Lee Mac
;; Prompts the user for a base point and displays
;; the area of the rectangular region enclosed by the
;; base point and cursor as the cursor is moved across the screen.
;;
;; Requires: GrText.lsp

(defun c:demo3 ( / *error* dis pt1 pt2 pt3 suf )

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (if (setq pt1 (getpoint "\nSpecify Base Point: "))
        (progn
            (setq pt1 (reverse (cdr (reverse pt1)))
                  pt2 (trans pt1 1 2)
                  suf (strcat (if (< (getvar 'lunits) 3) "mm" "in") (chr 178))
            )
            (while (= 5 (car (setq pt3 (grread nil 13 0))))
                (redraw)
                (setq pt3 (cadr pt3)
                      dis (mapcar '- pt3 pt1)
                      pt3 (trans pt3 1 2)
                )
                ;(LM:DisplayGrText pt3 (LM:GrText (strcat (rtos (abs (apply '* dis)) 2 4) suf)) 3 15 -31)
                (LM:DisplayGrTextRot pt0 (LM:GrText (strcat (rtos (abs (apply '* dis)) 2 4) suf)) 3 0 0 (/ pi 2))
                (grvecs
                    (list (if (< (car dis) 0.0) -256 256)
                        pt2 (list (car pt3) (cadr pt2))
                        pt3 (list (car pt3) (cadr pt2))
                        pt2 (list (car pt2) (cadr pt3))
                        pt3 (list (car pt2) (cadr pt3))
                    )
                )
            )
        )
    )
    (redraw) (princ)
)

(defun c:demo4 ( / *error* dis pt1 pt2 pt3 suf )

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (if (and
            (setq T_Entity (car (entsel "\nSelect polyline: ")))
            (= (vla-get-ObjectName (setq T_Object (vlax-ename->vla-object T_Entity))) "AcDbPolyline")
        )
        (progn
            (setq   T_Start (vlax-curve-getStartParam T_Object)
                    T_End   (vlax-curve-getEndParam T_Object)
                    pt1 (vlax-curve-getPointAtParam T_Object T_Start)
                    pt2 (trans pt1 2 1)
                    pt3 (vlax-curve-getPointAtParam T_Object (+ 1 T_Start))
                    pt4 (trans pt3 2 1)
                    dis (mapcar '- pt3 pt1)
                    rot (atan (cadr dis) (car dis))
                    
                    suf (strcat (if (< (getvar 'lunits) 3) "mm" "in") (chr 178))
            )
            ;(print dis)
            (print rot)
            (while (= 5 (car (grread nil 13 1)))
                (redraw)
                (LM:DisplayGrTextRot pt2 (LM:GrText (strcat (rtos (vlax-curve-getDistAtParam T_Object (+ 1 T_Start)) 2 4) suf)) 3 0 0 rot);15 -31)
                (grvecs
                    (list (if (< (car dis) 0.0) -256 256)
                        pt2 pt4
                        ; pt2 (list (car pt4) (cadr pt2))
                        ; pt4 (list (car pt4) (cadr pt2))
                        ; pt2 (list (car pt2) (cadr pt4))
                        ; pt4 (list (car pt2) (cadr pt4))
                    )
                )
            )
        )
    )
    (redraw) (princ)
)

(defun c:demo5 ( / *error* dis rot pt1 pt2 pt3 suf T_Segments T_Start T_End)

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (if (and
            (setq T_Entity (car (entsel "\nSelect polyline: ")))
            (= (vla-get-ObjectName (setq T_Object (vlax-ename->vla-object T_Entity))) "AcDbPolyline")
        )
        (progn
            (setq   T_Start (vlax-curve-getStartParam T_Object)
                    T_End   (vlax-curve-getEndParam T_Object)
                    suf (if (< (getvar 'lunits) 3) "mm" "in")
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
                        (- (vlax-curve-getDistAtParam T_Object T_Start) (vlax-curve-getDistAtParam T_Object (1- T_Start)))
                        (trans pt3 2 1)
                        rot
                ))))
            )
            (print T_Segments)
            (while (= 5 (car (grread nil 13 1)))
                (redraw)
                (foreach segment T_Segments
                    (LM:DisplayGrTextRot (cadr segment) (LM:GrText (strcat (rtos (car segment) 2 4) suf)) 3 0 0 (caddr segment));15 -31)
                )
            )
        )
    )
    (redraw) (princ)
)

(defun c:demo6 ( / *error* dis rot pt1 pt2 pt3 suf T_Segments T_Start T_End ss i ent)

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
                            (distance pt1 pt2);(- (vlax-curve-getDistAtParam T_Object T_Start) (vlax-curve-getDistAtParam T_Object (1- T_Start)))
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

(defun c:Test (/ T_Entity T_Object T_Start T_End T_SegmentLengths T_Count)
   (if
      (and
         (setq T_Entity (car (entsel "\nSelect polyline: ")))
         (= (vla-get-ObjectName (setq T_Object (vlax-ename->vla-object T_Entity))) "AcDbPolyline")
      )
      (progn
         (setq T_Start (vlax-curve-getStartParam T_Object))
         (setq T_End   (vlax-curve-getEndParam T_Object))
         (while (< T_Start T_End)
            (setq T_SegmentLengths (append T_SegmentLengths (list (- (vlax-curve-getDistAtParam T_Object (setq T_Start (1+ T_Start))) (vlax-curve-getDistAtParam T_Object (1- T_Start))))))
         )
         (setq T_Count 0)
         (foreach T_Item T_SegmentLengths
            (princ (strcat "\nSegment " (itoa (setq T_Count (1+ T_Count))) ": " (rtos T_Item)))
         )         
         (princ (strcat "\n\n ** Total polyline length is " (rtos (vla-get-Length T_Object))))
      )
      (princ "\n ** Nothing selected or not a polyline.")
   )
   (princ)
)

;; Display GrText  -  Lee Mac
;; pnt  -  cursor point in UCS
;; vec  -  GrText vector list
;; col  -  Text Colour (ACI Colour)
;; xof  -  x-offset from cursor in pixels
;; yof  -  y-offset from cursor in pixels

(defun LM:DisplayGrText ( pnt vec col xof yof / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
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

(princ)






;;------------------=={ Scale by Matrix }==-------------------;;
;;                                                            ;;
;;  Scales a VLA-Object or Point List using a                 ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1     - Base Point for Scaling Transformation            ;;
;;  scale  - Scale Factor by which to scale object            ;;
;;------------------------------------------------------------;;

(defun LM:ScaleByMatrix ( target p1 scale / m )

  (LM:ApplyMatrixTransformation target
    (setq m
      (list
        (list scale 0. 0.)
        (list 0. scale 0.)
        (list 0. 0. scale)
      )
    )
    (mapcar '- p1 (mxv m p1))
  )
)

;;----------------=={ Translate by Matrix }==-----------------;;
;;                                                            ;;
;;  Translates a VLA-Object or Point List using a             ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1, p2 - Points representing vector by which to translate ;;
;;------------------------------------------------------------;;

(defun LM:TranslateByMatrix ( target p1 p2 )

  (LM:ApplyMatrixTransformation target
    (list
      (list 1. 0. 0.)
      (list 0. 1. 0.)
      (list 0. 0. 1.)
    )
    (mapcar '- p2 p1)
  )
)

;;------------------=={ Rotate by Matrix }==------------------;;
;;                                                            ;;
;;  Rotates a VLA-Object or Point List using a                ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1     - Base Point for Rotation Transformation           ;;
;;  ang    - Angle through which to rotate object             ;;
;;------------------------------------------------------------;;

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

;;-----------------=={ Reflect by Matrix }==------------------;;
;;                                                            ;;
;;  Reflects a VLA-Object or Point List using a               ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to transform            ;;
;;  p1, p2 - Points representing vector in which to reflect   ;;
;;------------------------------------------------------------;;

(defun LM:ReflectByMatrix ( target p1 p2 )
  (
    (lambda ( a / m )
      (LM:ApplyMatrixTransformation target
        (setq m
          (list
            (list (cos a)    (sin a)  0.)
            (list (sin a) (- (cos a)) 0.)
            (list    0.         0.    1.)
          )
        )
        (mapcar '- p1 (mxv m p1))
      )
    )
    (* 2. (angle p1 p2))
  )
)

;;-----------=={ Apply Matrix Transformation }==--------------;;
;;                                                            ;;
;;  Transforms a VLA-Object or Point List using a             ;;
;;  Transformation Matrix                                     ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  target - VLA-Object or Point List to Transform            ;;
;;  matrix - 3x3 Matrix by which to Transform object          ;;
;;  vector - 3D translation vector                            ;;
;;------------------------------------------------------------;;

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

;; Matrix x Vector - Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
  (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)