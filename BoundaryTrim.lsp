 ;| Function to trim objects inside selected boundaries (allows for multiple boundaries)
   Boundaries can be "Circle, Ellipse, LWPolyline and Polyline" Entities
   Written By: Peter Jamtgaard Copyright 2015 All Rights Reserved
   ^C^C^P(or C:BoundaryTrim (load "BoundaryTrim.lsp"));BoundaryTrim
   EraseOutsideBoundary added by Tom Beauford
   ^C^C^P(or C:EraseOutsideBoundary (load "BoundaryTrim.lsp"));EraseOutsideBoundary
==============================================================================|;

(defun c:RectangleTrim (/ *error* main doc rec)
	(vl-load-com)
	(defun main ()
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
		
		(command-s "._rectangle")
		(setq rec (entlast))
		(BoundaryTrim rec)
		(entdel rec)

		(vla-EndUndoMark doc)
		(princ)
	)
	(defun *error*(s)
		(princ s)
		(vla-EndUndoMark doc)
		(princ)
	)
	(main)
)


;(defun C:BT ()(c:BoundaryTrim))
(defun C:BoundaryTrim (/ acDoc intCount ssBoundaries)
   (if (setq ssBoundaries    (ssget (list (cons 0 "Circle,Ellipse,LWPolyline,Polyline"))))
     (progn
  (vla-startundomark (setq acDoc (vla-get-activedocument (vlax-get-acad-object))))
  (repeat (setq intCount (sslength ssBoundaries))
   (setq intCount     (1- intCount))
   (BoundaryTrim        (ssname ssBoundaries intCount))
   (BoundaryWindowErase (ssname ssBoundaries intCount)); <-Erase objects inside boundary optional
  )
     )
   )
 (if	acDoc (vla-endundomark acDoc))
)

; Command line function to select objects that are windowed by a selected circle.
; (defun C:BoundarySelect (/ lstPoints objBoundary ssBoundary)
;  (if (and
; (setq ssBoundary  (ssget ":E:S" (list (cons 0 "Circle,Ellipse,LWPolyline,Polyline"))))
; (setq objBoundary (vlax-ename->vla-object (ssname ssBoundary 0)))
; (setq lstPoints   (SegmentPoints objBoundary 360))
;       )
;    (and
;     (setq ssSelections (ssget "_WP" lstPoints))
;    )
;  )
; )

; Function to trim linework inside a boundary entity
(defun BoundaryTrim (entBoundary1 / lstPoints entBoundary1 entBoundary2 lstCenter
                    lstPoints1 lstPoints2 objBoundary1 objBoundary2 ssBoundary *Error*)
 (defun *Error* ()
   (setvar "cmdecho" intCMDEcho)
 )
 (setq intCMDEcho (getvar "cmdecho"))
 (setvar "cmdecho" 0)
 (if (and
(setq objBoundary1  (vlax-ename->vla-object entBoundary1))
(setq lstPoints1    (SegmentPoints objBoundary1 360))
(setq lstCenter     (mapcar '(lambda (X)(/ (apply '+ X) (length lstPoints1)))(transposematrix lstPoints1)))
(vl-cmdf "offset"   (/ (distance (car lstPoints1) lstCenter) 36.0) entBoundary1 lstCenter "")
(setq entBoundary2  (entlast))
(setq objBoundary2  (vlax-ename->vla-object entBoundary2))
(setq lstPoints2   (SegmentPoints objBoundary2 360))
      )
   (progn
    (vl-cmdf "trim" entBoundary1 "" "f")
    (foreach lstPoint lstPoints2 (vl-cmdf lstPoint))
    (vl-cmdf "" "")
    (entdel entBoundary2)
    (vl-cmdf "redraw")
    (setvar "cmdecho" intCMDEcho)
   )
 )
)

; Function to trim linework outside a boundary entity
(defun TrimOutsideBoundary (entBoundary1 / lstPoints entBoundary1 entBoundary2 lstCenter
                    maxpt lstPoints1 lstPoints2 objBoundary1 objBoundary2 ssBoundary *Error*)
 (defun *Error* ()
   (setvar "cmdecho" intCMDEcho)
 )
 (setq intCMDEcho (getvar "cmdecho"))
 (setvar "cmdecho" 0)
 (if (and
(setq objBoundary1  (vlax-ename->vla-object entBoundary1))
(setq lstPoints1    (SegmentPoints objBoundary1 360))
(setq lstCenter     (mapcar '(lambda (X)(/ (apply '+ X) (length lstPoints1)))(transposematrix lstPoints1)))
(setq maxpt (list (1+ (car (getvar 'extmax)))(1+ (cadr (getvar 'extmax)))(1+ (caddr (getvar 'extmax)))))
(vl-cmdf "offset"   (/ (distance (car lstPoints1) lstCenter) 200.0) entBoundary1 maxpt "")
(setq entBoundary2  (entlast))
(setq objBoundary2  (vlax-ename->vla-object entBoundary2))
(setq lstPoints2   (SegmentPoints objBoundary2 360))
      )
   (progn
    (vl-cmdf "trim" entBoundary1 "" "f")
    (foreach lstPoint lstPoints2 (vl-cmdf lstPoint))
    (vl-cmdf "" "")
    (entdel entBoundary2)
    (vl-cmdf "redraw")
    (setvar "cmdecho" intCMDEcho)
   )
 )
)

; Function to erase linework inside a boundary entity
(defun BoundaryWindowErase (entBoundary / lstPoints objBoundary ssSelections)
 (if (and
(setq objBoundary  (vlax-ename->vla-object entBoundary))
(setq lstPoints    (SegmentPoints objBoundary 360))
(setq ssSelections (ssget "_WP" lstPoints))
      )
   (and
    (setq ssSelections (ssget "_WP" lstPoints))
    (vl-cmdf "erase" ssSelections "")
   )
 )
)

; Function to determine the points along a curve dividing it intSegments number of times
(defun SegmentPoints (objCurve intSegments /  sngSegment intCount lstPoint lstPoints sngLength sngSegment)
 (if (and
(setq sngLength   (vlax-curve-getdistatparam objCurve (vlax-curve-getendparam objCurve)))
(setq sngSegment  (/ sngLength intSegments))
(setq intCount    0)
      )
   (progn
    (repeat (1+ intSegments)
     (setq lstPoint   (vlax-curve-getpointatdist objCurve (* intCount sngSegment)))
     (setq lstPoints  (cons lstPoint lstPoints))
     (setq intCount   (1+ intCount))
    )
    lstPoints
   )
 )
)

; Function to Transpose a matrix
(defun TransposeMatrix (lstMatrix)
 (if (car lstMatrix)
   (cons (mapcar 'car lstMatrix)
  (TransposeMatrix (mapcar 'cdr lstMatrix))
  )
 )
)

; Function to erase linework outside a boundary entity
(defun C:EraseOutsideBoundary ( / ss1 n ssBoundary objBoundary lstPoints ssSelections entSelection)
 (vla-startundomark (setq acDoc (vla-get-activedocument (vlax-get-acad-object))))
 (setq ss1 (ssget "_X" '((67 . 0)))  n   -1)
 (if (and
(setq ssBoundary  (ssget ":E:S" (list (cons 0 "Circle,Ellipse,LWPolyline,Polyline"))))
(setq entBoundary  (ssname ssBoundary 0))
(ssdel entBoundary ss1)
(TrimOutsideBoundary entBoundary)
(setq objBoundary (vlax-ename->vla-object entBoundary))
(setq lstPoints   (SegmentPoints objBoundary 360))
      )
   (and
    (setq ssSelections (ssget "_CP" lstPoints))
    (repeat (sslength ssSelections)
      (setq entSelection (ssname ssSelections (setq n (1+ n))))
      (if(ssmemb entSelection ssSelections)(ssdel entSelection ss1))
    )
    (command "erase" ss1 "")
   )
 )
 (if	acDoc (vla-endundomark acDoc))
)