
;; Joe Burke
;; Last revision 3/12/2006. Combined two fuctions into one.
;; Added operation with User Coordinate Systems.
;; Two possible methods using a UCS. Given a plan is orthogonal in terms of
;; WCS, but is not on grid. Move the origin (command usc o) to some
;; known good point. Second method, a plan is rotated in terms of WCS.
;; Align the UCS to an object (command ucs ob) typically a line.
;;
;; Example of use with a UCS. Assume a plan is drawn on grid in WCS. 
;; Then the plan is moved off grid. When the UCS origin is moved to a 
;; known good point, the program will report all objects on grid and nothing 
;; needs to be fixed.
;;
;; The rounding value determines the precision of rounding. For architectural
;; floor plans using Imperial units, 0.25 or 0.125 would be typical values.
;; For metric files using millimeters the typical value would be 1.
;;
;; The fuzz value determines the range of what's considered "off grid" 
;; expressed as an integer which represents number of decimal places.
;; 6 equals 1e-6. Lower values will find fewer off grid objects. 
;; Values above 8 will will identify off grid points/objects beyond the 
;; precision which can be displayed in Properties.
;;
;; Use this program with caution.
   
(defun c:FixOffGrid ( / *error* doc layers tempval ucsang origin ss  
                        idx cnt obj objtyp pt rad lst process lay
                        lockedlayers layname unlock majrad minrad
                        FixPoint FixAngle FixValue AddToList flag
                        CheckPoint CheckAngle CheckValue locklst sellst
                        dtr rtd round GetLockedLayers GetCoordinates 
                        StringRight)
                        ;*rndval* and *fuzz* are globals                     

  (defun *error* (msg)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*"))
      (T (princ (strcat "\nError: " msg)))
    )
    (sssetfirst)
    (if 
      (and
        lockedlayers
        (not (eq "N" unlock))
      )
      (foreach x lockedlayers
        (vla-put-lock (vla-item layers x) :vlax-true)
      )
    )
    (vla-EndUndoMark doc)
    (foreach x sellst (vla-highlight x :vlax-false))
    (foreach x locklst (vla-highlight x :vlax-false))
    (princ (strcat "\nNumber of objects modified: " (itoa cnt)))
    (princ)
  ) ;end error

  ;; start sub-functions ;;
  (defun CheckPoint (p)
    (while
      (and 
        p 
        (equal (car p) (round (car p) *rndval*) *fuzz*)
      )
      (setq p (cdr p))
    )
    p
  )
  (defun CheckAngle (a)
    (not (equal (rtd a) (round (rtd a) *rndval*) *fuzz*))
  )
  (defun CheckValue (v)
    (not (equal v (round v *rndval*) *fuzz*))
  ) 
  (defun FixPoint (p)
    (trans (mapcar '(lambda (x) (round x *rndval*)) p) 1 0)
  )
  (defun FixAngle (a)
    (dtr (round (rtd a) *rndval*))
  )
  (defun FixValue (v)
    (round v *rndval*)
  )
  ;; If no locked layers, add object to sellst.
  ;; If locked layers, determine if the object
  ;; is on one of them and if so, add it to locklst instead.
  (defun AddToList (o lay)
    (cond
      ((not lockedlayers)
        (setq sellst (cons o sellst))
      )
      (lockedlayers
        (if (vl-position lay lockedlayers)
          (setq locklst (cons o locklst))
          (setq sellst (cons o sellst))
        )
      )
    )
  ) ;end

  ;-------------------------------------------
  ;degrees to radians
  (defun dtr (degrees)
     (* pi (/ degrees 180.0))
  ) ;end
  ;-------------------------------------------
  ;radians to degrees
  (defun rtd (radians)
     (/ (* radians 180.0) pi)
  ) ;end
  ;-------------------------------------------
  ;; JB 2/23/03
  (defun round (value to)
    (if (zerop to) value
      (* (atoi (rtos (/ (float value) to) 2 0)) to)))
  ;-------------------------------------------
  (defun GetLockedLayers (doc / layers layname laylst)
    (setq layers (vla-get-Layers doc))
    (vlax-for x layers
      (setq layname (vlax-get x 'Name))
      ;filter out xref layers
      (if 
        (and 
          (not (vl-string-search "|" (vlax-get x 'Name)))
          (eq :vlax-true (vla-get-lock x))
        )
        (setq laylst (cons layname laylst))
      )
    )
    laylst
  ) ;end
  ;; -------------------------------------------
  (defun GetCoordinates (obj / objname coord lst)
    (if (= (type obj) 'ENAME)
      (setq obj (vlax-ename->vla-object obj)))
    (and
      (setq objname (vlax-get obj 'ObjectName))
      (vl-position objname '("AcDbPolyline" "AcDb2dPolyline" 
                             "AcDb3dPolyline" "AcDbLeader"))
      (setq coord (vlax-get obj 'Coordinates))
      (cond
        ((= objname "AcDbPolyline")
          (repeat (/ (length coord) 2)
            (setq lst (cons (list (car coord) (cadr coord)) lst))
            (setq coord (cddr coord))
          )
        )
        (T
          (repeat (/ (length coord) 3)
            (setq lst (cons (list (car coord) (cadr coord) (caddr coord)) lst))
            (setq coord (cdddr coord))
          )
        )
      )
    ) ;and
    (reverse lst)
  ) ;end

  (defun StringRight (delim str / pos)
    (if (setq pos (vl-string-search delim str))
      (progn
        (setq str (substr str (+ 1 pos (strlen delim))))
        (vl-string-left-trim delim str)
      )
      str
    )
  ) ;end

  ;; end sub-functions ;;
 
  ;; start primary function ;;
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        layers (vla-get-layers doc)
        lockedlayers (GetLockedLayers doc)
  )
  
  (vla-StartUndoMark doc)

  (setq idx 0 cnt 0)

  ;; feedback regarding WCS/UCS conditions
  (setq ucsang (angle '(0.0 0.0 0.0) (getvar "ucsxdir")))
  (setq origin (getvar "ucsorg"))

  (cond
    ((= 1 (getvar "worlducs"))
      (princ "\nWorld coordinate system is active. ")
    )
    ((and 
       (/= 0 ucsang)
       (equal '(0.0 0.0 0.0) origin)
      )
      (princ (strcat "\nUCS rotation is " (angtos ucsang) ", origin is world. "))
    )
    ((and 
       (= 0 ucsang)
       (not (equal '(0.0 0.0 0.0) origin))
      )
      (princ (strcat "\nUCS rotation is zero, origin is " (vl-princ-to-string origin)))
    )
    ((and 
       (/= 0 ucsang)
       (not (equal '(0.0 0.0 0.0) origin))
      )
      (princ (strcat "\nUCS rotation is " 
        (angtos ucsang) ", origin is " (vl-princ-to-string origin)))
    )
  )

  ;; set *rndval* and *fuzz*
  (if *rndval*
    (progn
      (initget 6)
      (setq tempval (getdist (strcat "\nEnter rounding value <" (rtos *rndval* 3 4) "> : ")))
      (if tempval (setq *rndval* tempval))
    )
    (progn
      (initget 7)
      (setq *rndval* (getdist "\nEnter rounding value: "))
    )
  )

  (if *fuzz*
    (progn
      (initget 6)
      (setq tempval 
        (getint 
          (strcat "\nEnter fuzz factor number of decimal places <" 
            (itoa (atoi (StringRight "-" (rtos *fuzz* 1 0)))) "> : ")))
      (if tempval (setq *fuzz* (atof (strcat "1e-" (rtos tempval 2)))))
    )
    (progn
      (initget 6)
      (setq tempval (getint "\nEnter fuzz factor number of decimal places <10> : "))
      (if tempval
        (setq *fuzz* (atof (strcat "1e-" (rtos tempval 2))))
        (setq *fuzz* 1e-10)
      )
    )
  )

  (princ (strcat "\nFuzz factor = " (rtos *fuzz* 1 0) "\n"))

  (setq ss (ssget (list (cons 0 "LINE,INSERT,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE,POINT"))))
  (if (not ss) (exit))

  ;; Create list(s) of off grid objects.
  ;; Objects on locked layers are placed in a separate list.
  ;; See the AddToList function. 
  (repeat (sslength ss)
    (setq obj (vlax-ename->vla-object (ssname ss idx))
          objtyp (vlax-get obj 'ObjectName)
          layname (vlax-get obj 'Layer)
    )
    (cond
      ((and 
         (eq "AcDbLine" objtyp)
         ;; These checks avoid "degenerate geometry" errors
         ;; and things like zero length lines. Small objects
         ;; are not included/modified.
         (< *rndval* (vlax-get obj 'Length))
       )
        (cond 
          ((CheckPoint (trans (vlax-get obj 'StartPoint) 0 1))
            (AddToList obj layname)
          )
          ((CheckPoint (trans (vlax-get obj 'EndPoint) 0 1))
            (AddToList obj layname)
          )
        )
      )      
      ((eq "AcDbBlockReference" objtyp)
        (if (CheckPoint (trans (vlax-get obj 'InsertionPoint) 0 1))
          (AddToList obj layname)
        )
      )
      ((eq "AcDbPoint" objtyp)
        (if (CheckPoint (trans (vlax-get obj 'Coordinates) 0 1))
          (AddToList obj layname)
        )
      )
      ((and 
         (wcmatch objtyp "*Polyline")
         (< *rndval* (vlax-get obj 'Length))
       )
        (setq flag nil)
        (setq lst (GetCoordinates obj))
        (while (and (not flag) lst)
          (if (CheckPoint (trans (car lst) 0 1)) 
            (progn
              (AddToList obj layname)
              (setq flag T)
            )
          )
          (setq lst (cdr lst))
        )
        (if (and (not flag) (not (eq "AcDb3dPolyline" objtyp)))
          (if (CheckValue (vlax-get obj 'Elevation))
            (AddToList obj layname)
          )
        )
      )
      ((and 
         (eq "AcDbArc" objtyp)
         (< (* 0.5 *rndval*) (setq rad (vlax-get obj 'Radius)))
       )
        (cond
          ((CheckPoint (trans (vlax-get obj 'Center) 0 1))
            (AddToList obj layname)
          )
          ((CheckAngle (vlax-get obj 'StartAngle))
            (AddToList obj layname)
          )
          ((CheckAngle (vlax-get obj 'EndAngle))
            (AddToList obj layname)
          )        
          ((CheckValue rad)
            (AddToList obj layname)
          )
        )
      )
      ((and 
         (eq "AcDbCircle" objtyp)
         (< (* 0.5 *rndval*) (setq rad (vlax-get obj 'Radius)))
       )
        (cond
          ((CheckPoint (trans (vlax-get obj 'Center) 0 1))
            (AddToList obj layname)
          )
          ((CheckValue rad)
            (AddToList obj layname)
          )
        )
      )
      ((and 
         (eq "AcDbEllipse" objtyp)
         (< (* 0.5 *rndval*) (setq majrad (vlax-get obj 'MajorRadius)))
         (< (* 0.5 *rndval*) (setq minrad (vlax-get obj 'MinorRadius)))
       )
        (cond 
          ((CheckPoint (trans (vlax-get obj 'Center) 0 1))
            (AddToList obj layname)
          )
          ((CheckValue majrad)
            (AddToList obj layname)
          )
          ((CheckValue minrad)
            (AddToList obj layname)
          )
          ((CheckAngle (vlax-get obj 'StartAngle))
            (AddToList obj layname)
          )
          ((CheckAngle (vlax-get obj 'EndAngle))
            (AddToList obj layname)
          )
        )
      )
    ) ;cond
    (setq idx (1+ idx))
  ) ;; end create list(s)  
   
  (foreach x sellst (vla-highlight x :vlax-true))
  (foreach x locklst (vla-highlight x :vlax-true))
  
  ;; Check the lists.
  (if (or (< 0 (length sellst)) (< 0 (length locklst)))
    (progn 
      (princ (strcat "\nNumber of off-grid objects found/selected: " 
        (itoa (+ (length sellst) (length locklst)))))    
    )
    (progn
      (princ "\nNo off-grid objects found. ")
      (exit)
    )
  )

  ;; Verify user wants to proceed.
  (initget 1 "Y N  ")
  (setq process
    (getkword "\nRepair selected objects? [Yes/No] <Y>: "))
  (if (eq process "N") 
    (exit)
  )

  ;; User can choose unlock locked layers or not.
  ;; Objects on locked layers are not modified.
  (if locklst
    (progn
      (initget 1 "Y N  ")
      (setq unlock
        (getkword "\Temporarily unlock locked layers? [Yes/No] <Y>: "))
      (if (not (eq unlock "N"))
        (progn
          ;unlock locked layers
          (foreach x lockedlayers
            (vla-put-lock (vla-item layers x) :vlax-false)
          )
          ;merge the two lists
          (setq sellst (append sellst locklst))
        )
        (princ "\nSome objects were not modified due to locked layers. ")
      )
    )
  )

  ;; Modify the objects.
  (foreach obj sellst
    (setq objtyp (vlax-get obj 'ObjectName))
    (cond
      ((eq "AcDbLine" objtyp)
        (setq pt (trans (vlax-get obj 'StartPoint) 0 1))
        (vlax-put obj 'StartPoint (FixPoint pt))
        (setq pt (trans (vlax-get obj 'EndPoint) 0 1))
        (vlax-put obj 'EndPoint (FixPoint pt))
      )      
      ((eq "AcDbBlockReference" objtyp)
        (setq pt (trans (vlax-get obj 'InsertionPoint) 0 1))
        (vlax-put obj 'InsertionPoint (FixPoint pt))
      )
      ((eq "AcDbPoint" objtyp)
        (setq pt (trans (vlax-get obj 'Coordinates) 0 1))
        (vlax-put obj 'Coordinates (FixPoint pt))
      )

      ;Known issue: bulges are not adjusted at arcs in plines
      ;so tangent arcs will not be tangent afterwards.
      ;Typically not noticeable with small rounding values.
      ((eq "AcDbPolyline" objtyp)
        (setq lst (mapcar '(lambda (x) (trans x 0 1)) (GetCoordinates obj))
              lst (mapcar '(lambda (x) (FixPoint x)) lst)
              ;remove z coord
              lst (mapcar '(lambda (x) (list (car x) (cadr x))) lst)
        )
        (vlax-put obj 'Coordinates (apply 'append lst))
        (vlax-put obj 'Elevation (FixValue (vlax-get obj 'Elevation)))
      )
      ((eq "AcDb2dPolyline" objtyp)
        (setq lst (mapcar '(lambda (x) (trans x 0 1)) (GetCoordinates obj))
              lst (mapcar '(lambda (x) (FixPoint x)) lst)
        )
        (vlax-put obj 'Coordinates (apply 'append lst))
        (vlax-put obj 'Elevation (FixValue (vlax-get obj 'Elevation)))
      )
      ((eq "AcDb3dPolyline" objtyp)
        (setq lst (mapcar '(lambda (x) (trans x 0 1)) (GetCoordinates obj))
              lst (mapcar '(lambda (x) (FixPoint x)) lst)
        )
        (vlax-put obj 'Coordinates (apply 'append lst))
      )
      ((eq "AcDbArc" objtyp)
        (setq pt (trans (vlax-get obj 'Center) 0 1))
        (vlax-put obj 'Center (FixPoint pt))
        (vlax-put obj 'StartAngle (FixAngle (vlax-get obj 'StartAngle)))
        (vlax-put obj 'EndAngle (FixAngle (vlax-get obj 'EndAngle)))
        (vlax-put obj 'Radius (FixValue (vlax-get obj 'Radius)))
      )
      ((eq "AcDbCircle" objtyp)
        (setq pt (trans (vlax-get obj 'Center) 0 1))
        (vlax-put obj 'Center (FixPoint pt))
        (vlax-put obj 'Radius (FixValue (vlax-get obj 'Radius)))
      )
      ((eq "AcDbEllipse" objtyp)
        (setq pt (trans (vlax-get obj 'Center) 0 1))
        (vlax-put obj 'Center (FixPoint pt))
        (vlax-put obj 'StartAngle (FixAngle (vlax-get obj 'StartAngle)))
        (vlax-put obj 'EndAngle (FixAngle (vlax-get obj 'EndAngle)))
        (vlax-put obj 'MajorRadius (FixValue (vlax-get obj 'MajorRadius)))
        (vlax-put obj 'MinorRadius (FixValue (vlax-get obj 'MinorRadius)))
      )
    ) ;cond

    (setq cnt (1+ cnt))
  ) ;; end modify objects

  (*error* nil)
) ;end

;shortcut
(defun c:FXG () (c:FixOffGrid))