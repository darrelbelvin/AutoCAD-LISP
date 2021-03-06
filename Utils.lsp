(defun selectionLayerChange (basesel filter targetLayer / laysel)
	(if (setq laysel (ssget "_X" filter))
		(progn
			(setq laysel (selectunion basesel laysel))
			(if (or (not laysel)
				(zerop (sslength laysel)))
				(princ)
				(if (null targetLayer)
					(command "_.erase" laysel "")
					(command "_.change" laysel "" "_Properties" "_LAyer" targetLayer "")
				)
			)
		)
	)
	(setq laysel nil)
)

(defun selectionEntModify (basesel filter newdata / laysel ed i item)
	(if (setq laysel (ssget "_X" filter))
		(progn
			(setq laysel (selectunion basesel laysel))
			(if (or (not laysel)
				(zerop (sslength laysel)))
				(princ)
				(repeat (setq i (sslength laysel))
					(setq ed (entget (ssname laysel (setq i (1- i)))))
					(foreach item newdata (setq ed (subst item (assoc (car item) ed) ed)))
					(entmod ed)
				)
			)
		)
	)
	(setq laysel nil)
)

; examples
; (defun c:fontsize12to7 () (selectionEntModify (getselbothways) (list'(40 . 12.0)) (list'(40 . 7.0))))
; (defun c:fontsize9to4 () (selectionEntModify (getselbothways) (list'(40 . 9.0)) (list'(40 . 4.0))))


(defun getselbothways (/ ss)
	(if (not (setq ss (ssget "_I"))); if there is not an Implied [= pre-] selection [if there is, it will put that into 'ss']
		(progn
			(prompt "\nSelect entities: ")
			(setq ss (ssget)); then -- ask the User to select
		)
	); if
	ss
)

(defun colorByBlockAll (includeNonBlocks / *error* main doc colorBlock colorLayer entity block)
	(vl-load-com)
	(setq colorBlock (vlax-create-object (strcat "AutoCAD.AcCmColor." (substr (getvar "acadver") 1 2))))
	(vla-put-colorIndex colorBlock AcByBlock)
	(setq colorLayer (vlax-create-object (strcat "AutoCAD.AcCmColor." (substr (getvar "acadver") 1 2))))
	(vla-put-colorIndex colorLayer acByLayer)

	(vlax-for block (vla-get-blocks
							(vla-get-activedocument
								(vlax-get-acad-object)
							)
						)
		(if (and (= (vla-get-IsLayout block) :vlax-false)
			(= (vla-get-IsXref block) :vlax-false)
			)
			(vlax-for entity block
				(vla-put-trueColor entity colorBlock)
				(vla-put-layer entity "0" )
				(vlax-release-object entity)
			); vlax-for
			(if includeNonBlocks
				(vlax-for entity block
					(vla-put-trueColor entity colorLayer)
					(vlax-release-object entity)
				); vlax-for
			)
		)
	); vlax-for
)

(defun c:colorByBlockAll ()
	(vl-load-com)
	(defun main ()
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
		
		(initget 1 "Yes No")
		(colorByBlockAll (= "Yes" (getkword "Include stuff outside blocks? (Yes or No) ")))
		
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

(defun styles2one (newfont / styl)
	(while (setq styl (tblnext "STYLE" (not styl)))
		(setq styl (entget (tblobjname "STYLE" (cdr (assoc 2 styl)))))
		(entmod (subst (cons 3 newfont) (assoc 3 styl) styl))
	)
	(command "_.regen")
	(princ)
)

(defun c:kill ( / ss )
    (load "overkillsup")
	(setq ss(ssget))
	(acet-overkill2 (list ss  0.0001 nil T nil nil))
)

(defun c:BlocksPrefix (/ ss n ent name lst)
 (vl-load-com)
 (setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object))
       blocks (vla-get-Blocks acdoc)
       )
 (princ "\Select bloc to rename or Enter for all.")
 (if (or (ssget '((0 . "INSERT")))  (ssget "_X" '((0 . "INSERT"))))
   (if (setq pref (getstring "\nEnter the prefix to add: "))
     (progn
       (vlax-for b (setq ss (vla-get-ActiveSelectionSet acdoc))
         (setq name (vla-get-name b))
         (if (not (vl-position name lst))
           (setq lst (cons name lst))
           )
         )
       (foreach n lst
         (vla-put-Name (vla-item blocks n) (strcat pref n))
       )
     )
   )
 )
 (princ)

(princ)