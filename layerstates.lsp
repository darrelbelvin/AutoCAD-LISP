(defun ListLayerStates( / nme lslist)
;   (princ"\n*** Layer State List
; ***")
  (vlax-for x
    (vla-item
      (vla-GetExtensionDictionary
        (vla-get-layers
          (vlax-get-property
            (vlax-get-acad-object)
          'activedocument
          )
        )
      )
      "ACAD_LAYERSTATES"
    )
    (setq nme(vlax-get x 'name))
;   (princ(strcat
;   "\n" (setq nme(vlax-get x 'name))))
    (setq lsList (cons nme lsList))
  )
  (reverse lsList)
)

(defun c:DeleteAllLayerStates (/ *error* main doc)
  (vl-load-com)
  (defun main ()
    (vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
    
    (foreach state (ListLayerStates)
      (command ".layer" "state" "delete" state nil nil nil)
    )
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

(defun c:ALLLS (/ *error* main doc)
  (vl-load-com)
  (defun main ()
    (vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
    (command ".layer" "state" "restore" "all" nil nil)
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

(defun C:FlipLay (/ nme)
  (vl-load-com)
  (or *acad* (setq *acad* (vlax-get-acad-object)))
  (or *doc* (setq *doc* (vla-get-activedocument *acad*)))
  (or *layers* (setq *layers* (vla-get-layers *doc*)))
  (vlax-for x *layers*
    (setq nme (vla-get-name x))
    (cond
      ((= (getvar "clayer") nme) t)
      ((= (vla-get-freeze x) :vlax-True)(vla-put-freeze x :vlax-False))
      ((= (vla-get-freeze x) :vlax-False)(vla-put-freeze x :vlax-True))
    )
    ; (cond
    ;   ((= (vla-get-layeron x) :vlax-True)(vla-put-layeron x :vlax-False))
    ;   ((= (vla-get-layeron x) :vlax-False)(vla-put-layeron x :vlax-True))
    ; )
  )
  (vla-regen *doc* acActiveViewport)
  (princ)
)

(defun c:savecommandstolayerstates (/ *error* main doc)
  (vl-load-com)
  (defun main ()
    (vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
    
    (setq cmdlist (list C:MF C:UF C:BF))

    (foreach cmd cmdlist 
      (progn
        (setq cmdstr (vl-princ-to-string cmd)
              pos (vl-string-search "C:" cmdstr)
              cmdstr (substr cmdstr (+ 3 pos) (- (strlen cmdstr) (+ 3 pos)))
        )
        (cmd)
        (command "-Layer" "State" "Save" cmdstr "" "" "")
      )
    )
    
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
