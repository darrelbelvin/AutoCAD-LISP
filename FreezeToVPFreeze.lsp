(defun c:FreezeToVPFreeze (/ *error* main doc)
  (vl-load-com)
  (defun main ()
    (vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
    (command "VPLAYER" "Thaw" "*" "Current")
    (vlax-for
      layer
      (vla-get-layers
        (vla-get-activedocument
          (vlax-get-acad-object)
        )
      )
      (if (= (vla-get-freeze layer) :vlax-true)
        (command "Freeze" (vla-get-name layer) "Current")
      )
    )
    (command nil)
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

(defun c:FTV() (c:FreezeToVPFreeze))
