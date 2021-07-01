(defun lspOkCancel (message1 message2 message3 main)

  (setq dcl_id (load_dialog "msgbox.dcl"))
  (if (not (new_dialog "lspOkCancel" dcl_id))
    (exit)
  )

  (set_tile "message1" message1)
  (set_tile "message2" message2)
  (set_tile "message3" message3)
  (set_tile "main" main)

  (action_tile
    "cancel"
    "(done_dialog)
     (setq result nil)"
  )
  (action_tile
    "accept"
    "(done_dialog)
     (setq result T)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lspYesNo (message1 message2 message3 main)

  (setq dcl_id (load_dialog "msgbox.dcl"))
  (if (not (new_dialog "lspYesNo" dcl_id))
    (exit)
  )

  (set_tile "message1" message1)
  (set_tile "message2" message2)
  (set_tile "message3" message3)
  (set_tile "main" main)

  (action_tile
    "no"
    "(done_dialog)
     (setq result \"F\")"
  )
  (action_tile
    "yes"
    "(done_dialog)
     (setq result T)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lspOkOnly (message1 message2 message3 main)

  (setq dcl_id (load_dialog "msgbox.dcl"))
  (if (not (new_dialog "lspOkOnly" dcl_id))
    (exit)
  )

  (set_tile "message1" message1)
  (set_tile "message2" message2)
  (set_tile "message3" message3)
  (set_tile "main" main)

  
  (action_tile
    "yes"
    "(done_dialog)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lspYesNoCancel (message1 message2 message3 main)

  (setq dcl_id (load_dialog "msgbox.dcl"))
  (if (not (new_dialog "lspYesNoCancel" dcl_id))
    (exit)
  )

  (set_tile "message1" message1)
  (set_tile "message2" message2)
  (set_tile "message3" message3)
  (set_tile "main" main)

  (action_tile
    "no"
    "(done_dialog)
     (setq result \"F\")"
  )
  (action_tile
    "yes"
    "(done_dialog)
     (setq result T)"
  )
  (action_tile
    "cancel"
    "(done_dialog)
     (setq result nil)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lspRentryCancel (message1 message2 message3 main)

  (setq dcl_id (load_dialog "msgbox.dcl"))
  (if (not (new_dialog "lspRentryCancel" dcl_id))
    (exit)
  )

  (set_tile "message1" message1)
  (set_tile "message2" message2)
  (set_tile "message3" message3)
  (set_tile "main" main)

  (action_tile
    "cancel"
    "(done_dialog)
     (setq result nil)"
  )
  (action_tile
    "rentry"
    "(done_dialog)
     (setq result T)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ)
