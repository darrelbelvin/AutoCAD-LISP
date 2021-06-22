;;; CADALYST 08/08  www.cadalyst.com/code 
;;; Tip 2305: LeaderToMleader.lsp	Leader to Multileader	(c) 2008 Lyle Hardin 
;;; Pick an old style leader and text to create a new mleader entity and erase the old leader and text.
;;; March/2008

(defun ToMLeader (getleader / *error* main doc)
	(vl-load-com)
	(defun main ();/ leader leader2 pt1 pt2 pt3 layer mtext mtext2 text)
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))

		(if (= acTrue getleader)
			(setq	leader	(entsel "\nPick Leader")	; pick leader
			leader2	(entget (car leader))
			pt1	(dxf 10 leader2)		; get first point of leader
		;	pt2	(dxf2 10 leader2)		; get second point of leader
			layer 	(dxf 8 leader2)		; get layer of leader
			mtext	(entsel "\nPick Text")	; pick text
			mtext2	(entget (car mtext))
			)
			(setq
			mtext	(entsel "\nPick Text")	; pick text
			mtext2	(entget (car mtext))
			layer 	(dxf 8 mtext2)		; get layer of text
			)
		)

		(if (= (dxf 0 mtext2) "TEXT")
			(progn
				(command "TXT2MTXT" mtext "")
				(setq mtext (entlast)
					mtext2	(entget mtext)
				)
			)
		)

		(setq
		pt2	(dxf 10 mtext2)		; get point of text
		pt3	(list (+ (dxf 41 mtext2) (car pt2)) (- (cadr pt2) (dxf 46 mtext2)) (caddr pt2)) ; opposite corner of text
		text (GetText mtext2)		; get text
		)					; setq
		
		(command "-layer" "s" layer "")	; set layer of leader picked to current
		(command ".erase" mtext "")		; erase text picked
		(if (= acTrue getleader) 
			(progn
				(command ".erase" leader "")		; erase leader picked
				(command ".mleader" "h" "c" "_non" pt2 "_non" pt3 text "_non" pt1) ; mleader command with content first
			)
			(command ".mleader" "h" "c" "_non" pt2 "_non" pt3 (substr text 1 140) PAUSE)
		)
		(setpropertyvalue (entlast) "MText/Contents" text)
		
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

(defun c:LEADERTOMLEADER() (ToMLeader acTrue))
(defun c:TEXTTOMLEADER() (ToMLeader acFalse))
(defun c:TML() (c:leadertomleader))
(defun c:TTML() (c:texttomleader))

(defun dxf(code elist)		; define dxf function
  (cdr (assoc code elist))     ;Finds the association pair, strips 1st element
);defun

(defun dxf2(code elist)		; define dxf function for 2nd item
  (cdr (NTHASSOC 1 code elist))     ;Finds the association pair, strips 2nd element
);defun

(defun NTHASSOC (N KEY LST / ITEM) ;;DCBroad 2008
	(setq ITEM (assoc KEY LST))
	(if (<= N 0)
		ITEM
		(NTHASSOC (1- N) KEY (cdr (member ITEM LST)))
	)
)

(defun AllAssoc (code elist / next)
	(setq lst nil)
	(while (setq next (assoc code elist)) 
		(setq lst (append lst (list (cdr next)))
			elist (cdr (member next elist))
		)	
	)
	lst
)

(defun GetText(e)
	(LM:lst->str (append (AllAssoc 3 e) (list (dxf 1 e))) "")
)

(defun c:GetText() (GetText (entget (car (entsel)))))

(defun LM:lst->str ( lst del / str ) ; Lee Mac
    (setq str (car lst))
    (foreach itm (cdr lst) (setq str (strcat str del itm)))
    str
)