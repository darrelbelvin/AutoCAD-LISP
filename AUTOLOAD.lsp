; (defun AddSupportPath (Location Num / cnt tmpStr tmpList)
;   ; Adds a support path to Acad, in the number specified.
;   ; If no numbers specified, then it is added to the end.
;   ; Sub's StrParse

;   (setq Location (strcat Location ";"))
;   (setq Location (vl-string-translate "/" "\\" Location))
;   (if (not (vl-string-search (strcase Location) (strcase (getenv "ACAD"))))
;     (cond
;       ((equal Num 0)
;       (setenv "ACAD" (strcat Location (getenv "ACAD"))))
;       ((not Num)
;       (setenv "ACAD" (strcat (getenv "ACAD") Location)))
;       (T
;         (setq cnt 0)
;         (setq tmpStr "")
;         (setq tmpList (StrParse (getenv "ACAD") ";"))
;         (mapcar
;           '(lambda (x)
;             (if (equal Num cnt)
;               (setq tmpStr (strcat tmpStr Location ";" x ";"))
;               (setq tmpStr (strcat tmpStr x ";"))
;             )
;             (setq cnt (1+ cnt))
;           )
;           tmpList
;         )
;       )
;     )
;   )
;   (if tmpStr
;     (setenv "ACAD" (substr tmpStr 1 (1- (strlen tmpStr))))
;   )
; )

; (defun StrParse (String Seperator / Pos1 Pos2 NewStrList)
;   (setq Pos2 1)
;   (while (setq Pos1 (vl-string-search Seperator String Pos1))
;     (if (= Pos2 1)
;       (setq NewStrList (cons (substr String Pos2 Pos1) NewStrList))
;       (setq NewStrList (cons (substr String Pos2 (- (1+ Pos1) Pos2))
;                             NewStrList))
;     )
;     (setq Pos2 (1+ (+ (strlen Seperator) Pos1)))
;     (setq Pos1 (+ Pos1 (strlen Seperator)))
;   )
;   (reverse (setq NewStrList (cons (substr String Pos2) NewStrList)))
; )

; (AddSupportPath "%HOMEPATH%\\Dropbox (J3 Architects)\\J3 Architects Team Folder\\J3 Architects\\1.Projects\\0.CAD\\CAD setup files\\Lisp Routines" nil)
; (setvar "trustedpaths" "C:%HOMEPATH%\\Dropbox (J3 Architects)\\J3 Architects Team Folder\\J3 Architects\\1.Projects\\0.CAD\\CAD setup files\\Lisp Routines")
(setvar "trustedpaths" (substr (findfile "AUTOLOAD.LSP") 1 (vl-string-search "\\AUTOLOAD.LSP" (findfile "AUTOLOAD.LSP"))))

(autoload "LBL2.LSP" '("LBL"))
(autoload "StripMtext v5-0c.lsp" '("STRIPMTEXT" "SMT"))
(autoload "ScaleAboutCenters.lsp" '("SAC"))
(autoload "MergeHatch.lsp" '("MERGEHATCH" "MH"))
(autoload "BFindV2-0.lsp" '("BFIND"))
(autoload "StealV1-8.lsp" '("STEAL" "STEALALL" "STEALTEMPLATE" "STEALTEMPLATES" "STEALLAST"))
(autoload "BoundaryTrim.lsp" '("RECTANGLETRIM" "BOUNDARYTRIM" "ERASEOUTSIDEBOUNDARY"))
(autoload "FixOffGrid.lsp" '("FIXOFFGRID" "FXG"))
(autoload "PolyOutlineV1-1.lsp" '("POL" "POLYOUTLINE" "MPOLYOUTLINE"))
(autoload "ChangeMline.lsp" '("CHMLS"))
(autoload "RevitLayerProcessing.lsp" '("REVITELEVLAYERS" "REVITPLANLAYERS"))
(autoload "LeaderToMleader.LSP" '("LEADERTOMLEADER" "TML" "TEXTTOMLEADER" "TTML"))
(autoload "ShowDims.lsp" '("SHOWDIMS" "SD"))
(autoload "PLDiet.lsp" '("POLYLINEDIET" "PLD"))
(autoload "ShowDims.lsp" '("SHOWDIMS" "SD" "HIGHIGHTOFFAXIS" "HOA"))
(load "layerstates.lsp" nil)

(load "CALCHELPERS.LSP" nil)
(load "SELECTUNION.LSP" nil)
(load "MultiLineShortcuts.lsp" nil)
(load "RecreateHatchBoundary.lsp" nil)
(load "WindowsShortcuts.lsp" nil)
(load "MatchPropReverse.lsp" nil)

(load "LayerRoutines.LSP" nil)

(setvar "TEXTALLCAPS" 1)
(setvar "PROXYNOTICE" 0)
(setvar "ISAVEPERCENT" 0)
(defun c:sq () (c:all)(command "QSAVE" "CLOSE"))
(defun c:eg () (entget (car (entsel))))
(defun c:ula () (command "-LAYER" "Unlock" "*" ""))
(defun c:s1 () (setvar "SNAPMODE" 1)(setvar "SNAPUNIT" (list 1 1)))
(defun c:s2 () (setvar "SNAPMODE" 1)(setvar "SNAPUNIT" (list 0.5 0.5)))
(defun c:s4 () (setvar "SNAPMODE" 1)(setvar "SNAPUNIT" (list 0.25 0.25)))
