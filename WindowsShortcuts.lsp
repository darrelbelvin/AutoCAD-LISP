;; Open  -  Lee Mac
;; A wrapper for the 'Open' method of the Shell Object
;; target - [int/str] File, folder or ShellSpecialFolderConstants enum

(defun LM:open ( target / rtn shl )
    (if (and (or (= 'int (type target)) (setq target (findfile target)))
             (setq shl (vla-getinterfaceobject (vlax-get-acad-object) "shell.application"))
        )
        (progn
            (setq rtn (vl-catch-all-apply 'vlax-invoke (list shl 'open target)))
            (vlax-release-object shl)
            (if (vl-catch-all-error-p rtn)
                (prompt (vl-catch-all-error-message rtn))
                t
            )
        )
    )
)

(defun C:ExplorerCurrentFolder (/ ShellObject)
(vl-load-com)
	(setq ShellObject (vla-getInterfaceObject (vlax-get-acad-object) "Shell.Application"))
	(vlax-invoke-method ShellObject 'Explore (getvar "dwgprefix"))
	(vlax-release-object ShellObject)
)

(defun c:cf() (c:ExplorerCurrentFolder))

; (defun c:aliasnotepad() (startapp "notepad" (findfile "acad.pgp")))
(defun c:aliasfile() (LM:open (findfile "acad.pgp")))
; ;|
; Example to Open a File Selected by the User
; ((lambda ( f ) (if f (LM:open f))) (getfiled "Select File" "" "" 16))

; Example to Open a Directory:
; (LM:open "C:\\My Folder\\file.dwg")

; Example to Open the Fonts Special Folder:
; (LM:open 20)
; |;

; ;|
; DEC	HEX	FOLDER
; ------------------------------
; 29	1d	ALTSTARTUP
; 26	1a	APPDATA
; 10	0a	BITBUCKET
; 30	1e	COMMONALTSTARTUP
; 35	23	COMMONAPPDATA
; 25	19	COMMONDESKTOPDIR
; 31	1f	COMMONFAVORITES
; 23	17	COMMONPROGRAMS
; 22	16	COMMONSTARTMENU
; 24	18	COMMONSTARTUP
; 3	3	CONTROLS
; 33	21	COOKIES
; 0	0	DESKTOP
; 16	10	DESKTOPDIRECTORY
; 17	11	DRIVES
; 6	6	FAVORITES
; 20	14	FONTS
; 34	22	HISTORY
; 32	20	INTERNETCACHE
; 28	1c	LOCALAPPDATA
; 39	27	MYPICTURES
; 19	13	NETHOOD
; 18	12	NETWORK
; 5	5	PERSONAL
; 4	4	PRINTERS
; 27	1b	PRINTHOOD
; 40	28	PROFILE
; 38	26	PROGRAMFILES
; 48	30	PROGRAMFILESx86
; 2	2	PROGRAMS
; 8	8	RECENT
; 9	9	SENDTO
; 11	0b	STARTMENU
; 7	7	STARTUP
; 37	25	SYSTEM
; 41	29	SYSTEMx86
; 21	15	TEMPLATES
; 36	24	WINDOWS
; |;