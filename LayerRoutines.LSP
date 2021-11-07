;;;*MFLAY-12.LSP
;;;*THIS IS THE LISP LAYER ROUTINES FOR TERRENE HOMES
;;;**** Copyright 12****
;;;             Kevin Rech
;;;
;;;7/17/12 Standard Layering
;;;-----------------------------------------------------------------------------
;;;https://regex101.com/r/TaSQZl/1 to extract all function definitions

;********* START OF LAYER ROUTINES **********
;___________________________________________________________________

(defun LA:commonLayerPre(goToModel, turnOff)
  ;set enviroment variables
  (command "cmdecho" "0" )
  (command ".regenauto" "off" )
  (command ".expert" "5")
  (setvar "tilemode" 1)

  (if (= goToModel acTrue) (
    ;set to current sheet
    (setq cursht "Model" )
  ))

  ;set for a fast regen
  (if (= turnOff acTrue) (command "_.-layer" "t" "0" "m" "0" "f" "*"
  "t" "TITLEBLOCK" ""))

  ;check for proper viewport zooms
  (command ".regenauto" "on" )
)

;turn all layers on
(defun c:all()
  (LA:commonLayerPre acFalse acFalse)

  ;Modify layer properties

  (command "LAYTHW" "LAYON" "_.-layer" "unlock" "*" "" "REGEN")

  (princ "ALL.")  
  (princ)
)

;basement floor
(defun c:bf()
  (LA:commonLayerPre acTrue acTrue)

 ;Modify layer properties

  (command "_.-layer"
  "t" "BE-SD-FAN" "m" "BE-SD-FAN" "c" "90" "BE-SD-FAN" "l" "CONTINUOUS" "BE-SD-FAN" "lw" "0.09" "BE-SD-FAN"
  "t" "BF-APPLIANCE" "m" "BF-APPLIANCE" "c" "5" "BF-APPLIANCE" "l" "CONTINUOUS" "BF-APPLIANCE" "lw" "0.09" "BF-APPLIANCE"
  "t" "BF-CAB" "m" "BF-CAB" "c" "61" "BF-CAB" "l" "CONTINUOUS" "BF-CAB" "lw" "0.09" "BF-CAB"
  "t" "BF-CONC" "m" "BF-CONC" "c" "61" "BF-CONC" "l" "CONTINUOUS" "BF-CONC" "lw" "0.09" "BF-CONC"
  "t" "BF-ENGINEERING" "m" "BF-ENGINEERING" "c" "4" "BF-ENGINEERING" "l" "CONTINUOUS" "BF-ENGINEERING" "lw" "0.20" "BF-ENGINEERING"
  "t" "BF-FLOOR BREAK" "m" "BF-FLOOR BREAK" "c" "252" "BF-FLOOR BREAK" "l" "CONTINUOUS" "BF-FLOOR BREAK" "lw" "0.09" "BF-FLOOR BREAK"
  "t" "BF-HATCH" "m" "BF-HATCH" "c" "33" "BF-HATCH" "l" "CONTINUOUS" "BF-HATCH" "lw" "0.09" "BF-HATCH"
  "t" "BF-HEADER" "m" "BF-HEADER" "c" "4" "BF-HEADER" "l" "CENTER" "BF-HEADER" "lw" "0.30" "BF-HEADER"
  "t" "BF-INT-SW" "m" "BF-INT-SW" "c" "52" "BF-INT-SW" "l" "HIDDEN2" "BF-INT-SW" "lw" "0.25" "BF-INT-SW"
  "t" "BF-NOTES" "m" "BF-NOTES" "c" "141" "BF-NOTES" "l" "CONTINUOUS" "BF-NOTES" "lw" "0.09" "BF-NOTES"
  "t" "BF-OPENING" "m" "BF-OPENING" "c" "6" "BF-OPENING" "l" "CONTINUOUS" "BF-OPENING" "lw" "0.09" "BF-OPENING"
  "t" "BF-ROOM" "m" "BF-ROOM" "c" "255" "BF-ROOM" "l" "CONTINUOUS" "BF-ROOM" "lw" "0.09" "BF-ROOM" "UNLOCK" "BF-ROOM"
  "t" "BF-SOFFIT" "m" "BF-SOFFIT" "c" "255" "BF-SOFFIT" "l" "CONTINUOUS" "BF-SOFFIT" "lw" "0.09" "BF-SOFFIT"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "141" "BF-STAIR" "l" "CONTINUOUS" "BF-STAIR" "lw" "0.09" "BF-STAIR"
  "t" "BF-STONE" "m" "BF-STONE" "c" "33" "BF-STONE" "l" "CONTINUOUS" "BF-STONE" "lw" "0.09" "BF-STONE"
  "t" "BF-STUDS" "m" "BF-STUDS" "c" "106" "BF-STUDS" "l" "CONTINUOUS" "BF-STUDS" "lw" "0.40" "BF-STUDS"
  "t" "BF-TEXT" "m" "BF-TEXT" "c" "255" "BF-TEXT" "l" "CONTINUOUS" "BF-TEXT" "lw" "0.09" "BF-TEXT"
  "t" "BF-WALL ABOVE" "m" "BF-WALL ABOVE" "c" "255" "BF-WALL ABOVE" "l" "HIDDEN" "BF-WALL ABOVE" "lw" "0.09" "BF-WALL ABOVE"
  "t" "BF-WALLS-NEW" "m" "BF-WALLS-NEW" "c" "62" "BF-WALLS-NEW" "l" "CONTINUOUS" "BF-WALLS-NEW" "lw" "0.5" "BF-WALLS-NEW" "unlock" "BF-WALLS-NEW"
  "t" "BF-WALLS-REMOVED" "m" "BF-WALLS-REMOVED" "c" "254" "BF-WALLS-REMOVED" "l" "HIDDEN2" "BF-WALLS-REMOVED" "lw" "0.09" "BF-WALLS-REMOVED" 
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "1" "BF-WALLS" "l" "CONTINUOUS" "BF-WALLS" "lw" "0.25" "BF-WALLS"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS"
  "t" "FN-DIM" "m" "FN-DIM" "c" "40" "FN-DIM" "l" "CONTINUOUS" "FN-DIM" "lw" "0.09" "FN-DIM"
  "t" "FN-ENGINEERING" "m" "FN-ENGINEERING" "c" "4" "FN-ENGINEERING" "l" "CONTINUOUS" "FN-ENGINEERING" "lw" "0.20" "FN-ENGINEERING"
  "t" "FN-FOOTING" "m" "FN-FOOTING" "c" "255" "FN-FOOTING" "l" "HIDDEN2" "FN-FOOTING" "lw" "0.25" "FN-FOOTING"
  "t" "FN-HATCH" "m" "FN-HATCH" "c" "32" "FN-HATCH" "l" "CONTINUOUS" "FN-HATCH" "lw" "0.09" "FN-HATCH"
  "t" "FN-POST" "m" "FN-POST" "c" "1" "FN-POST" "l" "CONTINUOUS" "FN-POST" "lw" "0.25" "FN-POST"
  "t" "FN-TEXT" "m" "FN-TEXT" "c" "255" "FN-TEXT" "l" "CONTINUOUS" "FN-TEXT" "lw" "0.09" "FN-TEXT"
  "t" "FN-VENT" "m" "FN-VENT" "c" "61" "FN-VENT" "l" "CONTINUOUS" "FN-VENT" "lw" "0.09" "FN-VENT"
  "t" "FN-WALL ABOVE" "m" "FN-WALL ABOVE" "c" "255" "FN-WALL ABOVE" "l" "HIDDEN" "FN-WALL ABOVE" "lw" "0.09" "FN-WALL ABOVE"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "1" "FN-WALLS" "l" "CONTINUOUS" "FN-WALLS" "lw" "0.25" "FN-WALLS"
  "t" "MF-MISC" "m" "MF-MISC" "c" "6" "MF-MISC" "l" "HIDDEN" "MF-MISC" "lw" "0.09" "MF-MISC"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "255" "MF-STUDS" "l" "HIDDEN2" "MF-STUDS" "lw" "0.09" "MF-STUDS" "lock" "MF-STUDS"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "252" "MF-WALLS" "l" "HIDDEN2" "MF-WALLS" "lw" "0.09" "MF-WALLS" "lock" "MF-WALLS"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"

 ;Set current layer & Regen
  "s" "BF-TEXT"
  "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

 (princ "BF.")  
 (princ)
)

;basement floor area
(defun c:bfa()
  (LA:commonLayerPre acTrue acTrue)

 ;Modify layer properties

 (command "_.-layer"
  "t" "BF-APPLIANCE" "m" "BF-APPLIANCE" "c" "5" "BF-APPLIANCE" "l" "CONTINUOUS" "BF-APPLIANCE" "lw" "0.09" "BF-APPLIANCE"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"
  "t" "BF-AREA" "m" "BF-AREA" "c" "2" "BF-AREA" "l" "CONTINUOUS" "BF-AREA" "lw" "0.13" "BF-AREA"
  "t" "BE-SD-FAN" "m" "BE-SD-FAN" "c" "90" "BE-SD-FAN" "l" "CONTINUOUS" "BE-SD-FAN" "lw" "0.09" "BE-SD-FAN"
  "t" "BF-CAB" "m" "BF-CAB" "c" "61" "BF-CAB" "l" "CONTINUOUS" "BF-CAB" "lw" "0.09" "BF-CAB"
  "t" "BF-CONC" "m" "BF-CONC" "c" "61" "BF-CONC" "l" "CONTINUOUS" "BF-CONC" "lw" "0.09" "BF-CONC"
  "t" "BF-FLOOR BREAK" "m" "BF-FLOOR BREAK" "c" "252" "BF-FLOOR BREAK" "l" "CONTINUOUS" "BF-FLOOR BREAK" "lw" "0.09" "BF-FLOOR BREAK"
  "t" "FN-FOOTING" "m" "FN-FOOTING" "c" "255" "FN-FOOTING" "l" "HIDDEN2" "FN-FOOTING" "lw" "0.25" "FN-FOOTING"
  "t" "FN-HATCH" "m" "FN-HATCH" "c" "33" "FN-HATCH" "l" "CONTINUOUS" "FN-HATCH" "lw" "0.09" "FN-HATCH"
  "t" "BF-HATCH" "m" "BF-HATCH" "c" "33" "BF-HATCH" "l" "CONTINUOUS" "BF-HATCH" "lw" "0.09" "BF-HATCH"
  "t" "BF-NOTES" "m" "BF-NOTES" "c" "141" "BF-NOTES" "l" "CONTINUOUS" "BF-NOTES" "lw" "0.09" "BF-NOTES"
  "t" "RF-DS" "m" "RF-DS" "c" "255" "RF-DS" "l" "CONTINUOUS" "RF-DS" "lw" "0.09" "RF-DS"
  "t" "BF-OPENING" "m" "BF-OPENING" "c" "6" "BF-OPENING" "l" "CONTINUOUS" "BF-OPENING" "lw" "0.09" "BF-OPENING"
  "t" "FN-POST" "m" "FN-POST" "c" "1" "FN-POST" "l" "CONTINUOUS" "FN-POST" "lw" "0.25" "FN-POST"
  "t" "BF-SOFFIT" "m" "BF-SOFFIT" "c" "255" "BF-SOFFIT" "l" "CONTINUOUS" "BF-SOFFIT" "lw" "0.09" "BF-SOFFIT"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "141" "BF-STAIR" "l" "CONTINUOUS" "BF-STAIR" "lw" "0.09" "BF-STAIR"
  "t" "FN-VENT" "m" "FN-VENT" "c" "61" "FN-VENT" "l" "CONTINUOUS" "FN-VENT" "lw" "0.09" "FN-VENT"
  "t" "FN-WALL ABOVE" "m" "FN-WALL ABOVE" "c" "255" "FN-WALL ABOVE" "l" "HIDDEN" "FN-WALL ABOVE" "lw" "0.09" "FN-WALL ABOVE"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "1" "FN-WALLS" "l" "CONTINUOUS" "FN-WALLS" "lw" "0.25" "FN-WALLS"
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "1" "BF-WALLS" "l" "CONTINUOUS" "BF-WALLS" "lw" "0.25" "BF-WALLS"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"
  "t" "BF-WALLS-NEW" "m" "BF-WALLS-NEW" "c" "62" "BF-WALLS-NEW" "l" "CONTINUOUS" "BF-WALLS-NEW" "lw" "0.5" "BF-WALLS-NEW" "unlock" "BF-WALLS-NEW"

 ;Set current layer & Regen
  "s" "BF-AREA"
  "" "REGEN")

 (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

 (princ "BFA.")  
 (princ)
)
 
;foundation
(defun c:fn()
  (LA:commonLayerPre acTrue acTrue)

 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "FN-BEAM" "m" "FN-BEAM" "c" "51" "FN-BEAM" "l" "CENTER2" "FN-BEAM" "lw" "0.25" "FN-BEAM"
  "t" "FN-DECK-POST" "m" "FN-DECK-POST" "c" "1" "FN-DECK-POST" "l" "CONTINUOUS" "FN-DECK-POST" "lw" "0.25" "FN-DECK-POST"
  "t" "FN-DIM" "m" "FN-DIM" "c" "40" "FN-DIM" "l" "CONTINUOUS" "FN-DIM" "lw" "0.15" "FN-DIM"
  "t" "FN-ENGINEERING" "m" "FN-ENGINEERING" "c" "4" "FN-ENGINEERING" "l" "CONTINUOUS" "FN-ENGINEERING" "lw" "0.20" "FN-ENGINEERING"
  "t" "FN-FOOTING" "m" "FN-FOOTING" "c" "255" "FN-FOOTING" "l" "HIDDEN2" "FN-FOOTING" "lw" "0.20" "FN-FOOTING"
  "t" "FN-HATCH" "m" "FN-HATCH" "c" "33" "FN-HATCH" "l" "CONTINUOUS" "FN-HATCH" "lw" "0.09" "FN-HATCH"
  "t" "FN-HD" "m" "FN-HD" "c" "3" "FN-HD" "l" "CONTINUOUS" "FN-HD" "lw" "0.20" "FN-HD"
  "t" "FN-POST" "m" "FN-POST" "c" "1" "FN-POST" "l" "CONTINUOUS" "FN-POST" "lw" "0.25" "FN-POST"
  "t" "FN-TEXT" "m" "FN-TEXT" "c" "255" "FN-TEXT" "l" "CONTINUOUS" "FN-TEXT" "lw" "0.15" "FN-TEXT"
  "t" "FN-VENT" "m" "FN-VENT" "c" "61" "FN-VENT" "l" "CONTINUOUS" "FN-VENT" "lw" "0.09" "FN-VENT"
  "t" "FN-WALL ABOVE" "m" "FN-WALL ABOVE" "c" "255" "FN-WALL ABOVE" "l" "HIDDEN" "FN-WALL ABOVE" "lw" "0.09" "FN-WALL ABOVE"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "1" "FN-WALLS" "l" "CONTINUOUS" "FN-WALLS" "lw" "0.5" "FN-WALLS"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.09" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "HIDDEN2" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-HD" "m" "MF-HD" "c" "45" "MF-HD" "l" "CONTINUOUS" "MF-HD" "lw" "0.09" "MF-HD"  
  "t" "MF-MISC" "m" "MF-MISC" "c" "6" "MF-MISC" "l" "HIDDEN" "MF-MISC" "lw" "0.09" "MF-MISC"
  "t" "MF-STONE" "m" "MF-STONE" "c" "33" "MF-STONE" "l" "CONTINUOUS" "MF-STONE" "lw" "0.13" "MF-STONE"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "106" "MF-STUDS" "l" "HIDDEN2" "MF-STUDS" "lw" "0.09" "MF-STUDS" "lock" "MF-STUDS"
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "252" "MF-WALLS-EX" "l" "hidden2" "MF-WALLS-EX" "lw" "0.15" "MF-WALLS-EX" "lock" "MF-WALLS-EX"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "252" "MF-WALLS-NEW" "l" "hidden2" "MF-WALLS-NEW" "lw" "0.15" "MF-WALLS-NEW" "lock" "MF-WALLS-NEW"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "252" "MF-WALLS" "l" "HIDDEN2" "MF-WALLS" "lw" "0.15" "MF-WALLS" "lock" "MF-WALLS"
  "t" "MFF-DECK-BM" "m" "MFF-DECK-BM" "c" "51" "MFF-DECK-BM" "l" "CENTER2" "MFF-DECK-BM" "lw" "0.25" "MFF-DECK-BM"
  "t" "RF-DS" "m" "RF-DS" "c" "255" "RF-DS" "l" "CONTINUOUS" "RF-DS" "lw" "0.09" "RF-DS"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.15" "TITLEBLOCK"


 ;Set current layer & Regen
  "s" "FN-TEXT"
  "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "FN.")  
  (princ)
)

;foundation area
(defun c:fna()
  (LA:commonLayerPre acTrue acTrue)
  ;Modify layer properties

 (command "_.-layer"
  "t" "FN-DIM" "m" "FN-DIM" "c" "40" "FN-DIM" "l" "CONTINUOUS" "FN-DIM" "lw" "0.15" "FN-DIM"
  "t" "FN-AREA" "m" "FN-AREA" "c" "2" "FN-AREA" "l" "CONTINUOUS" "FN-AREA" "lw" "0.13" "FN-AREA"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "FN-BEAM" "m" "FN-BEAM" "c" "51" "FN-BEAM" "l" "CENTER2" "FN-BEAM" "lw" "0.25" "FN-BEAM"
  "t" "FN-DECK-POST" "m" "FN-DECK-POST" "c" "1" "FN-DECK-POST" "l" "CONTINUOUS" "FN-DECK-POST" "lw" "0.25" "FN-DECK-POST"
  "t" "FN-ENGINEERING" "m" "FN-ENGINEERING" "c" "4" "FN-ENGINEERING" "l" "CONTINUOUS" "FN-ENGINEERING" "lw" "0.20" "FN-ENGINEERING"
  "t" "FN-FOOTING" "m" "FN-FOOTING" "c" "255" "FN-FOOTING" "l" "HIDDEN2" "FN-FOOTING" "lw" "0.25" "FN-FOOTING"
  "t" "FN-HATCH" "m" "FN-HATCH" "c" "33" "FN-HATCH" "l" "CONTINUOUS" "FN-HATCH" "lw" "0.09" "FN-HATCH"
  "t" "FN-POST" "m" "FN-POST" "c" "1" "FN-POST" "l" "CONTINUOUS" "FN-POST" "lw" "0.25" "FN-POST"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.09" "MF-CONC"
  "t" "FN-TEXT" "m" "FN-TEXT" "c" "255" "FN-TEXT" "l" "CONTINUOUS" "FN-TEXT" "lw" "0.15" "FN-TEXT"
  "t" "MF-DECK" "m" "MF-DECK" "c" "252" "MF-DECK" "l" "DASHED" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "255" "MF-WALLS" "l" "HIDDEN2" "MF-WALLS" "lw" "0.15" "MF-WALLS" "lock" "MF-WALLS"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "255" "MF-WALLS-NEW" "l" "CONTINUOUS" "MF-WALLS-NEW" "lw" "0.5" "MF-WALLS-NEW" "lock" "MF-WALLS-NEW"
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "255" "MF-WALLS-EX" "l" "CONTINUOUS" "MF-WALLS-EX" "lw" "0.3" "MF-WALLS-EX" "lock" "MF-WALLS-EX"
  "t" "MF-STONE" "m" "MF-STONE" "c" "33" "MF-STONE" "l" "CONTINUOUS" "MF-STONE" "lw" "0.13" "MF-STONE"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "106" "MF-STUDS" "l" "HIDDEN2" "MF-STUDS" "lw" "0.09" "MF-STUDS" "lock" "MF-STUDS"
  "t" "FN-VENT" "m" "FN-VENT" "c" "61" "FN-VENT" "l" "CONTINUOUS" "FN-VENT" "lw" "0.09" "FN-VENT"
  "t" "FN-WALL ABOVE" "m" "FN-WALL ABOVE" "c" "255" "FN-WALL ABOVE" "l" "HIDDEN" "FN-WALL ABOVE" "lw" "0.09" "FN-WALL ABOVE"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "1" "FN-WALLS" "l" "CONTINUOUS" "FN-WALLS" "lw" "0.5" "FN-WALLS"
  "t" "RF-DS" "m" "RF-DS" "c" "255" "RF-DS" "l" "CONTINUOUS" "RF-DS" "lw" "0.09" "RF-DS"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.15" "TITLEBLOCK"


  ;Set current layer & Regen
    "s" "FN-AREA"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "FNA.")  
  (princ)
)

;main floor framing
(defun c:mff()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "BF-HEADER" "m" "BF-HEADER" "c" "4" "BF-HEADER" "l" "CENTER" "BF-HEADER" "lw" "0.30" "BF-HEADER"
  "t" "BF-INT-SW" "m" "BF-INT-SW" "c" "52" "BF-INT-SW" "l" "HIDDEN2" "BF-INT-SW" "lw" "0.25" "BF-INT-SW"
  "t" "FN-POST" "m" "FN-POST" "c" "1" "FN-POST" "l" "CONTINUOUS" "FN-POST" "lw" "0.25" "FN-POST"
  "t" "FN-BEAM" "m" "FN-BEAM" "c" "51" "FN-BEAM" "l" "CENTER2" "FN-BEAM" "lw" "0.25" "FN-BEAM"
  "t" "FN-FOOTING" "m" "FN-FOOTING" "c" "33" "FN-FOOTING" "l" "HIDDEN2" "FN-FOOTING" "lw" "0.19" "FN-FOOTING"
  "t" "FN-HD" "m" "FN-HD" "c" "252" "FN-HD" "l" "CONTINUOUS" "FN-HD" "lw" "0.20" "FN-HD"
  "t" "BF-SOFFIT" "m" "BF-SOFFIT" "c" "255" "BF-SOFFIT" "l" "CONTINUOUS" "BF-SOFFIT" "lw" "0.09" "BF-SOFFIT"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "141" "BF-STAIR" "l" "CONTINUOUS" "BF-STAIR" "lw" "0.09" "BF-STAIR"
  "t" "BF-STUDS" "m" "BF-STUDS" "c" "106" "BF-STUDS" "l" "CONTINUOUS" "BF-STUDS" "lw" "0.40" "BF-STUDS"
  "t" "FN-VENT" "m" "FN-VENT" "c" "61" "FN-VENT" "l" "CONTINUOUS" "FN-VENT" "lw" "0.09" "FN-VENT"
  "t" "FN-WALL ABOVE" "m" "FN-WALL ABOVE" "c" "255" "FN-WALL ABOVE" "l" "HIDDEN" "FN-WALL ABOVE" "lw" "0.09" "FN-WALL ABOVE"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "1" "FN-WALLS" "l" "CONTINUOUS" "FN-WALLS" "lw" "0.25" "FN-WALLS"
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "1" "BF-WALLS" "l" "CONTINUOUS" "BF-WALLS" "lw" "0.25" "BF-WALLS"
  "t" "BF-WALLS-NEW" "m" "BF-WALLS-NEW" "c" "62" "BF-WALLS-NEW" "l" "CONTINUOUS" "BF-WALLS-NEW" "lw" "0.25" "BF-WALLS-NEW" "unlock" "BF-WALLS-NEW"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.09" "MF-APPLIANCE"
  "t" "MF-CRAWL-ACC" "m" "MF-CRAWL-ACC" "c" "6" "MF-CRAWL-ACC" "l" "HIDDEN" "MF-CRAWL-ACC" "lw" "0.09" "MF-CRAWL-ACC"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "252" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "HIDDEN2" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-HD" "m" "MF-HD" "c" "3" "MF-HD" "l" "CONTINUOUS" "MF-HD" "lw" "0.25" "MF-HD"
  "t" "MF-HVAC" "m" "MF-HVAC" "c" "60" "MF-HVAC" "l" "HIDDEN2" "MF-HVAC" "lw" "0.09" "MF-HVAC"
  "t" "MF-INT-SW" "m" "MF-INT-SW" "c" "33" "MF-INT-SW" "l" "HIDDEN2" "MF-INT-SW" "lw" "0.18" "MF-INT-SW"
  "t" "MF-MISC" "m" "MF-MISC" "c" "6" "MF-MISC" "l" "HIDDEN" "MF-MISC" "lw" "0.09" "MF-MISC"
  "t" "MF-OPTIONAL" "m" "MF-OPTIONAL" "c" "9" "MF-OPTIONAL" "l" "DASHED2" "MF-OPTIONAL" "lw" "0.09" "MF-OPTIONAL"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "255" "MF-STUDS" "l" "HIDDEN2" "MF-STUDS" "lw" "0.09" "MF-STUDS" "lock" "MF-STUDS"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "252" "MF-WALLS" "l" "HIDDEN2" "MF-WALLS" "lw" "0.09" "MF-WALLS" "lock" "MF-WALLS"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "252" "MF-WALLS-NEW" "l" "dashed2" "MF-WALLS-NEW" "lw" "0.09" "MF-WALLS-NEW" "lock" "MF-WALLS-NEW"
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "252" "MF-WALLS-EX" "l" "dashed2" "MF-WALLS-EX" "lw" "0.09" "MF-WALLS-EX" "lock" "MF-WALLS-EX"
  "t" "MFF-BEAM" "m" "MFF-BEAM" "c" "51" "MFF-BEAM" "l" "CENTER2" "MFF-BEAM" "lw" "0.25" "MFF-BEAM"
  "t" "MFF-DECK-BM" "m" "MFF-DECK-BM" "c" "51" "MFF-DECK-BM" "l" "CENTER2" "MFF-DECK-BM" "lw" "0.25" "MFF-DECK-BM"
  "t" "MFF-DIM" "m" "MFF-DIM" "c" "40" "MFF-DIM" "l" "CONTINUOUS" "MFF-DIM" "lw" "0.09" "MFF-DIM"
  "t" "MFF-HVAC" "m" "MFF-HVAC" "c" "60" "MFF-HVAC" "l" "DASHED" "MFF-HVAC" "lw" "0.09" "MFF-HVAC"
  "t" "MFF-DECK-FRAMING" "m" "MFF-DECK-FRAMING" "c" "111" "MFF-DECK-FRAMING" "l" "CONTINUOUS" "MFF-DECK-FRAMING" "lw" "0.09" "MFF-DECK-FRAMING"
  "t" "MFF-FRAMING" "m" "MFF-FRAMING" "c" "111" "MFF-FRAMING" "l" "CONTINUOUS" "MFF-FRAMING" "lw" "0.15" "MFF-FRAMING"
  "t" "MFF-TEXT" "m" "MFF-TEXT" "c" "255" "MFF-TEXT" "l" "CONTINUOUS" "MFF-TEXT" "lw" "0.09" "MFF-TEXT"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"


  ;Set current layer & Regen
    "s" "MFF-TEXT"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "MFF.")  
  (princ)
)

;main floor
(defun c:mf()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS"
  "t" "ME-SD-FAN" "m" "ME-SD-FAN" "c" "90" "ME-SD-FAN" "l" "CONTINUOUS" "ME-SD-FAN" "lw" "0.09" "ME-SD-FAN"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.09" "MF-APPLIANCE" "unlock" "MF-APPLIANCE"
  "t" "MF-ATTIC-ACC" "m" "MF-ATTIC-ACC" "c" "6" "MF-ATTIC-ACC" "l" "HIDDEN" "MF-ATTIC-ACC" "lw" "0.09" "MF-ATTIC-ACC"
  "t" "MF-CAB" "m" "MF-CAB" "c" "61" "MF-CAB" "l" "CONTINUOUS" "MF-CAB" "lw" "0.13" "MF-CAB"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.25" "MF-CONC"
  "t" "MF-CRAWL-ACC" "m" "MF-CRAWL-ACC" "c" "6" "MF-CRAWL-ACC" "l" "HIDDEN" "MF-CRAWL-ACC" "lw" "0.09" "MF-CRAWL-ACC"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "123" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-DIM" "m" "MF-DIM" "c" "40" "MF-DIM" "l" "CONTINUOUS" "MF-DIM" "lw" "0.13" "MF-DIM"
  "t" "MF-ENGINEERING" "m" "MF-ENGINEERING" "c" "4" "MF-ENGINEERING" "l" "CONTINUOUS" "MF-ENGINEERING" "lw" "0.25" "MF-ENGINEERING"
  "t" "MF-FLOOR BREAK" "m" "MF-FLOOR BREAK" "c" "252" "MF-FLOOR BREAK" "l" "CONTINUOUS" "MF-FLOOR BREAK" "lw" "0.09" "MF-FLOOR BREAK"
  "t" "MF-FLUE" "m" "MF-FLUE" "c" "60" "MF-FLUE" "l" "DASHED" "MF-FLUE" "lw" "0.09" "MF-FLUE"
  "t" "MF-HATCH" "m" "MF-HATCH" "c" "253" "MF-HATCH" "l" "CONTINUOUS" "MF-HATCH" "lw" "0.15" "MF-HATCH"
  "t" "MF-HD" "m" "MF-HD" "c" "3" "MF-HD" "l" "CONTINUOUS" "MF-HD" "lw" "0.25" "MF-HD"
  "t" "MF-HDR-ROOF" "m" "MF-HDR-ROOF" "c" "141" "MF-HDR-ROOF" "l" "CENTER" "MF-HDR-ROOF" "lw" "0.30" "MF-HDR-ROOF" "UNLOCK" "MF-HDR-ROOF"
  "t" "MF-HEADER" "m" "MF-HEADER" "c" "4" "MF-HEADER" "l" "CENTER" "MF-HEADER" "lw" "0.30" "MF-HEADER" "UNLOCK" "MF-HEADER"
  "t" "MF-HVAC" "m" "MF-HVAC" "c" "60" "MF-HVAC" "l" "CONTINUOUS" "MF-HVAC" "lw" "0.09" "MF-HVAC"
  "t" "MF-INT-SW" "m" "MF-INT-SW" "c" "52" "MF-INT-SW" "l" "HIDDEN2" "MF-INT-SW" "lw" "0.25" "MF-INT-SW"
  "t" "MF-MISC" "m" "MF-MISC" "c" "6" "MF-MISC" "l" "HIDDEN" "MF-MISC" "lw" "0.09" "MF-MISC"
  "t" "MF-OPENING" "m" "MF-OPENING" "c" "6" "MF-OPENING" "l" "CONTINUOUS" "MF-OPENING" "lw" "0.15" "MF-OPENING"
  "t" "MF-OPTIONAL" "m" "MF-OPTIONAL" "c" "9" "MF-OPTIONAL" "l" "DASHED2" "MF-OPTIONAL" "lw" "0.13" "MF-OPTIONAL"
  "t" "MF-ROOM" "m" "MF-ROOM" "c" "255" "MF-ROOM" "l" "CONTINUOUS" "MF-ROOM" "lw" "0.09" "MF-ROOM" "UNLOCK" "MF-ROOM"
  "t" "MF-SKYLIGHT" "m" "MF-SKYLIGHT" "c" "90" "MF-SKYLIGHT" "l" "DASHED2" "MF-SKYLIGHT" "lw" "0.13" "MF-SKYLIGHT"
  "t" "MF-SOFFIT-HATCH" "m" "MF-SOFFIT-HATCH" "c" "31" "MF-SOFFIT-HATCH" "l" "CONTINUOUS" "MF-SOFFIT-HATCH" "lw" "0.13" "MF-SOFFIT-HATCH"
  "t" "MF-SOFFIT-ROOF" "m" "MF-SOFFIT-ROOF" "c" "255" "MF-SOFFIT-ROOF" "l" "DASHED2" "MF-SOFFIT-ROOF" "lw" "0.13" "MF-SOFFIT-ROOF"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "255" "MF-SOFFIT" "l" "DASHED2" "MF-SOFFIT" "lw" "0.13" "MF-SOFFIT"
  "t" "MF-STAIR" "m" "MF-STAIR" "c" "141" "MF-STAIR" "l" "CONTINUOUS" "MF-STAIR" "lw" "0.13" "MF-STAIR"
  "t" "MF-STONE" "m" "MF-STONE" "c" "33" "MF-STONE" "l" "CONTINUOUS" "MF-STONE" "lw" "0.13" "MF-STONE"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "111" "MF-STUDS" "l" "CONTINUOUS" "MF-STUDS" "lw" "0.40" "MF-STUDS" "unlock" "MF-STUDS"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"
  "t" "MF-TEXT" "m" "MF-TEXT" "c" "255" "MF-TEXT" "l" "CONTINUOUS" "MF-TEXT" "lw" "0.09" "MF-TEXT"
  "t" "MF-WALL ABOVE" "m" "MF-WALL ABOVE" "c" "255" "MF-WALL ABOVE" "l" "DASHED" "MF-WALL ABOVE" "lw" "0.13" "MF-WALL ABOVE"
  "t" "MF-WALL BELOW" "m" "MF-WALL BELOW" "c" "255" "MF-WALL BELOW" "l" "HIDDEN" "MF-WALL BELOW" "lw" "0.13" "MF-WALL BELOW"
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "1" "MF-WALLS-EX" "l" "CONTINUOUS" "MF-WALLS-EX" "lw" "0.3" "MF-WALLS-EX" "unlock" "MF-WALLS-EX"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "62" "MF-WALLS-NEW" "l" "CONTINUOUS" "MF-WALLS-NEW" "lw" "0.5" "MF-WALLS-NEW" "unlock" "MF-WALLS-NEW"
  "t" "MF-WALLS-REMOVED" "m" "MF-WALLS-REMOVED" "c" "254" "MF-WALLS-REMOVED" "l" "HIDDEN2" "MF-WALLS-REMOVED" "lw" "0.09" "MF-WALLS-REMOVED" 
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.5" "MF-WALLS" "unlock" "MF-WALLS"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.18" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"


  ;Set current layer & Regen
    "s" "MF-TEXT"
    "" "REGEN")


  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "MF.")  
  (princ)
)

;question
(defun c:mfx()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.09" "MF-APPLIANCE" "unlock" "MF-APPLIANCE"
  "t" "MF-ATTIC-ACC" "m" "MF-ATTIC-ACC" "c" "6" "MF-ATTIC-ACC" "l" "HIDDEN" "MF-ATTIC-ACC" "lw" "0.09" "MF-ATTIC-ACC"
  "t" "MF-CRAWL-ACC" "m" "MF-CRAWL-ACC" "c" "6" "MF-CRAWL-ACC" "l" "HIDDEN" "MF-CRAWL-ACC" "lw" "0.09" "MF-CRAWL-ACC"
  "t" "MF-CAB" "m" "MF-CAB" "c" "61" "MF-CAB" "l" "CONTINUOUS" "MF-CAB" "lw" "0.13" "MF-CAB"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.25" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "61" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-DIM" "m" "MF-DIM" "c" "40" "MF-DIM" "l" "CONTINUOUS" "MF-DIM" "lw" "0.13" "MF-DIM"
  "t" "MF-ENGINEERING" "m" "MF-ENGINEERING" "c" "4" "MF-ENGINEERING" "l" "CONTINUOUS" "MF-ENGINEERING" "lw" "0.25" "MF-ENGINEERING"
  "t" "MF-FLOOR BREAK" "m" "MF-FLOOR BREAK" "c" "252" "MF-FLOOR BREAK" "l" "CONTINUOUS" "MF-FLOOR BREAK" "lw" "0.09" "MF-FLOOR BREAK"
  "t" "MF-HATCH" "m" "MF-HATCH" "c" "33" "MF-HATCH" "l" "CONTINUOUS" "MF-HATCH" "lw" "0.15" "MF-HATCH"
  "t" "MF-HEADER" "m" "MF-HEADER" "c" "4" "MF-HEADER" "l" "CENTER" "MF-HEADER" "lw" "0.30" "MF-HEADER" "UNLOCK" "MF-HEADER"
  "t" "MF-HD" "m" "MF-HD" "c" "4" "MF-HD" "l" "CONTINUOUS" "MF-HD" "lw" "0.25" "MF-HD"
  "t" "MF-HDR-ROOF" "m" "MF-HDR-ROOF" "c" "141" "MF-HDR-ROOF" "l" "CENTER" "MF-HDR-ROOF" "lw" "0.30" "MF-HDR-ROOF" "UNLOCK" "MF-HDR-ROOF"
  "t" "MF-HVAC" "m" "MF-HVAC" "c" "60" "MF-HVAC" "l" "CONTINUOUS" "MF-HVAC" "lw" "0.09" "MF-HVAC"
  "t" "MF-FLUE" "m" "MF-FLUE" "c" "60" "MF-FLUE" "l" "DASHED" "MF-FLUE" "lw" "0.09" "MF-FLUE"
  "t" "MF-INT-SW" "m" "MF-INT-SW" "c" "52" "MF-INT-SW" "l" "HIDDEN2" "MF-INT-SW" "lw" "0.25" "MF-INT-SW"
  "t" "MF-MISC" "m" "MF-MISC" "c" "6" "MF-MISC" "l" "HIDDEN" "MF-MISC" "lw" "0.09" "MF-MISC"
  "t" "MF-OPENING" "m" "MF-OPENING" "c" "6" "MF-OPENING" "l" "CONTINUOUS" "MF-OPENING" "lw" "0.15" "MF-OPENING"
  "t" "MF-OPTIONAL" "m" "MF-OPTIONAL" "c" "9" "MF-OPTIONAL" "l" "DASHED2" "MF-OPTIONAL" "lw" "0.13" "MF-OPTIONAL"
  "t" "ME-SD-FAN" "m" "ME-SD-FAN" "c" "90" "ME-SD-FAN" "l" "CONTINUOUS" "ME-SD-FAN" "lw" "0.09" "ME-SD-FAN"
  "t" "MF-SKYLIGHT" "m" "MF-SKYLIGHT" "c" "90" "MF-SKYLIGHT" "l" "DASHED2" "MF-SKYLIGHT" "lw" "0.13" "MF-SKYLIGHT"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "255" "MF-SOFFIT" "l" "DASHED2" "MF-SOFFIT" "lw" "0.13" "MF-SOFFIT"
  "t" "MF-SOFFIT-HATCH" "m" "MF-SOFFIT-HATCH" "c" "31" "MF-SOFFIT-HATCH" "l" "CONTINUOUS" "MF-SOFFIT-HATCH" "lw" "0.13" "MF-SOFFIT-HATCH"
  "t" "MF-STAIR" "m" "MF-STAIR" "c" "141" "MF-STAIR" "l" "CONTINUOUS" "MF-STAIR" "lw" "0.13" "MF-STAIR"
  "t" "MF-STONE" "m" "MF-STONE" "c" "33" "MF-STONE" "l" "CONTINUOUS" "MF-STONE" "lw" "0.13" "MF-STONE"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "106" "MF-STUDS" "l" "CONTINUOUS" "MF-STUDS" "lw" "0.40" "MF-STUDS" "unlock" "MF-STUDS"
  "t" "MF-TEXT" "m" "MF-TEXT" "c" "255" "MF-TEXT" "l" "CONTINUOUS" "MF-TEXT" "lw" "0.09" "MF-TEXT"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"
  "t" "MF-ROOM" "m" "MF-ROOM" "c" "255" "MF-ROOM" "l" "CONTINUOUS" "MF-ROOM" "lw" "0.09" "MF-ROOM" "UNLOCK" "MF-ROOM"
  "t" "MF-WALL ABOVE" "m" "MF-WALL ABOVE" "c" "255" "MF-WALL ABOVE" "l" "DASHED" "MF-WALL ABOVE" "lw" "0.13" "MF-WALL ABOVE"
  "t" "MF-WALL BELOW" "m" "MF-WALL BELOW" "c" "255" "MF-WALL BELOW" "l" "HIDDEN" "MF-WALL BELOW" "lw" "0.13" "MF-WALL BELOW"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.5" "MF-WALLS" "unlock" "MF-WALLS"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "62" "MF-WALLS-NEW" "l" "CONTINUOUS" "MF-WALLS-NEW" "lw" "0.5" "MF-WALLS-NEW" 
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "1" "MF-WALLS-EX" "l" "CONTINUOUS" "MF-WALLS-EX" "lw" "0.3" "MF-WALLS-EX" 
  "t" "MF-WALLS-REMOVED" "m" "MF-WALLS-REMOVED" "c" "254" "MF-WALLS-REMOVED" "l" "HIDDEN2" "MF-WALLS-REMOVED" "lw" "0.3" "MF-WALLS-REMOVED" 
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.18" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"


  ;Set current layer & Regen
    "s" "MF-WALLS-EX"
    "" "REGEN")


  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "MF.")  
  (princ)
)

;main floor area
(defun c:mfa()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS"
  "t" "MF-AREA" "m" "MF-AREA" "c" "2" "MF-AREA" "l" "CONTINUOUS" "MF-AREA" "lw" "0.13" "MF-AREA"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.09" "MF-APPLIANCE"
  "t" "MF-CAB" "m" "MF-CAB" "c" "61" "MF-CAB" "l" "CONTINUOUS" "MF-CAB" "lw" "0.13" "MF-CAB"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.25" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "61" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-FLOOR BREAK" "m" "MF-FLOOR BREAK" "c" "252" "MF-FLOOR BREAK" "l" "CONTINUOUS" "MF-FLOOR BREAK" "lw" "0.09" "MF-FLOOR BREAK"
  "t" "MF-HATCH" "m" "MF-HATCH" "c" "33" "MF-HATCH" "l" "CONTINUOUS" "MF-HATCH" "lw" "0.15" "MF-HATCH"
  "t" "MF-HDR-ROOF" "m" "MF-HDR-ROOF" "c" "141" "MF-HDR-ROOF" "l" "CENTER" "MF-HDR-ROOF" "lw" "0.30" "MF-HDR-ROOF" "UNLOCK" "MF-HDR-ROOF"
  "t" "MF-HVAC" "m" "MF-HVAC" "c" "60" "MF-HVAC" "l" "CONTINUOUS" "MF-HVAC" "lw" "0.09" "MF-HVAC"
  "t" "MF-OPENING" "m" "MF-OPENING" "c" "6" "MF-OPENING" "l" "CONTINUOUS" "MF-OPENING" "lw" "0.15" "MF-OPENING"
  "t" "MF-OPTIONAL" "m" "MF-OPTIONAL" "c" "9" "MF-OPTIONAL" "l" "DASHED2" "MF-OPTIONAL" "lw" "0.13" "MF-OPTIONAL"
  "t" "ME-SD-FAN" "m" "ME-SD-FAN" "c" "90" "ME-SD-FAN" "l" "CONTINUOUS" "ME-SD-FAN" "lw" "0.09" "ME-SD-FAN"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "255" "MF-SOFFIT" "l" "DASHED2" "MF-SOFFIT" "lw" "0.13" "MF-SOFFIT"
  "t" "MF-SOFFIT-HATCH" "m" "MF-SOFFIT-HATCH" "c" "31" "MF-SOFFIT-HATCH" "l" "CONTINUOUS" "MF-SOFFIT-HATCH" "lw" "0.13" "MF-SOFFIT-HATCH"
  "t" "MF-STAIR" "m" "MF-STAIR" "c" "141" "MF-STAIR" "l" "CONTINUOUS" "MF-STAIR" "lw" "0.13" "MF-STAIR"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"
  "t" "MF-ROOM" "m" "MF-ROOM" "c" "255" "MF-ROOM" "l" "CONTINUOUS" "MF-ROOM" "lw" "0.09" "MF-ROOM" "UNLOCK" "MF-ROOM"
  "t" "MF-WALL ABOVE" "m" "MF-WALL ABOVE" "c" "255" "MF-WALL ABOVE" "l" "DASHED" "MF-WALL ABOVE" "lw" "0.13" "MF-WALL ABOVE"
  "t" "MF-WALL BELOW" "m" "MF-WALL BELOW" "c" "255" "MF-WALL BELOW" "l" "HIDDEN" "MF-WALL BELOW" "lw" "0.13" "MF-WALL BELOW"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.5" "MF-WALLS" "unlock" "MF-WALLS"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "1" "MF-WALLS-NEW" "l" "continuous" "MF-WALLS-NEW" "lw" "0.5" "MF-WALLS-NEW" 
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "1" "MF-WALLS-EX" "l" "continuous" "MF-WALLS-EX" "lw" "0.5" "MF-WALLS-EX" 
  "t" "MF-WALLS-REMOVED" "m" "MF-WALLS-REMOVED" "c" "254" "MF-WALLS-REMOVED" "l" "HIDDEN2" "MF-WALLS-REMOVED" "lw" "0.09" "MF-WALLS-REMOVED" 
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.18" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"


  ;Set current layer & Regen
    "s" "MF-AREA"
    "" "REGEN")


  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "MFA.")  
  (princ)
)

;upper floor framing
(defun c:uff()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "MF-ATTIC-ACC" "m" "MF-ATTIC-ACC" "c" "6" "MF-ATTIC-ACC" "l" "HIDDEN" "MF-ATTIC-ACC" "lw" "0.09" "MF-ATTIC-ACC"
  "t" "MF-HEADER" "m" "MF-HEADER" "c" "4" "MF-HEADER" "l" "CENTER" "MF-HEADER" "lw" "0.30" "MF-HEADER"
  "t" "MF-HDR-ROOF" "m" "MF-HDR-ROOF" "c" "141" "MF-HDR-ROOF" "l" "CENTER" "MF-HDR-ROOF" "lw" "0.30" "MF-HDR-ROOF"
  "t" "ME-SD-FAN" "m" "ME-SD-FAN" "c" "90" "ME-SD-FAN" "l" "CONTINUOUS" "ME-SD-FAN" "lw" "0.09" "ME-SD-FAN"
  "t" "MF-HVAC" "m" "MF-HVAC" "c" "60" "MF-HVAC" "l" "CONTINUOUS" "MF-HVAC" "lw" "0.09" "MF-HVAC"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "33" "MF-SOFFIT" "l" "DASHED2" "MF-SOFFIT" "lw" "0.13" "MF-SOFFIT"
  "t" "MF-SOFFIT-HATCH" "m" "MF-SOFFIT-HATCH" "c" "33" "MF-SOFFIT-HATCH" "l" "CONTINUOUS" "MF-SOFFIT-HATCH" "lw" "0.13" "MF-SOFFIT-HATCH"
  "t" "MF-INT-SW" "m" "MF-INT-SW" "c" "52" "MF-INT-SW" "l" "HIDDEN2" "MF-INT-SW" "lw" "0.09" "MF-INT-SW"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "106" "MF-STUDS" "l" "CONTINUOUS" "MF-STUDS" "lw" "0.20" "MF-STUDS"
  "t" "MF-FLUE" "m" "MF-FLUE" "c" "60" "MF-FLUE" "l" "DASHED" "MF-FLUE" "lw" "0.09" "MF-FLUE"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.05" "MF-WALLS"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "1" "MF-WALLS-NEW" "l" "continuous" "MF-WALLS-NEW" "lw" "0.15" "MF-WALLS-NEW" "lock" "MF-WALLS-NEW"
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "1" "MF-WALLS-EX" "l" "continuous" "MF-WALLS-EX" "lw" "0.15" "MF-WALLS-EX" "lock" "MF-WALLS-EX"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "253" "UF-APPLIANCE" "l" "CONTINUOUS" "UF-APPLIANCE" "lw" "0.09" "UF-APPLIANCE"
  "t" "UF-DECK-POST" "m" "UF-DECK-POST" "c" "61" "UF-DECK-POST" "l" "CONTINUOUS" "UF-DECK-POST" "lw" "0.09" "UF-DECK-POST"
  "t" "UF-HD" "m" "UF-HD" "c" "4" "UF-HD" "l" "CONTINUOUS" "UF-HD" "lw" "0.25" "UF-HD"
  "t" "UF-STAIR" "m" "UF-STAIR" "c" "253" "UF-STAIR" "l" "HIDDEN2" "UF-STAIR" "lw" "0.13" "UF-STAIR"
  "t" "UF-OPTIONAL" "m" "UF-OPTIONAL" "c" "9" "UF-OPTIONAL" "l" "DASHED2" "UF-OPTIONAL" "lw" "0.09" "UF-OPTIONAL"
  "t" "UF-STUDS" "m" "UF-STUDS" "c" "255" "UF-STUDS" "l" "HIDDEN2" "UF-STUDS" "lw" "0.1" "UF-STUDS" "lock" "UF-STUDS"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "253" "UF-WALLS" "l" "HIDDEN2" "UF-WALLS" "lw" "0.13" "UF-WALLS" "lock" "UF-WALLS"
  "t" "UF-WALLS-NEW" "m" "UF-WALLS-NEW" "c" "253" "UF-WALLS-NEW" "l" "DASHED" "UF-WALLS-NEW" "lw" "0.10" "UF-WALLS-NEW"
  "t" "UF-WALLS-EX" "m" "UF-WALLS-EX" "c" "253" "UF-WALLS-EX" "l" "DASHED" "UF-WALLS-EX" "lw" "0.10" "UF-WALLS-EX"
  "t" "UFF-DIM" "m" "UFF-DIM" "c" "40" "UFF-DIM" "l" "CONTINUOUS" "UFF-DIM" "lw" "0.13" "UFF-DIM"
  "t" "UFF-ENGINEERING" "m" "UFF-ENGINEERING" "c" "4" "UFF-ENGINEERING" "l" "CONTINUOUS" "UFF-ENGINEERING" "lw" "0.09" "UFF-ENGINEERING"
  "t" "UFF-BEAM" "m" "UFF-BEAM" "c" "51" "UFF-BEAM" "l" "CENTER2" "UFF-BEAM" "lw" "0.50" "UFF-BEAM"
  "t" "UFF-FRAMING" "m" "UFF-FRAMING" "c" "111" "UFF-FRAMING" "l" "CONTINUOUS" "UFF-FRAMING" "lw" "0.25" "UFF-FRAMING"
  "t" "UFF-FLUSH BM" "m" "UFF-FLUSH BM" "c" "51" "UFF-FLUSH BM" "l" "CENTER2" "UFF-FLUSH BM" "lw" "0.40" "UFF-FLUSH BM"
  "t" "UFF-HVAC" "m" "UFF-HVAC" "c" "60" "UFF-HVAC" "l" "DASHED" "UFF-HVAC" "lw" "0.09" "UFF-HVAC"
  "t" "UF-INT-SW" "m" "UF-INT-SW" "c" "33" "UF-INT-SW" "l" "HIDDEN2" "UF-INT-SW" "lw" "0.18" "UF-INT-SW"
  "t" "UFF-TEXT" "m" "UFF-TEXT" "c" "255" "UFF-TEXT" "l" "CONTINUOUS" "UFF-TEXT" "lw" "0.09" "UFF-TEXT"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"

  ;Set current layer & Regen
    "s" "UFF-TEXT"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "UFF.")  
  (princ)
)

;upper floor
(defun c:uf()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "MF-FLUE" "m" "MF-FLUE" "c" "60" "MF-FLUE" "l" "DASHED" "MF-FLUE" "lw" "0.09" "MF-FLUE"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"
  "t" "UE-SD-FAN" "m" "UE-SD-FAN" "c" "90" "UE-SD-FAN" "l" "CONTINUOUS" "UE-SD-FAN" "lw" "0.09" "UE-SD-FAN"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "5" "UF-APPLIANCE" "l" "CONTINUOUS" "UF-APPLIANCE" "lw" "0.13" "UF-APPLIANCE" "unlock" "UF-APPLIANCE"
  "t" "UF-ATTIC-ACC" "m" "UF-ATTIC-ACC" "c" "6" "UF-ATTIC-ACC" "l" "HIDDEN" "UF-ATTIC-ACC" "lw" "0.09" "UF-ATTIC-ACC"
  "t" "UF-CAB" "m" "UF-CAB" "c" "61" "UF-CAB" "l" "CONTINUOUS" "UF-CAB" "lw" "0.09" "UF-CAB"
  "t" "UF-DECK-POST" "m" "UF-DECK-POST" "c" "61" "UF-DECK-POST" "l" "CONTINUOUS" "UF-DECK-POST" "lw" "0.09" "UF-DECK-POST"
  "t" "UF-DECK" "m" "UF-DECK" "c" "61" "UF-DECK" "l" "CONTINUOUS" "UF-DECK" "lw" "0.09" "UF-DECK"
  "t" "UF-DIM" "m" "UF-DIM" "c" "40" "UF-DIM" "l" "CONTINUOUS" "UF-DIM" "lw" "0.13" "UF-DIM"
  "t" "UF-ENGINEERING" "m" "UF-ENGINEERING" "c" "4" "UF-ENGINEERING" "l" "CONTINUOUS" "UF-ENGINEERING" "lw" "0.25" "UF-ENGINEERING"
  "t" "UF-FLOOR BREAK" "m" "UF-FLOOR BREAK" "c" "252" "UF-FLOOR BREAK" "l" "CONTINUOUS" "UF-FLOOR BREAK" "lw" "0.09" "UF-FLOOR BREAK"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "253" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.15" "UF-HATCH"
  "t" "UF-HD" "m" "UF-HD" "c" "4" "UF-HD" "l" "CONTINUOUS" "UF-HD" "lw" "0.25" "UF-HD"
  "t" "UF-HEADER" "m" "UF-HEADER" "c" "4" "UF-HEADER" "l" "CENTER" "UF-HEADER" "lw" "0.30" "UF-HEADER"
  "t" "UF-HVAC" "m" "UF-HVAC" "c" "60" "UF-HVAC" "l" "CONTINUOUS" "UF-HVAC" "lw" "0.09" "UF-HVAC"
  "t" "UF-INT-SW" "m" "UF-INT-SW" "c" "52" "UF-INT-SW" "l" "HIDDEN2" "UF-INT-SW" "lw" "0.25" "UF-INT-SW"
  "t" "UF-NOTES" "m" "UF-NOTES" "c" "141" "UF-NOTES" "l" "CONTINUOUS" "UF-NOTES" "lw" "0.09" "UF-NOTES"
  "t" "UF-OPENING" "m" "UF-OPENING" "c" "6" "UF-OPENING" "l" "CONTINUOUS" "UF-OPENING" "lw" "0.09" "UF-OPENING"
  "t" "UF-OPTIONAL" "m" "UF-OPTIONAL" "c" "9" "UF-OPTIONAL" "l" "DASHED2" "UF-OPTIONAL" "lw" "0.09" "UF-OPTIONAL"
  "t" "UF-ROOM" "m" "UF-ROOM NAME" "c" "255" "UF-ROOM" "l" "CONTINUOUS" "UF-ROOM" "lw" "0.09" "UF-ROOM" "UNLOCK" "UF-ROOM"
  "t" "UF-SKYLIGHT" "m" "UF-SKYLIGHT" "c" "90" "UF-SKYLIGHT" "l" "DASHED2" "UF-SKYLIGHT" "lw" "0.13" "UF-SKYLIGHT"
  "t" "UF-SOFFIT-HATCH" "m" "UF-SOFFIT-HATCH" "c" "31" "UF-SOFFIT-HATCH" "l" "CONTINUOUS" "UF-SOFFIT-HATCH" "lw" "0.13" "UF-SOFFIT-HATCH"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "255" "UF-SOFFIT" "l" "DASHED2" "UF-SOFFIT" "lw" "0.13" "UF-SOFFIT"
  "t" "UF-STAIR" "m" "UF-STAIR" "c" "141" "UF-STAIR" "l" "CONTINUOUS" "UF-STAIR" "lw" "0.09" "UF-STAIR"
  "t" "UF-STONE" "m" "UF-STONE" "c" "33" "UF-STONE" "l" "CONTINUOUS" "UF-STONE" "lw" "0.09" "UF-STONE"
  "t" "UF-STUDS" "m" "UF-STUDS" "c" "111" "UF-STUDS" "l" "CONTINUOUS" "UF-STUDS" "lw" "0.40" "UF-STUDS" "unlock" "UF-STUDS"
  "t" "UF-TEXT" "m" "UF-TEXT" "c" "255" "UF-TEXT" "l" "CONTINUOUS" "UF-TEXT" "lw" "0.13" "UF-TEXT"
  "t" "UF-WALL BELOW" "m" "UF-WALL BELOW" "c" "255" "UF-WALL BELOW" "l" "HIDDEN" "UF-WALL BELOW" "lw" "0.09" "UF-WALL BELOW"
  "t" "UF-WALLS-EX" "m" "UF-WALLS-EX" "c" "1" "UF-WALLS-EX" "l" "continuous" "UF-WALLS-EX" "lw" "0.3" "UF-WALLS-EX"
  "t" "UF-WALLS-NEW" "m" "UF-WALLS-NEW" "c" "62" "UF-WALLS-NEW" "l" "continuous" "UF-WALLS-NEW" "lw" "0.5" "UF-WALLS-NEW"
  "t" "UF-WALLS-REMOVED" "m" "UF-WALLS-REMOVED" "c" "254" "UF-WALLS-REMOVED" "l" "HIDDEN2" "UF-WALLS-REMOVED" "lw" "0.09" "UF-WALLS-REMOVED"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "CONTINUOUS" "UF-WALLS" "lw" "0.5" "UF-WALLS" "unlock" "UF-WALLS"

  ;Set current layer & Regen
    "s" "UF-TEXT"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "UF.")  
  (princ)
)

;upper floor area
(defun c:ufa()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA" 
  "t" "UE-SD-FAN" "m" "UE-SD-FAN" "c" "90" "UE-SD-FAN" "l" "CONTINUOUS" "UE-SD-FAN" "lw" "0.09" "UE-SD-FAN"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "5" "UF-APPLIANCE" "l" "CONTINUOUS" "UF-APPLIANCE" "lw" "0.13" "UF-APPLIANCE"
  "t" "UF-AREA" "m" "UF-AREA" "c" "2" "UF-AREA" "l" "CONTINUOUS" "UF-AREA" "lw" "0.09" "UF-AREA"
  "t" "UF-CAB" "m" "UF-CAB" "c" "61" "UF-CAB" "l" "CONTINUOUS" "UF-CAB" "lw" "0.09" "UF-CAB"
  "t" "UF-DECK" "m" "UF-DECK" "c" "61" "UF-DECK" "l" "CONTINUOUS" "UF-DECK" "lw" "0.09" "UF-DECK"
  "t" "UF-DECK-POST" "m" "UF-DECK-POST" "c" "61" "UF-DECK-POST" "l" "CONTINUOUS" "UF-DECK-POST" "lw" "0.09" "UF-DECK-POST"
  "t" "UF-INT-SW" "m" "UF-INT-SW" "c" "52" "UF-INT-SW" "l" "HIDDEN2" "UF-INT-SW" "lw" "0.25" "UF-INT-SW"
  "t" "UF-FLOOR BREAK" "m" "UF-FLOOR BREAK" "c" "252" "UF-FLOOR BREAK" "l" "CONTINUOUS" "UF-FLOOR BREAK" "lw" "0.09" "UF-FLOOR BREAK"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "252" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.15" "UF-HATCH"
  "t" "UF-OPENING" "m" "UF-OPENING" "c" "6" "UF-OPENING" "l" "CONTINUOUS" "UF-OPENING" "lw" "0.09" "UF-OPENING"
  "t" "UF-OPTIONAL" "m" "UF-OPTIONAL" "c" "9" "UF-OPTIONAL" "l" "DASHED2" "UF-OPTIONAL" "lw" "0.09" "UF-OPTIONAL"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "255" "UF-SOFFIT" "l" "DASHED2" "UF-SOFFIT" "lw" "0.13" "UF-SOFFIT"
  "t" "UF-SOFFIT-HATCH" "m" "UF-SOFFIT-HATCH" "c" "31" "UF-SOFFIT-HATCH" "l" "CONTINUOUS" "UF-SOFFIT-HATCH" "lw" "0.13" "UF-SOFFIT-HATCH"
  "t" "UF-STAIR" "m" "UF-STAIR" "c" "141" "UF-STAIR" "l" "CONTINUOUS" "UF-STAIR" "lw" "0.09" "UF-STAIR"
  "t" "UF-ROOM" "m" "UF-ROOM" "c" "255" "UF-ROOM" "l" "CONTINUOUS" "UF-ROOM" "lw" "0.09" "UF-ROOM" "UNLOCK" "UF-ROOM"
  "t" "UF-WALL BELOW" "m" "UF-WALL BELOW" "c" "255" "UF-WALL BELOW" "l" "HIDDEN" "UF-WALL BELOW" "lw" "0.09" "UF-WALL BELOW"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "CONTINUOUS" "UF-WALLS" "lw" "0.50" "UF-WALLS" "unlock" "UF-WALLS"
  "t" "UF-WALLS-NEW" "m" "UF-WALLS-NEW" "c" "62" "UF-WALLS-NEW" "l" "continuous" "UF-WALLS-NEW" "lw" "0.5" "UF-WALLS-NEW" "unlock" "UF-WALLS-NEW"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"

  ;Set current layer & Regen
    "s" "UF-AREA"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "UFA.")  
  (princ)
)

;roof
(defun c:rf()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "MF-ATTIC-ACC" "m" "MF-ATTIC-ACC" "c" "6" "MF-ATTIC-ACC" "l" "HIDDEN" "MF-ATTIC-ACC" "lw" "0.09" "MF-ATTIC-ACC"
  "t" "MF-FLUE" "m" "MF-FLUE" "c" "60" "MF-FLUE" "l" "DASHED" "MF-FLUE" "lw" "0.09" "MF-FLUE"
  "t" "MF-HDR-ROOF" "m" "MF-HDR-ROOF" "c" "141" "MF-HDR-ROOF" "l" "CENTER" "MF-HDR-ROOF" "lw" "0.30" "MF-HDR-ROOF" "UNLOCK" "MF-HDR-ROOF"
  "t" "MF-INT-SW" "m" "MF-INT-SW" "c" "252" "MF-INT-SW" "l" "HIDDEN2" "MF-INT-SW" "lw" "0.18" "MF-INT-SW"
  "t" "MF-SKYLIGHT" "m" "MF-SKYLIGHT" "c" "90" "MF-SKYLIGHT" "l" "DASHED2" "MF-SKYLIGHT" "lw" "0.13" "MF-SKYLIGHT"
  "t" "MF-SOFFIT-ROOF" "m" "MF-SOFFIT-ROOF" "c" "141" "MF-SOFFIT-ROOF" "l" "DASHED2" "MF-SOFFIT-ROOF" "lw" "0.13" "MF-SOFFIT-ROOF"
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "252" "MF-WALLS-EX" "l" "HIDDEN" "MF-WALLS-EX" "lw" "0.3" "MF-WALLS-EX" 
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "252" "MF-WALLS-NEW" "l" "HIDDEN" "MF-WALLS-NEW" "lw" "0.5" "MF-WALLS-NEW" 
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "252" "MF-WALLS" "l" "HIDDEN" "MF-WALLS" "lw" "0.20" "MF-WALLS" "lock" "MF-WALLS"
  "t" "RF-BEAM" "m" "RF-BEAM" "c" "51" "RF-BEAM" "l" "CENTER2" "RF-BEAM" "lw" "0.40" "RF-BEAM" 
  "t" "RF-DIM" "m" "RF-DIM" "c" "40" "RF-DIM" "l" "CONTINUOUS" "RF-DIM" "lw" "0.13" "RF-DIM"
  "t" "RF-DS" "m" "RF-DS" "c" "255" "RF-DS" "l" "CONTINUOUS" "RF-DS" "lw" "0.13" "RF-DS"
  "t" "RF-ENGINEERING" "m" "RF-ENGINEERING" "c" "20" "RF-ENGINEERING" "l" "CONTINUOUS" "RF-ENGINEERING" "lw" "0.18" "RF-ENGINEERING"
  "t" "RF-FRAMING" "m" "RF-FRAMING" "c" "111" "RF-FRAMING" "l" "CONTINUOUS" "RF-FRAMING" "lw" "0.25" "RF-FRAMING"
  "t" "RF-GUTTER" "m" "RF-GUTTER" "c" "2" "RF-GUTTER" "l" "CONTINUOUS" "RF-GUTTER" "lw" "0.25" "RF-GUTTER"
  "t" "RF-HVAC" "m" "RF-HVAC" "c" "60" "RF-HVAC" "l" "DASHED" "RF-HVAC" "lw" "0.09" "RF-HVAC"
  "t" "RF-OPENING" "m" "RF-OPENING" "c" "6" "RF-OPENING" "l" "CONTINUOUS" "RF-OPENING" "lw" "0.15" "RF-OPENING"
  "t" "RF-OVERFRAME" "m" "RF-OVERFRAME" "c" "33" "RF-OVERFRAME" "l" "CONTINUOUS" "RF-OVERFRAME" "lw" "0.09" "RF-OVERFRAME"
  "t" "RF-PROFILE" "m" "RF-PROFILE" "c" "132" "RF-PROFILE" "l" "CONTINUOUS" "RF-PROFILE" "lw" "0.50" "RF-PROFILE"
  "t" "RF-TEXT" "m" "RF-TEXT" "c" "255" "RF-TEXT" "l" "CONTINUOUS" "RF-TEXT" "lw" "0.18" "RF-TEXT"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"
  "t" "UE-SD-FAN" "m" "UE-SD-FAN" "c" "90" "UE-SD-FAN" "l" "CONTINUOUS" "UE-SD-FAN" "lw" "0.09" "UE-SD-FAN"
  "t" "UF-ATTIC-ACC" "m" "UF-ATTIC-ACC" "c" "6" "UF-ATTIC-ACC" "l" "HIDDEN" "UF-ATTIC-ACC" "lw" "0.09" "UF-ATTIC-ACC"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "253" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.09" "UF-HATCH"
  "t" "UF-HEADER" "m" "UF-HEADER" "c" "4" "UF-HEADER" "l" "CENTER" "UF-HEADER" "lw" "0.30" "UF-HEADER"
  "t" "UF-INT-SW" "m" "UF-INT-SW" "c" "33" "UF-INT-SW" "l" "HIDDEN2" "UF-INT-SW" "lw" "0.18" "UF-INT-SW"
  "t" "UF-SKYLIGHT" "m" "UF-SKYLIGHT" "c" "90" "UF-SKYLIGHT" "l" "DASHED2" "UF-SKYLIGHT" "lw" "0.13" "UF-SKYLIGHT"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "33" "UF-SOFFIT" "l" "HIDDEN" "UF-SOFFIT" "lw" "0.09" "UF-SOFFIT"
  "t" "UF-STUDS" "m" "UF-STUDS" "c" "106" "UF-STUDS" "l" "CONTINUOUS" "UF-STUDS" "lw" "0.25" "UF-STUDS"
  "t" "UF-WALL ABOVE" "m" "UF-WALL ABOVE" "c" "255" "UF-WALL ABOVE" "l" "DASHED" "UF-WALL ABOVE" "lw" "0.09" "UF-WALL ABOVE"
  "t" "UF-WALLS-EX" "m" "UF-WALLS-EX" "c" "1" "UF-WALLS-EX" "l" "DASHED" "UF-WALLS-EX" "lw" "0.30" "UF-WALLS-EX"
  "t" "UF-WALLS-NEW" "m" "UF-WALLS-NEW" "c" "62" "UF-WALLS-NEW" "l" "DASHED" "UF-WALLS-NEW" "lw" "0.30" "UF-WALLS-NEW"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "DASHED" "UF-WALLS" "lw" "0.30" "UF-WALLS"

  ;Set current layer & Regen
    "s" "RF-TEXT"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "RF.")  
  (princ)
)

;footprint for site plans
(defun c:site()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS"
  "t" "BF-AREA" "m" "BF-AREA" "c" "2" "BF-AREA" "l" "CONTINUOUS" "BF-AREA" "lw" "0.13" "BF-AREA" 
  "t" "MF-AREA" "m" "MF-AREA" "c" "2" "MF-AREA" "l" "CONTINUOUS" "MF-AREA" "lw" "0.40" "MF-AREA"
  "t" "MF-DECK" "m" "MF-DECK" "c" "2" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "UF-AREA" "m" "UF-AREA" "c" "6" "UF-AREA" "l" "DASHED" "UF-AREA" "lw" "0.13" "UF-AREA"
  "t" "RF-PROFILE" "m" "RF-PROFILE" "c" "6" "RF-PROFILE" "l" "HIDDEN" "RF-PROFILE" "lw" "0.09" "RF-PROFILE"


  ;Set current layer & Regen
    "s" "EL-TEXT"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "SITE.")  
  (princ)
)

;roof area
(defun c:rfa()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "252" "MF-WALLS" "l" "HIDDEN" "MF-WALLS" "lw" "0.20" "MF-WALLS" "lock" "MF-WALLS"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "252" "MF-WALLS-NEW" "l" "HIDDEN" "MF-WALLS-NEW" "lw" "0.20" "MF-WALLS-NEW" "lock" "MF-WALLS-NEW"
  "t" "MF-HDR-ROOF" "m" "MF-HDR-ROOF" "c" "141" "MF-HDR-ROOF" "l" "CENTER" "MF-HDR-ROOF" "lw" "0.30" "MF-HDR-ROOF" "UNLOCK" "MF-HDR-ROOF"
  "t" "UE-SD-FAN" "m" "UE-SD-FAN" "c" "90" "UE-SD-FAN" "l" "CONTINUOUS" "UE-SD-FAN" "lw" "0.09" "UE-SD-FAN"
  "t" "UF-HEADER" "m" "UF-HEADER" "c" "4" "UF-HEADER" "l" "CENTER" "UF-HEADER" "lw" "0.30" "UF-HEADER"
  "t" "UF-INT-SW" "m" "UF-INT-SW" "c" "33" "UF-INT-SW" "l" "HIDDEN2" "UF-INT-SW" "lw" "0.18" "UF-INT-SW"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "33" "UF-SOFFIT" "l" "HIDDEN" "UF-SOFFIT" "lw" "0.09" "UF-SOFFIT"
  "t" "UF-WALL ABOVE" "m" "UF-WALL ABOVE" "c" "255" "UF-WALL ABOVE" "l" "DASHED" "UF-WALL ABOVE" "lw" "0.09" "UF-WALL ABOVE"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "DASHED" "UF-WALLS" "lw" "0.30" "UF-WALLS" "lock" "UF-WALLS"
  "t" "UF-WALLS-NEW" "m" "UF-WALLS-NEW" "c" "1" "UF-WALLS-NEW" "l" "DASHED" "UF-WALLS-NEW" "lw" "0.30" "UF-WALLS-NEW" "lock" "UF-WALLS-NEW"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "252" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.09" "UF-HATCH"
  "t" "RF-AREA" "m" "RF-AREA" "c" "2" "RF-AREA" "l" "CONTINUOUS" "RF-AREA" "lw" "0.13" "RF-AREA"
  "t" "RF-BEAM" "m" "RF-BEAM" "c" "51" "RF-BEAM" "l" "CENTER2" "RF-BEAM" "lw" "0.25" "RF-BEAM" 
  "t" "RF-DIM" "m" "RF-DIM" "c" "40" "RF-DIM" "l" "CONTINUOUS" "RF-DIM" "lw" "0.13" "RF-DIM"
  "t" "RF-DS" "m" "RF-DS" "c" "255" "RF-DS" "l" "CONTINUOUS" "RF-DS" "lw" "0.13" "RF-DS"
  "t" "RF-ENGINEERING" "m" "RF-ENGINEERING" "c" "20" "RF-ENGINEERING" "l" "CONTINUOUS" "RF-ENGINEERING" "lw" "0.18" "RF-ENGINEERING"
  "t" "RF-FRAMING" "m" "RF-FRAMING" "c" "111" "RF-FRAMING" "l" "CONTINUOUS" "RF-FRAMING" "lw" "0.25" "RF-FRAMING"
  "t" "RF-GUTTER" "m" "RF-GUTTER" "c" "2" "RF-GUTTER" "l" "CONTINUOUS" "RF-GUTTER" "lw" "0.25" "RF-GUTTER"
  "t" "RF-HVAC" "m" "RF-HVAC" "c" "60" "RF-HVAC" "l" "DASHED" "RF-HVAC" "lw" "0.09" "RF-HVAC"
  "t" "RF-OVERFRAME" "m" "RF-OVERFRAME" "c" "33" "RF-OVERFRAME" "l" "CONTINUOUS" "RF-OVERFRAME" "lw" "0.09" "RF-OVERFRAME"
  "t" "RF-PROFILE" "m" "RF-PROFILE" "c" "132" "RF-PROFILE" "l" "CONTINUOUS" "RF-PROFILE" "lw" "0.50" "RF-PROFILE"
  "t" "RF-TEXT" "m" "RF-TEXT" "c" "255" "RF-TEXT" "l" "CONTINUOUS" "RF-TEXT" "lw" "0.18" "RF-TEXT"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"

  ;Set current layer & Regen
    "s" "RF-AREA"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "RFA.")  
  (princ)
)

;elevations
(defun c:el()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "EL.")  
  (princ)
)

;question
(defun c:be()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "FN-POST" "m" "FN-POST" "c" "1" "FN-POST" "l" "CONTINUOUS" "FN-POST" "lw" "0.25" "FN-POST"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "1" "FN-WALLS" "l" "CONTINUOUS" "FN-WALLS" "lw" "0.25" "FN-WALLS"
  "t" "FN-WALL ABOVE" "m" "FN-WALL ABOVE" "c" "255" "FN-WALL ABOVE" "l" "HIDDEN" "FN-WALL ABOVE" "lw" "0.09" "FN-WALL ABOVE"
  "t" "BE-WIRE" "m" "BE-WIRE" "c" "40" "BE-WIRE" "l" "CONTINUOUS" "BE-WIRE" "lw" "0.09" "BE-WIRE"
  "t" "BE-ELECT." "m" "BE-ELECT." "c" "2" "BE-ELECT." "l" "CONTINUOUS" "BE-ELECT." "lw" "0.15" "BE-ELECT."
  "t" "BE-SD-FAN" "m" "BE-SD-FAN" "c" "90" "BE-SD-FAN" "l" "CONTINUOUS" "BE-SD-FAN" "lw" "0.15" "BE-SD-FAN"
  "t" "BE-TEXT" "m" "BE-TEXT" "c" "255" "BE-TEXT" "l" "CONTINUOUS" "BE-TEXT" "lw" "0.15" "BE-TEXT"
  "t" "BF-ROOM" "m" "BF-ROOM" "c" "252" "BF-ROOM" "l" "CONTINUOUS" "BF-ROOM" "lw" "0.09" "BF-ROOM" "LOCK" "BF-ROOM"
  "t" "BE-DIM" "m" "BE-DIM" "c" "40" "BE-DIM" "l" "CONTINUOUS" "BE-DIM" "lw" "0.18" "BE-DIM"
  "t" "BF-APPLIANCE" "m" "BF-APPLIANCE" "c" "5" "BF-APPLIANCE" "l" "CONTINUOUS" "BF-APPLIANCE" "lw" "0.09" "BF-APPLIANCE"
  "t" "BF-CAB" "m" "BF-CAB" "c" "61" "BF-CAB" "l" "CONTINUOUS" "BF-CAB" "lw" "0.09" "BF-CAB"
  "t" "BF-CONC" "m" "BF-CONC" "c" "255" "BF-CONC" "l" "CONTINUOUS" "BF-CONC" "lw" "0.25" "BF-CONC"
  "t" "BF-FLOOR BREAK" "m" "BF-FLOOR BREAK" "c" "252" "BF-FLOOR BREAK" "l" "CONTINUOUS" "BF-FLOOR BREAK" "lw" "0.09" "BF-FLOOR BREAK"
  "t" "BF-HATCH" "m" "BF-HATCH" "c" "33" "BF-HATCH" "l" "CONTINUOUS" "BF-HATCH" "lw" "0.09" "BF-HATCH"
  "t" "BF-HEADER" "m" "BF-HEADER" "c" "4" "BF-HEADER" "l" "CENTER" "BF-HEADER" "lw" "0.30" "BF-HEADER"
  "t" "BF-OPENING" "m" "BF-OPENING" "c" "6" "BF-OPENING" "l" "CONTINUOUS" "BF-OPENING" "lw" "0.09" "BF-OPENING"
  "t" "BF-SOFFIT" "m" "BF-SOFFIT" "c" "255" "BF-SOFFIT" "l" "DASHED2" "BF-SOFFIT" "lw" "0.09" "BF-SOFFIT"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "141" "BF-STAIR" "l" "CONTINUOUS" "BF-STAIR" "lw" "0.09" "BF-STAIR"
  "t" "MF-STONE" "m" "MF-STONE" "c" "33" "MF-STONE" "l" "CONTINUOUS" "MF-STONE" "lw" "0.09" "MF-STONE"
  "t" "BF-STUDS" "m" "BF-STUDS" "c" "80" "BF-STUDS" "l" "CONTINUOUS" "BF-STUDS" "lw" "0.40" "BF-STUDS"
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "1" "BF-WALLS" "l" "CONTINUOUS" "BF-WALLS" "lw" "0.25" "BF-WALLS" 
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "255" "MF-WALLS" "l" "hidden2" "MF-WALLS" "lw" "0.05" "MF-WALLS" "lock" "MF-WALLS"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"



  ;Set current layer & Regen
    "s" "BE-TEXT"
    "" "REGEN")


  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "BE.")  
  (princ)
)

;question
(defun c:me()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "ME-WIRE" "m" "ME-WIRE" "c" "40" "ME-WIRE" "l" "CONTINUOUS" "ME-WIRE" "lw" "0.09" "ME-WIRE"
  "t" "ME-ELECT." "m" "ME-ELECT." "c" "2" "ME-ELECT." "l" "CONTINUOUS" "ME-ELECT." "lw" "0.15" "ME-ELECT."
  "t" "ME-SD-FAN" "m" "ME-SD-FAN" "c" "90" "ME-SD-FAN" "l" "CONTINUOUS" "ME-SD-FAN" "lw" "0.15" "ME-SD-FAN"
  "t" "ME-TEXT" "m" "ME-TEXT" "c" "255" "ME-TEXT" "l" "CONTINUOUS" "ME-TEXT" "lw" "0.15" "ME-TEXT"
  "t" "ME-DIM" "m" "ME-DIM" "c" "40" "ME-DIM" "l" "CONTINUOUS" "ME-DIM" "lw" "0.18" "ME-DIM"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.09" "MF-APPLIANCE"
  "t" "MF-CAB" "m" "MF-CAB" "c" "61" "MF-CAB" "l" "CONTINUOUS" "MF-CAB" "lw" "0.09" "MF-CAB"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.25" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "61" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-FLOOR BREAK" "m" "MF-FLOOR BREAK" "c" "252" "MF-FLOOR BREAK" "l" "CONTINUOUS" "MF-FLOOR BREAK" "lw" "0.09" "MF-FLOOR BREAK"
  "t" "MF-HATCH" "m" "MF-HATCH" "c" "33" "MF-HATCH" "l" "CONTINUOUS" "MF-HATCH" "lw" "0.09" "MF-HATCH"
  "t" "MF-HEADER" "m" "MF-HEADER" "c" "4" "MF-HEADER" "l" "CENTER" "MF-HEADER" "lw" "0.30" "MF-HEADER"
  "t" "MF-HVAC" "m" "MF-HVAC" "c" "60" "MF-HVAC" "l" "CONTINUOUS" "MF-HVAC" "lw" "0.09" "MF-HVAC"
  "t" "MF-OPENING" "m" "MF-OPENING" "c" "6" "MF-OPENING" "l" "CONTINUOUS" "MF-OPENING" "lw" "0.09" "MF-OPENING"
  "t" "MF-ROOM" "m" "MF-ROOM" "c" "252" "MF-ROOM" "l" "CONTINUOUS" "MF-ROOM" "lw" "0.09" "MF-ROOM" "LOCK" "MF-ROOM"
  "t" "MF-MISC" "m" "MF-MISC" "c" "6" "MF-MISC" "l" "HIDDEN" "MF-MISC" "lw" "0.09" "MF-MISC"
  "t" "MF-WALL ABOVE" "m" "MF-WALL ABOVE" "c" "255" "MF-WALL ABOVE" "l" "DASHED" "MF-WALL ABOVE" "lw" "0.13" "MF-WALL ABOVE"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "255" "MF-SOFFIT" "l" "DASHED2" "MF-SOFFIT" "lw" "0.09" "MF-SOFFIT"
  "t" "MF-STAIR" "m" "MF-STAIR" "c" "141" "MF-STAIR" "l" "CONTINUOUS" "MF-STAIR" "lw" "0.09" "MF-STAIR"
  "t" "MF-STONE" "m" "MF-STONE" "c" "33" "MF-STONE" "l" "CONTINUOUS" "MF-STONE" "lw" "0.09" "MF-STONE"
  "t" "MF-STUDS" "m" "MF-STUDS" "c" "80" "MF-STUDS" "l" "CONTINUOUS" "MF-STUDS" "lw" "0.40" "MF-STUDS"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.25" "MF-WALLS" "unlock" "MF-WALLS"
  "t" "MF-WALL ABOVE" "m" "MF-WALL ABOVE" "c" "255" "MF-WALL ABOVE" "l" "DASHED" "MF-WALL ABOVE" "lw" "0.13" "MF-WALL ABOVE"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "255" "UF-WALLS" "l" "HIDDEN2" "UF-WALLS" "lw" "0.05" "UF-WALLS" "lock" "UF-WALLS"


  ;Set current layer & Regen
    "s" "ME-TEXT"
    "" "REGEN")


  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "ME.")  
  (princ)
)

;question
(defun c:ue()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "DEFPOINTS" "m" "DEFPOINTS" "c" "253" "DEFPOINTS" "l" "CONTINUOUS" "DEFPOINTS" "lw" "0.09" "DEFPOINTS" 
  "t" "UE-WIRE" "m" "UE-WIRE" "c" "40" "UE-WIRE" "l" "CONTINUOUS" "UE-WIRE" "lw" "0.09" "UE-WIRE"
  "t" "UE-ELECT." "m" "UE-ELECT." "c" "2" "UE-ELECT." "l" "CONTINUOUS" "UE-ELECT." "lw" "0.15" "UE-ELECT."
  "t" "UE-SD-FAN" "m" "UE-SD-FAN" "c" "90" "UE-SD-FAN" "l" "CONTINUOUS" "UE-SD-FAN" "lw" "0.15" "UE-SD-FAN"
  "t" "UE-TEXT" "m" "UE-TEXT" "c" "255" "UE-TEXT" "l" "CONTINUOUS" "UE-TEXT" "lw" "0.15" "UE-TEXT"
  "t" "UE-DIM" "m" "UE-DIM" "c" "40" "UE-DIM" "l" "CONTINUOUS" "UE-DIM" "lw" "0.18" "UE-DIM"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "5" "UF-APPLIANCE" "l" "CONTINUOUS" "UF-APPLIANCE" "lw" "0.09" "UF-APPLIANCE"
  "t" "UF-CAB" "m" "UF-CAB" "c" "61" "UF-CAB" "l" "CONTINUOUS" "UF-CAB" "lw" "0.09" "UF-CAB"
  "t" "UF-DECK" "m" "UF-DECK" "c" "61" "UF-DECK" "l" "CONTINUOUS" "UF-DECK" "lw" "0.09" "UF-DECK"
  "t" "UF-DECK-POST" "m" "UF-DECK-POST" "c" "61" "UF-DECK-POST" "l" "CONTINUOUS" "UF-DECK-POST" "lw" "0.09" "UF-DECK-POST"
  "t" "UF-FLOOR BREAK" "m" "UF-FLOOR BREAK" "c" "252" "UF-FLOOR BREAK" "l" "CONTINUOUS" "UF-FLOOR BREAK" "lw" "0.09" "UF-FLOOR BREAK"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "33" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.09" "UF-HATCH"
  "t" "UF-HEADER" "m" "UF-HEADER" "c" "4" "UF-HEADER" "l" "CENTER" "UF-HEADER" "lw" "0.30" "UF-HEADER"
  "t" "UF-HVAC" "m" "UF-HVAC" "c" "60" "UF-HVAC" "l" "CONTINUOUS" "UF-HVAC" "lw" "0.09" "UF-HVAC"
  "t" "UF-ROOM" "m" "UF-ROOM" "c" "255" "UF-ROOM" "l" "CONTINUOUS" "UF-ROOM" "lw" "0.09" "UF-ROOM" "LOCK" "UF-ROOM"
  "t" "UF-OPENING" "m" "UF-OPENING" "c" "6" "UF-OPENING" "l" "CONTINUOUS" "UF-OPENING" "lw" "0.09" "UF-OPENING"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "255" "UF-SOFFIT" "l" "DASHED2" "UF-SOFFIT" "lw" "0.09" "UF-SOFFIT"
  "t" "UF-STAIR" "m" "UF-STAIR" "c" "141" "UF-STAIR" "l" "CONTINUOUS" "UF-STAIR" "lw" "0.09" "UF-STAIR"
  "t" "UF-STONE" "m" "UF-STONE" "c" "33" "UF-STONE" "l" "CONTINUOUS" "UF-STONE" "lw" "0.09" "UF-STONE"
  "t" "UF-STUDS" "m" "UF-STUDS" "c" "80" "UF-STUDS" "l" "CONTINUOUS" "UF-STUDS" "lw" "0.40" "UF-STUDS" "unlock" "UF-STUDS"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "CONTINUOUS" "UF-WALLS" "lw" "0.25" "UF-WALLS" "unlock" "UF-WALLS"
  "t" "SECTION CUT LINE" "m" "SECTION CUT LINE" "c" "255" "SECTION CUT LINE" "l" "CONTINUOUS" "SECTION CUT LINE" "lw" "0.09" "SECTION CUT LINE"
  "t" "TITLEBLOCK" "m" "TITLEBLOCK" "c" "255" "TITLEBLOCK" "l" "CONTINUOUS" "TITLEBLOCK" "lw" "0.09" "TITLEBLOCK"

  ;Set current layer & Regen
    "s" "UE-TEXT"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "UE.")  
  (princ)
)

;question
(defun c:mfm()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.13" "MF-APPLIANCE" "UNLOCK" "MF-APPLIANCE"
  "t" "MF-CAB" "m" "MF-CAB" "c" "61" "MF-CAB" "l" "CONTINUOUS" "MF-CAB" "lw" "0.13" "MF-CAB"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.09" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "61" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-FLOOR" "m" "MF-FLOOR" "c" "254" "MF-FLOOR" "l" "CONTINUOUS" "MF-FLOOR" "lw" "0.09" "MF-FLOOR"
  "t" "MF-FLOOR BREAK" "m" "MF-FLOOR BREAK" "c" "251" "MF-FLOOR BREAK" "l" "CONTINUOUS" "MF-FLOOR BREAK" "lw" "0.09" "MF-FLOOR BREAK"
  "t" "MF-HATCH" "m" "MF-HATCH" "c" "33" "MF-HATCH" "l" "CONTINUOUS" "MF-HATCH" "lw" "0.09" "MF-HATCH"
  "t" "MF-LABELS1" "m" "MF-LABELS1" "c" "255" "MF-LABELS1" "l" "CONTINUOUS" "MF-LABELS1" "lw" "0.15" "MF-LABELS1"
  "t" "MF-LABELS2" "m" "MF-LABELS2" "c" "255" "MF-LABELS2" "l" "CONTINUOUS" "MF-LABELS2" "lw" "0.15" "MF-LABELS2"
  "t" "MF-OPENING" "m" "MF-OPENING" "c" "6" "MF-OPENING" "l" "CONTINUOUS" "MF-OPENING" "lw" "0.13" "MF-OPENING"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "252" "MF-SOFFIT" "l" "HIDDEN2" "MF-SOFFIT" "lw" "0.13" "MF-SOFFIT"
  "t" "MF-STAIR" "m" "MF-STAIR" "c" "141" "MF-STAIR" "l" "CONTINUOUS" "MF-STAIR" "lw" "0.13" "MF-STAIR"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.20" "MF-WALLS" "UNLOCK" "MF-WALLS"


  ;Set current layer & Regen
    "s" "MF-WALLS"
    "" "REGEN")

  (princ "MFM.")  
  (princ)
)

;main floor marketing furniture
(defun c:mfmf()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.09" "MF-APPLIANCE"
  "t" "MF-CAB" "m" "MF-CAB" "c" "61" "MF-CAB" "l" "CONTINUOUS" "MF-CAB" "lw" "0.09" "MF-CAB"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.09" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "61" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-FLOOR" "m" "MF-FLOOR" "c" "254" "MF-FLOOR" "l" "CONTINUOUS" "MF-FLOOR" "lw" "0.09" "MF-FLOOR"
  "t" "MF-FLOOR BREAK" "m" "MF-FLOOR BREAK" "c" "251" "MF-FLOOR BREAK" "l" "CONTINUOUS" "MF-FLOOR BREAK" "lw" "0.09" "MF-FLOOR BREAK"
  "t" "MF-FURN" "m" "MF-FURN" "c" "5" "MF-FURN" "l" "CONTINUOUS" "MF-FURN" "lw" "0.09" "MF-FURN"
  "t" "MF-HATCH" "m" "MF-HATCH" "c" "33" "MF-HATCH" "l" "CONTINUOUS" "MF-HATCH" "lw" "0.09" "MF-HATCH"
  "t" "MF-LABELS2" "m" "MF-LABELS2" "c" "255" "MF-LABELS2" "l" "CONTINUOUS" "MF-LABELS2" "lw" "0.15" "MF-LABELS2"
  "t" "MF-OPENING" "m" "MF-OPENING" "c" "6" "MF-OPENING" "l" "CONTINUOUS" "MF-OPENING" "lw" "0.09" "MF-OPENING"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "252" "MF-SOFFIT" "l" "HIDDEN2" "MF-SOFFIT" "lw" "0.09" "MF-SOFFIT"
  "t" "MF-STAIR" "m" "MF-STAIR" "c" "141" "MF-STAIR" "l" "CONTINUOUS" "MF-STAIR" "lw" "0.09" "MF-STAIR"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.09" "MF-WALLS"
  "t" "MF-WALLS-EX" "m" "MF-WALLS-EX" "c" "1" "MF-WALLS-EX" "l" "CONTINUOUS" "MF-WALLS-EX" "lw" "0.3" "MF-WALLS-EX" "unlock" "MF-WALLS-EX"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "62" "MF-WALLS-NEW" "l" "CONTINUOUS" "MF-WALLS-NEW" "lw" "0.5" "MF-WALLS-NEW" "unlock" "MF-WALLS-NEW"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"

  ;Set current layer & Regen
    "s" "MF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "MFMF.")  
  (princ)
)

;question
(defun c:mfmd()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "5" "MF-APPLIANCE" "l" "CONTINUOUS" "MF-APPLIANCE" "lw" "0.09" "MF-APPLIANCE"
  "t" "MF-CAB" "m" "MF-CAB" "c" "61" "MF-CAB" "l" "CONTINUOUS" "MF-CAB" "lw" "0.09" "MF-CAB"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "CONTINUOUS" "MF-CONC" "lw" "0.09" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "61" "MF-DECK" "l" "CONTINUOUS" "MF-DECK" "lw" "0.09" "MF-DECK"
  "t" "MF-DECK-POST" "m" "MF-DECK-POST" "c" "61" "MF-DECK-POST" "l" "CONTINUOUS" "MF-DECK-POST" "lw" "0.09" "MF-DECK-POST"
  "t" "MF-FLOOR" "m" "MF-FLOOR" "c" "254" "MF-FLOOR" "l" "CONTINUOUS" "MF-FLOOR" "lw" "0.09" "MF-FLOOR"
  "t" "MF-FLOOR BREAK" "m" "MF-FLOOR BREAK" "c" "252" "MF-FLOOR BREAK" "l" "CONTINUOUS" "MF-FLOOR BREAK" "lw" "0.09" "MF-FLOOR BREAK"
  "t" "MF-MARK-DIM" "m" "MF-MARK-DIM" "c" "5" "MF-MARK-DIM" "l" "CONTINUOUS" "MF-MARK-DIM" "lw" "0.09" "MF-MARK-DIM"
  "t" "MF-HATCH" "m" "MF-HATCH" "c" "33" "MF-HATCH" "l" "CONTINUOUS" "MF-HATCH" "lw" "0.09" "MF-HATCH"
  "t" "MF-LABELS2" "m" "MF-LABELS2" "c" "255" "MF-LABELS2" "l" "CONTINUOUS" "MF-LABELS2" "lw" "0.15" "MF-LABELS2"
  "t" "MF-OPENING" "m" "MF-OPENING" "c" "6" "MF-OPENING" "l" "CONTINUOUS" "MF-OPENING" "lw" "0.09" "MF-OPENING"
  "t" "MF-SOFFIT" "m" "MF-SOFFIT" "c" "254" "MF-SOFFIT" "l" "HIDDEN2" "MF-SOFFIT" "lw" "0.09" "MF-SOFFIT"
  "t" "MF-STAIR" "m" "MF-STAIR" "c" "141" "MF-STAIR" "l" "CONTINUOUS" "MF-STAIR" "lw" "0.09" "MF-STAIR"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "1" "MF-WALLS" "l" "CONTINUOUS" "MF-WALLS" "lw" "0.09" "MF-WALLS"

  ;Set current layer & Regen
    "s" "MF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "MFMD.")  
  (princ)
)

;question
(defun c:ufm()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "5" "UF-APPLIANCE" "l" "CONTINUOUS" "UF-APPLIANCE" "lw" "0.13" "UF-APPLIANCE" "UNLOCK" "UF-APPLIANCE"
  "t" "UF-CAB" "m" "UF-CAB" "c" "61" "UF-CAB" "l" "CONTINUOUS" "UF-CAB" "lw" "0.13" "UF-CAB"
  "t" "UF-DECK" "m" "UF-DECK" "c" "61" "UF-DECK" "l" "CONTINUOUS" "UF-DECK" "lw" "0.09" "UF-DECK"
  "t" "UF-DECK-POST" "m" "UF-DECK-POST" "c" "61" "UF-DECK-POST" "l" "CONTINUOUS" "UF-DECK-POST" "lw" "0.09" "UF-DECK-POST"
  "t" "UF-FLOOR" "m" "UF-FLOOR" "c" "254" "UF-FLOOR" "l" "CONTINUOUS" "UF-FLOOR" "lw" "0.09" "UF-FLOOR"
  "t" "UF-FLOOR BREAK" "m" "UF-FLOOR BREAK" "c" "251" "UF-FLOOR BREAK" "l" "CONTINUOUS" "UF-FLOOR BREAK" "lw" "0.09" "UF-FLOOR BREAK"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "33" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.09" "UF-HATCH"
  "t" "UF-LABELS1" "m" "UF-LABELS1" "c" "255" "UF-LABELS1" "l" "CONTINUOUS" "UF-LABELS1" "lw" "0.15" "UF-LABELS1"
  "t" "UF-LABELS2" "m" "UF-LABELS2" "c" "255" "UF-LABELS2" "l" "CONTINUOUS" "UF-LABELS2" "lw" "0.15" "UF-LABELS2"
  "t" "UF-OPENING" "m" "UF-OPENING" "c" "6" "UF-OPENING" "l" "CONTINUOUS" "UF-OPENING" "lw" "0.13" "UF-OPENING"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "252" "UF-SOFFIT" "l" "HIDDEN2" "UF-SOFFIT" "lw" "0.13" "UF-SOFFIT"
  "t" "UF-STAIR" "m" "UF-STAIR" "c" "141" "UF-STAIR" "l" "CONTINUOUS" "UF-STAIR" "lw" "0.13" "UF-STAIR"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "CONTINUOUS" "UF-WALLS" "lw" "0.25" "UF-WALLS" "UNLOCK" "UF-WALLS"
  "t" "UF-ROOM" "m" "UF-ROOM" "c" "255" "UF-ROOM" "l" "CONTINUOUS" "UF-ROOM" "lw" "0.09" "UF-ROOM" "UNLOCK" "UF-ROOM"

  ;Set current layer & Regen
    "s" "UF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "UFM.")  
  (princ)
)

;upper floor maketing furniture
(defun c:ufmf()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "5" "UF-APPLIANCE" "l" "CONTINUOUS" "UF-APPLIANCE" "lw" "0.09" "UF-APPLIANCE"
  "t" "UF-CAB" "m" "UF-CAB" "c" "61" "UF-CAB" "l" "CONTINUOUS" "UF-CAB" "lw" "0.09" "UF-CAB"
  "t" "UF-DECK" "m" "UF-DECK" "c" "61" "UF-DECK" "l" "CONTINUOUS" "UF-DECK" "lw" "0.09" "UF-DECK"
  "t" "UF-DECK-POST" "m" "UF-DECK-POST" "c" "61" "UF-DECK-POST" "l" "CONTINUOUS" "UF-DECK-POST" "lw" "0.09" "UF-DECK-POST"
  "t" "UF-FLOOR" "m" "UF-FLOOR" "c" "254" "UF-FLOOR" "l" "CONTINUOUS" "UF-FLOOR" "lw" "0.09" "UF-FLOOR"
  "t" "UF-FLOOR BREAK" "m" "UF-FLOOR BREAK" "c" "251" "UF-FLOOR BREAK" "l" "CONTINUOUS" "UF-FLOOR BREAK" "lw" "0.09" "UF-FLOOR BREAK"
  "t" "UF-FURN" "m" "UF-FURN" "c" "5" "UF-FURN" "l" "CONTINUOUS" "UF-FURN" "lw" "0.09" "UF-FURN"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "33" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.09" "UF-HATCH"
  "t" "UF-LABELS2" "m" "UF-LABELS2" "c" "255" "UF-LABELS2" "l" "CONTINUOUS" "UF-LABELS2" "lw" "0.15" "UF-LABELS2"
  "t" "UF-OPENING" "m" "UF-OPENING" "c" "6" "UF-OPENING" "l" "CONTINUOUS" "UF-OPENING" "lw" "0.09" "UF-OPENING"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "252" "UF-SOFFIT" "l" "HIDDEN2" "UF-SOFFIT" "lw" "0.09" "UF-SOFFIT"
  "t" "UF-STAIR" "m" "UF-STAIR" "c" "141" "UF-STAIR" "l" "CONTINUOUS" "UF-STAIR" "lw" "0.09" "UF-STAIR"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "CONTINUOUS" "UF-WALLS" "lw" "0.09" "UF-WALLS"
  "t" "UF-WALLS-NEW" "m" "UF-WALLS-NEW" "c" "62" "UF-WALLS-NEW" "l" "continuous" "UF-WALLS-NEW" "lw" "0.5" "UF-WALLS-NEW"
  "t" "UF-ROOM" "m" "UF-ROOM" "c" "255" "UF-ROOM" "l" "CONTINUOUS" "UF-ROOM" "lw" "0.09" "UF-ROOM" "UNLOCK" "UF-ROOM"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA"

  ;Set current layer & Regen
    "s" "MF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "UFMF.")  
  (princ)
)

;question
(defun c:ufmd()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "5" "UF-APPLIANCE" "l" "CONTINUOUS" "UF-APPLIANCE" "lw" "0.09" "UF-APPLIANCE"
  "t" "UF-CAB" "m" "UF-CAB" "c" "61" "UF-CAB" "l" "CONTINUOUS" "UF-CAB" "lw" "0.09" "UF-CAB"
  "t" "UF-DECK" "m" "UF-DECK" "c" "61" "UF-DECK" "l" "CONTINUOUS" "UF-DECK" "lw" "0.09" "UF-DECK"
  "t" "UF-DECK-POST" "m" "UF-DECK-POST" "c" "61" "UF-DECK-POST" "l" "CONTINUOUS" "UF-DECK-POST" "lw" "0.09" "UF-DECK-POST"
  "t" "UF-FLOOR" "m" "UF-FLOOR" "c" "254" "UF-FLOOR" "l" "CONTINUOUS" "UF-FLOOR" "lw" "0.09" "UF-FLOOR"
  "t" "UF-FLOOR BREAK" "m" "UF-FLOOR BREAK" "c" "252" "UF-FLOOR BREAK" "l" "CONTINUOUS" "UF-FLOOR BREAK" "lw" "0.09" "UF-FLOOR BREAK"
  "t" "UF-MARK-DIM" "m" "UF-MARK-DIM" "c" "5" "UF-MARK-DIM" "l" "CONTINUOUS" "UF-MARK-DIM" "lw" "0.09" "UF-MARK-DIM"
  "t" "UF-HATCH" "m" "UF-HATCH" "c" "33" "UF-HATCH" "l" "CONTINUOUS" "UF-HATCH" "lw" "0.09" "UF-HATCH"
  "t" "UF-LABELS2" "m" "UF-LABELS2" "c" "255" "UF-LABELS2" "l" "CONTINUOUS" "UF-LABELS2" "lw" "0.15" "UF-LABELS2"
  "t" "UF-OPENING" "m" "UF-OPENING" "c" "6" "UF-OPENING" "l" "CONTINUOUS" "UF-OPENING" "lw" "0.09" "UF-OPENING"
  "t" "UF-SOFFIT" "m" "UF-SOFFIT" "c" "254" "UF-SOFFIT" "l" "HIDDEN2" "UF-SOFFIT" "lw" "0.09" "UF-SOFFIT"
  "t" "UF-STAIR" "m" "UF-STAIR" "c" "141" "UF-STAIR" "l" "CONTINUOUS" "UF-STAIR" "lw" "0.09" "UF-STAIR"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "1" "UF-WALLS" "l" "CONTINUOUS" "UF-WALLS" "lw" "0.09" "UF-WALLS"

  ;Set current layer & Regen
    "s" "MF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "UFMD.")  
  (princ)
)

;question
(defun c:bfm()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "BF-APPLIANCE" "m" "BF-APPLIANCE" "c" "5" "BF-APPLIANCE" "l" "CONTINUOUS" "BF-APPLIANCE" "lw" "0.09" "BF-APPLIANCE"
  "t" "BF-CAB" "m" "BF-CAB" "c" "61" "BF-CAB" "l" "CONTINUOUS" "BF-CAB" "lw" "0.09" "BF-CAB"
  "t" "BF-CONC" "m" "BF-CONC" "c" "61" "BF-CONC" "l" "CONTINUOUS" "BF-CONC" "lw" "0.09" "BF-CONC"
  "t" "BF-FLOOR" "m" "BF-FLOOR" "c" "253" "BF-FLOOR" "l" "CONTINUOUS" "BF-FLOOR" "lw" "0.09" "BF-FLOOR"
  "t" "BF-FLOOR BREAK" "m" "BF-FLOOR BREAK" "c" "252" "BF-FLOOR BREAK" "l" "CONTINUOUS" "BF-FLOOR BREAK" "lw" "0.09" "BF-FLOOR BREAK"
  "t" "BF-HATCH" "m" "BF-HATCH" "c" "33" "BF-HATCH" "l" "CONTINUOUS" "BF-HATCH" "lw" "0.09" "BF-HATCH"
  "t" "BF-LABELS1" "m" "BF-LABELS1" "c" "255" "BF-LABELS1" "l" "CONTINUOUS" "BF-LABELS1" "lw" "0.15" "BF-LABELS1"
  "t" "BF-LABELS2" "m" "BF-LABELS2" "c" "255" "BF-LABELS2" "l" "CONTINUOUS" "BF-LABELS2" "lw" "0.15" "BF-LABELS2"
  "t" "BF-OPENING" "m" "BF-OPENING" "c" "6" "BF-OPENING" "l" "CONTINUOUS" "BF-OPENING" "lw" "0.09" "BF-OPENING"
  "t" "BF-SOFFIT" "m" "BF-SOFFIT" "c" "254" "BF-SOFFIT" "l" "HIDDEN2" "BF-SOFFIT" "lw" "0.09" "BF-SOFFIT"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "141" "BF-STAIR" "l" "CONTINUOUS" "BF-STAIR" "lw" "0.09" "BF-STAIR"
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "1" "BF-WALLS" "l" "CONTINUOUS" "BF-WALLS" "lw" "0.09" "BF-WALLS"

  ;Set current layer & Regen
    "s" "BF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "BFM.")  
  (princ)
)

;bottom floor marketing furniture
(defun c:bfmf()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties


 (command "_.-layer"
  "t" "BF-APPLIANCE" "m" "BF-APPLIANCE" "c" "5" "BF-APPLIANCE" "l" "CONTINUOUS" "BF-APPLIANCE" "lw" "0.09" "BF-APPLIANCE"
  "t" "BF-CAB" "m" "BF-CAB" "c" "61" "BF-CAB" "l" "CONTINUOUS" "BF-CAB" "lw" "0.09" "BF-CAB"
  "t" "BF-CONC" "m" "BF-CONC" "c" "61" "BF-CONC" "l" "CONTINUOUS" "BF-CONC" "lw" "0.09" "BF-CONC"
  "t" "BF-FLOOR BREAK" "m" "BF-FLOOR BREAK" "c" "252" "BF-FLOOR BREAK" "l" "CONTINUOUS" "BF-FLOOR BREAK" "lw" "0.09" "BF-FLOOR BREAK"
  "t" "BF-FLOOR" "m" "BF-FLOOR" "c" "253" "BF-FLOOR" "l" "CONTINUOUS" "BF-FLOOR" "lw" "0.09" "BF-FLOOR"
  "t" "BF-FURN" "m" "BF-FURN" "c" "5" "BF-FURN" "l" "CONTINUOUS" "BF-FURN" "lw" "0.09" "BF-FURN"
  "t" "BF-HATCH" "m" "BF-HATCH" "c" "33" "BF-HATCH" "l" "CONTINUOUS" "BF-HATCH" "lw" "0.09" "BF-HATCH"
  "t" "BF-LABELS2" "m" "BF-LABELS2" "c" "255" "BF-LABELS2" "l" "CONTINUOUS" "BF-LABELS2" "lw" "0.15" "BF-LABELS2"
  "t" "BF-OPENING" "m" "BF-OPENING" "c" "6" "BF-OPENING" "l" "CONTINUOUS" "BF-OPENING" "lw" "0.09" "BF-OPENING"
  "t" "BF-SOFFIT" "m" "BF-SOFFIT" "c" "254" "BF-SOFFIT" "l" "HIDDEN2" "BF-SOFFIT" "lw" "0.09" "BF-SOFFIT"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "141" "BF-STAIR" "l" "CONTINUOUS" "BF-STAIR" "lw" "0.09" "BF-STAIR"
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "1" "BF-WALLS" "l" "CONTINUOUS" "BF-WALLS" "lw" "0.09" "BF-WALLS"
  "t" "BF-WALLS-NEW" "m" "BF-WALLS-NEW" "c" "62" "BF-WALLS-NEW" "l" "CONTINUOUS" "BF-WALLS-NEW" "lw" "0.5" "BF-WALLS-NEW" "unlock" "BF-WALLS-NEW"
  "t" "MF-TEXT-AREA" "m" "MF-TEXT-AREA" "c" "255" "MF-TEXT-AREA" "l" "CONTINUOUS" "MF-TEXT-AREA" "lw" "0.09" "MF-TEXT-AREA" 

  ;Set current layer & Regen
    "s" "BF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "BFMF.")  
  (princ)
)

;question
(defun c:bfmd()
  (LA:commonLayerPre acTrue acTrue)
 ;Modify layer properties

 (command "_.-layer"
  "t" "BF-APPLIANCE" "m" "BF-APPLIANCE" "c" "5" "BF-APPLIANCE" "l" "CONTINUOUS" "BF-APPLIANCE" "lw" "0.09" "BF-APPLIANCE"
  "t" "BF-CAB" "m" "BF-CAB" "c" "61" "BF-CAB" "l" "CONTINUOUS" "BF-CAB" "lw" "0.09" "BF-CAB"
  "t" "BF-CONC" "m" "BF-CONC" "c" "61" "BF-CONC" "l" "CONTINUOUS" "BF-CONC" "lw" "0.09" "BF-CONC"
  "t" "BF-FLOOR" "m" "BF-FLOOR" "c" "253" "BF-FLOOR" "l" "CONTINUOUS" "BF-FLOOR" "lw" "0.09" "BF-FLOOR"
  "t" "BF-FLOOR BREAK" "m" "BF-FLOOR BREAK" "c" "252" "BF-FLOOR BREAK" "l" "CONTINUOUS" "BF-FLOOR BREAK" "lw" "0.09" "BF-FLOOR BREAK"
  "t" "BF-MARK-DIM" "m" "BF-MARK-DIM" "c" "5" "BF-MARK-DIM" "l" "CONTINUOUS" "BF-MARK-DIM" "lw" "0.09" "BF-MARK-DIM"
  "t" "BF-HATCH" "m" "BF-HATCH" "c" "33" "BF-HATCH" "l" "CONTINUOUS" "BF-HATCH" "lw" "0.09" "BF-HATCH"
  "t" "BF-LABELS2" "m" "BF-LABELS2" "c" "255" "BF-LABELS2" "l" "CONTINUOUS" "BF-LABELS2" "lw" "0.15" "BF-LABELS2"
  "t" "BF-OPENING" "m" "BF-OPENING" "c" "6" "BF-OPENING" "l" "CONTINUOUS" "BF-OPENING" "lw" "0.09" "BF-OPENING"
  "t" "BF-SOFFIT" "m" "BF-SOFFIT" "c" "254" "BF-SOFFIT" "l" "HIDDEN2" "BF-SOFFIT" "lw" "0.09" "BF-SOFFIT"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "141" "BF-STAIR" "l" "CONTINUOUS" "BF-STAIR" "lw" "0.09" "BF-STAIR"
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "1" "BF-WALLS" "l" "CONTINUOUS" "BF-WALLS" "lw" "0.09" "BF-WALLS"

  ;Set current layer & Regen
    "s" "BF-WALLS"
    "" "REGEN")

  (command "_.-layer" "t" "EL*" "ON" "EL*" "t" "0" "ON" "0" "" "REGEN")

  (princ "BFMD.")  
  (princ)
)

;question
(defun c:amf()
  ;save current layer
  (setq SET-OLD-LAYER (getvar "clayer"))
  (LA:commonLayerPre acTrue acFalse)
 ;Modify layer properties

 (command "_.-layer"
  "t" "MF-APPLIANCE" "m" "MF-APPLIANCE" "c" "255" "MF-APPLIANCE" "l" "HIDDEN2" "MF-APPLIANCE" "lw" "0.05" "MF-APPLIANCE" "lock" "MF-APPLIANCE"
  "t" "MF-CONC" "m" "MF-CONC" "c" "255" "MF-CONC" "l" "HIDDEN2" "MF-CONC" "lw" "0.05" "MF-CONC"
  "t" "MF-DECK" "m" "MF-DECK" "c" "255" "MF-DECK" "l" "HIDDEN2" "MF-DECK" "lw" "0.05" "MF-DECK"
  "t" "MF-WALLS" "m" "MF-WALLS" "c" "255" "MF-WALLS" "l" "HIDDEN2" "MF-WALLS" "lw" "0.05" "MF-WALLS" "lock" "MF-WALLS"
  "t" "MF-WALLS-NEW" "m" "MF-WALLS-NEW" "c" "62" "MF-WALLS-NEW" "l" "CONTINUOUS" "MF-WALLS-NEW" "lw" "0.5" "MF-WALLS-NEW" "lock" "MF-WALLS-NEW"
  "t" "MF-WALLS-REMOVED" "m" "MF-WALLS-REMOVED" "c" "254" "MF-WALLS-REMOVED" "l" "HIDDEN2" "MF-WALLS-REMOVED" "lw" "0.09" "MF-WALLS-REMOVED" "lock" "MF-WALLS-REMOVED"
  "" "REGEN")

  ;restore current layer
  (setvar "clayer" SET-OLD-LAYER)
  (princ)
)

;add upper floor
(defun c:auf()
  ;save current layer
  (setq SET-OLD-LAYER (getvar "clayer"))
  (LA:commonLayerPre acTrue acFalse)
 ;Modify layer properties

 (command "_.-layer"
  "t" "UF-APPLIANCE" "m" "UF-APPLIANCE" "c" "255" "UF-APPLIANCE" "l" "HIDDEN2" "UF-APPLIANCE" "lw" "0.05" "UF-APPLIANCE" "lock" "UF-APPLIANCE"
  "t" "UF-CONC" "m" "UF-CONC" "c" "255" "UF-CONC" "l" "HIDDEN2" "UF-CONC" "lw" "0.05" "UF-CONC"
  "t" "UF-DECK" "m" "UF-DECK" "c" "255" "UF-DECK" "l" "HIDDEN2" "UF-DECK" "lw" "0.05" "UF-DECK"
  "t" "UF-WALLS" "m" "UF-WALLS" "c" "255" "UF-WALLS" "l" "HIDDEN2" "UF-WALLS" "lw" "0.05" "UF-WALLS" "lock" "UF-WALLS"
  "t" "UF-WALLS-NEW" "m" "UF-WALLS-NEW" "c" "62" "UF-WALLS-NEW" "l" "continuous" "UF-WALLS-NEW" "lw" "0.5" "UF-WALLS-NEW" "lock" "UF-WALLS-NEW"

  "" "REGEN")

  ;restore current layer
    (setvar "clayer" SET-OLD-LAYER)
  (princ)
)

;add basement floor
(defun c:abf()
  ;save current layer
  (setq SET-OLD-LAYER (getvar "clayer"))
  (LA:commonLayerPre acTrue acFalse)
 ;Modify layer properties

 (command "_.-layer"
  "t" "BF-APPLIANCE" "m" "BF-APPLIANCE" "c" "255" "BF-APPLIANCE" "l" "HIDDEN2" "BF-APPLIANCE" "lw" "0.05" "BF-APPLIANCE"
  "t" "BF-CONC" "m" "BF-CONC" "c" "255" "BF-CONC" "l" "HIDDEN2" "BF-CONC" "lw" "0.05" "BF-CONC"
  "t" "BF-OPENING" "m" "BF-OPENING" "c" "255" "BF-OPENING" "l" "HIDDEN2" "BF-OPENING" "lw" "0.05" "BF-OPENING"
  "t" "BF-STAIR" "m" "BF-STAIR" "c" "255" "BF-STAIR" "l" "HIDDEN2" "BF-STAIR" "lw" "0.05" "BF-STAIR"
  "t" "BF-WALLS" "m" "BF-WALLS" "c" "255" "BF-WALLS" "l" "HIDDEN2" "BF-WALLS" "lw" "0.05" "BF-WALLS"
  "t" "FN-FOOTING" "m" "FN-FOOTING" "c" "255" "FN-FOOTING" "l" "HIDDEN2" "FN-FOOTING" "lw" "0.05" "FN-FOOTING"
  "t" "FN-OVERLAY" "m" "FN-OVERLAY" "c" "255" "FN-OVERLAY" "l" "HIDDEN2" "FN-OVERLAY" "lw" "0.05" "FN-OVERLAY"
  "t" "FN-POST" "m" "FN-POST" "c" "255" "FN-POST" "l" "HIDDEN2" "FN-POST" "lw" "0.05" "FN-POST"
  "t" "FN-VENT" "m" "FN-VENT" "c" "255" "FN-VENT" "l" "HIDDEN2" "FN-VENT" "lw" "0.05" "FN-VENT"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "255" "FN-WALLS" "l" "HIDDEN2" "FN-WALLS" "lw" "0.05" "FN-WALLS"
  "t" "BF-WALLS-NEW" "m" "BF-WALLS-NEW" "c" "62" "BF-WALLS-NEW" "l" "CONTINUOUS" "BF-WALLS-NEW" "lw" "0.5" "BF-WALLS-NEW" "unlock" "BF-WALLS-NEW"

  "" "REGEN")

  ;restore current layer
    (setvar "clayer" SET-OLD-LAYER)
  (princ)
)

;add foundation
(defun c:afn()
  ;save current layer
  (setq SET-OLD-LAYER (getvar "clayer"))
  (LA:commonLayerPre acTrue acFalse)
 ;Modify layer properties

 (command "_.-layer"
  "t" "FN-FOOTING" "m" "FN-FOOTING" "c" "255" "FN-FOOTING" "l" "HIDDEN2" "FN-FOOTING" "lw" "0.05" "FN-FOOTING"
  "t" "FN-OVERLAY" "m" "FN-OVERLAY" "c" "255" "FN-OVERLAY" "l" "HIDDEN2" "FN-OVERLAY" "lw" "0.05" "FN-OVERLAY"
  "t" "FN-POST" "m" "FN-POST" "c" "255" "FN-POST" "l" "HIDDEN2" "FN-POST" "lw" "0.05" "FN-POST"
  "t" "FN-VENT" "m" "FN-VENT" "c" "255" "FN-VENT" "l" "HIDDEN2" "FN-VENT" "lw" "0.05" "FN-VENT"
  "t" "FN-WALLS" "m" "FN-WALLS" "c" "255" "FN-WALLS" "l" "HIDDEN2" "FN-WALLS" "lw" "0.05" "FN-WALLS"

  "" "REGEN")

  ;restore current layer
    (setvar "clayer" SET-OLD-LAYER) 
  (princ)
)

;add main floor framing
(defun c:amff()
  ;save current layer
  (setq SET-OLD-LAYER (getvar "clayer"))
  (LA:commonLayerPre acTrue acFalse)
 ;Modify layer properties

 (command "_.-layer"
  "t" "MFF-ENGINEERING" "m" "MFF-ENGINEERING" "c" "255" "MFF-ENGINEERING" "l" "HIDDEN2" "MFF-ENGINEERING" "lw" "0.05" "MFF-ENGINEERING"
  "t" "MFF-BEAM" "m" "MFF-BEAM" "c" "255" "MFF-BEAM" "l" "HIDDEN2" "MFF-BEAM" "lw" "0.05" "MFF-BEAM"
  "t" "MFF-FRAMING" "m" "MFF-FRAMING" "c" "255" "MFF-FRAMING" "l" "HIDDEN2" "MFF-FRAMING" "lw" "0.05" "MFF-FRAMING"

  "" "REGEN")

  ;restore current layer
    (setvar "clayer" SET-OLD-LAYER)
  (princ)
)

;add upper floor framing
(defun c:auff()
  ;save current layer
  (setq SET-OLD-LAYER (getvar "clayer"))
  (LA:commonLayerPre acTrue acFalse)
 ;Modify layer properties

 (command "_.-layer"
  "t" "UFF-ENGINEERING" "m" "UFF-ENGINEERING" "c" "255" "UFF-ENGINEERING" "l" "HIDDEN2" "UFF-ENGINEERING" "lw" "0.05" "UFF-ENGINEERING"
  "t" "UFF-BEAM" "m" "UFF-BEAM" "c" "255" "UFF-BEAM" "l" "HIDDEN2" "UFF-BEAM" "lw" "0.05" "UFF-BEAM"
  "t" "UFF-FRAMING" "m" "UFF-FRAMING" "c" "255" "UFF-FRAMING" "l" "HIDDEN2" "UFF-FRAMING" "lw" "0.05" "UFF-FRAMING"

  "" "REGEN")

  ;restore current layer
    (setvar "clayer" SET-OLD-LAYER)
  (princ)
)

;add roof
(defun c:arf()
  ;save current layer
  (setq SET-OLD-LAYER (getvar "clayer"))

  (LA:commonLayerPre acTrue acFalse)
 ;Modify layer properties

 (command "_.-layer"
  "t" "RF-BEAM" "m" "RF-BEAM" "c" "255" "RF-BEAM" "l" "HIDDEN2" "RF-BEAM" "lw" "0.05" "RF-BEAM" 
  "t" "RF-DS" "m" "RF-DS" "c" "255" "RF-DS" "l" "HIDDEN2" "RF-DS" "lw" "0.05" "RF-DS"
  "t" "RF-ENGINEERING" "m" "RF-ENGINEERING" "c" "255" "RF-ENGINEERING" "l" "HIDDEN2" "RF-ENGINEERING" "lw" "0.05" "RF-ENGINEERING"
  "t" "RF-FRAMING" "m" "RF-FRAMING" "c" "255" "RF-FRAMING" "l" "HIDDEN2" "RF-FRAMING" "lw" "0.05" "RF-FRAMING"
  "t" "RF-GUTTER" "m" "RF-GUTTER" "c" "255" "RF-GUTTER" "l" "HIDDEN2" "RF-GUTTER" "lw" "0.05" "RF-GUTTER"
  "t" "RF-OVERFRAME" "m" "RF-OVERFRAME" "c" "255" "RF-OVERFRAME" "l" "HIDDEN2" "RF-OVERFRAME" "lw" "0.05" "RF-OVERFRAME"
  "t" "RF-PROFILE" "m" "RF-PROFILE" "c" "255" "RF-PROFILE" "l" "HIDDEN2" "RF-PROFILE" "lw" "0.05" "RF-PROFILE"

  "" "REGEN")

  ;restore current layer
    (setvar "clayer" SET-OLD-LAYER) 
  (princ)
)

(defun c:af(/ *error* main doc)
	(vl-load-com)
	(defun main ()
		(vla-StartUndoMark (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))
		(command "_.-layer" "t" (strcat (substr (getvar "CLAYER") 1 2) "-FURN") "")
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

(defun c:forceoverride()
  (command "_.-layer"
  "t" "*" "c" "249" "*" "l" "EXCA" "*" "lw" "2.11" "*" 
  "" "REGEN")
)