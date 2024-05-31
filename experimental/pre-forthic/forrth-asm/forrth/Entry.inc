;;==============================================================================
;; File           : Entry.inc
;; Created date   : 11/20/2017
;; Last update    : 11/25/2017
;; Author         : Rino Jose
;; Description    : Defines Entry structure

;-------------------------------------------------------------------------------
; Entry:        Holds a variable length dictionary entry
;
; Last update:  11/20/2017
; Description:  A dictionary entry has 4 fields:
;                   1. The name of the word being executed
;                   2. The code to be executed
;                   3. A link to the previous entry
;                   4. Parameters (variable length)
;
Entry struct
	EntryWord    db (MAXWORDLEN+1) DUP(?)
	Routine      dd ?
	Previous     dd ?
	Immediate    db 0
	Parameter    dd ?
Entry ends