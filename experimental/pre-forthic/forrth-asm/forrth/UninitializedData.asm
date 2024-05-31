;;==============================================================================
;; File           : UninitializedData.asm
;; Created date   : 12/02/2017
;; Last update    : 12/02/2017
;; Author         : Rino Jose
;; Description    : Declares uninitialized data
;;

.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc

;;==============================================================================
;; PUBLIC
public DictionaryStrings_


;;==============================================================================
;; DATA
.data?
	DictionaryStrings_   db DICTIONARY_STRING_SIZE DUP(?)

end