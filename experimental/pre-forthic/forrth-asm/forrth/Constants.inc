;; File           : Constants_.inc
;; Created date   : 11/19/2017
;; Last update    : 11/24/2017
;; Author         : Rino Jose
;; Description    : Defines constants for Forrth interpreter

RETSTACKLEN  EQU 32
STACKLEN EQU 32        ; Length of parameter stack

MAXWORDLEN EQU 63      ; Max word length

BUFFERLEN EQU 1024*10  ; Length of MessageBuffer_

NUM_ENTRIES EQU 512    ; Number of entries in the Dictionary_

AVG_ENTRY_SIZE EQU 48  ; This is in double words. It assumes that EntryWord is 32
                       ; double words and we have roughly 10 parameters

DICTIONARY_SIZE EQU NUM_ENTRIES * AVG_ENTRY_SIZE   ; double words

DICTIONARY_STRING_SIZE EQU 1024*100   ; 100K

EOF EQU -1             ; Checked EOF char in C++