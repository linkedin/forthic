;;==============================================================================
;; File:            DictionaryMacros.inc
;; Created date:    11/21/2017
;; Last update:     11/28/2017
;; Author:          Rino Jose
;; Description:     Defines macros for manipulating dictionary entries.
;;


; Convenience constants
ZERO_PARAMS   EQU 0
ONE_PARAM     EQU 1

NOT_IMMEDIATE EQU 0
IMMEDIATE     EQU 1



;-------------------------------------------------------------------------------
; CopyWord: MACRO
;
; Last update:  11/24/2017
; Description:  Fills EntryWord at |dest| with spaces and copies
;               string in esi to |dest|
;;
; Input:        esi contains pointer to string to copy
;
; Modifies:     ecx, al, esi, edi
;
CopyWord MACRO dest
; Pad dest with BUFFERLEN spaces
		mov ecx, MAXWORDLEN ; ecx is WordEntry size
		mov al, ' '         ; Will pad with space char
		mov edi, dest       ; edi = destination
		rep stosb           ; Pad with spaces

; Copy bytes
		mov edi, dest       ; edi = destination
@@:
		mov cl, [esi]       ; cl = *esi
		cmp cl, 0           ; cl == 0?
		je @F               ; If so, we're done
		movsb               ; Copy byte
		jmp @B              ; Loop
@@:
ENDM


;-------------------------------------------------------------------------------
; SetEntryWord: MACRO
;
; Last update:  11/24/2017
; Description:  Fills EntryWord at DP_ (next free entry) with spaces and copies
;               string in esi to DP_
;
; Assumptions:  DP_ is pointing to the beginning of an uninitialized Entry
;
; Input:        esi contains pointer to string to copy
;
; Modifies:     ecx, al, esi, edi
;
SetEntryWord MACRO
	CopyWord DP_
ENDM


;-------------------------------------------------------------------------------
; SetRoutine: MACRO
;
; Last update:  11/21/2017
; Description:  Sets Routine of current entry.
;
; Assumptions:  DP_ is pointing to the beginning of an uninitialized Entry
;
; Input:        esi contains pointer to routine
;
; Modifies:     edi
;
SetRoutine MACRO
		mov edi, DP_            ; ecx = entry
		add edi, Entry.Routine  ; edi = entry.Routine
		mov [edi], esi          ; entry.Routine = routine
ENDM

;-------------------------------------------------------------------------------
; SetPrevious: MACRO
;
; Last update:  11/21/2017
; Description:  Sets Previous of current entry to be LastEntry_
;
; Assumptions:  DP_ is pointing to the beginning of an uninitialized Entry
;
; Modifies:     esi, edi
;
SetPrevious MACRO
		mov edi, DP_            ; ecx = entry
		add edi, Entry.Previous ; edi = entry.Routine
		mov esi, LastEntry_     ; esi = LastEntry_
		mov [edi], esi          ; entry.Previous = LastEntry_
ENDM


;-------------------------------------------------------------------------------
; SetImmediate: MACRO
;
; Last update:  11/25/2017
; Description:  Sets Immediate of current entry.
;
; Assumptions:  DP_ is pointing to the beginning of an uninitialized Entry
;
; Modifies:     edi, eax
;
SetImmediate MACRO val
		mov edi, DP_             ; ecx = entry
		add edi, Entry.Immediate ; edi = entry.Immediate
		mov al, val              ; al = val
		mov [edi], al            ; entry.Immediate = val
ENDM



;-------------------------------------------------------------------------------
; CompleteEntry: MACRO
;
; Last update:  11/21/2017
; Description:  Sets LastEntry_ to be the latest entry and updates DP_ to
;               point past the parameters of the entry being added. The
;               number of parameters is in ecx
;
; Assumptions:  DP_ is pointing to the beginning of an uninitialized Entry
;
; Input:        ecx contains the number of parameters (double words) for this entry
;
; Modifies:     esi, edi
;
CompleteEntry MACRO
;; Set LastEntry_ and Advance DP_
		mov edi, DP_             ; edi = original DP_
		mov [LastEntry_], edi    ; LastEntry_ = newly created entry

		mov edi, DP_             ; edi = entry
		add edi, Entry.Parameter ; edi = entry.parameter
		imul ecx, 4              ; ecx = number of param bytes
		add edi, ecx             ; edi = next free entry
		mov DP_, edi             ; DP_ = next free entry
ENDM


;-------------------------------------------------------------------------------
; NewEntry: MACRO
;
; Last update:  11/28/2017
; Description:  Creates a new dictionary entry
;
; Assumptions:  DP_ is pointing to the beginning of an uninitialized Entry
;
; Input:        Entry word label
;               Entry routine
;               num entry parameters
;               is immediate
;
; Modifies:     esi, edi, ecx
;
NewEntry MACRO entry_word, routine, num_params, is_immediate
		mov esi, offset entry_word       ; esi = &entry_word
		SetEntryWord                     ; Copy entry_word to current entry's EntryWord
		mov esi, offset routine          ; esi = &routine
		SetRoutine                       ; current_entry.Routine = &routine
		SetPrevious                      ; current_entry.Previous = LastEntry_
		SetImmediate is_immediate        ; current_entry.Immediate = is_immediate
		mov ecx, num_params              ; num_parameters = ecx = num params
		CompleteEntry                    ; Updates LastEntry_ and DP_ for entry
ENDM
