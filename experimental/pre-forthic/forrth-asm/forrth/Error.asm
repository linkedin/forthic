;;==============================================================================
;; File:            Error.asm
;; Created date:    11/25/2017
;; Last update:     11/28/2017
;; Author:          Rino Jose
;; Description:     Contains subroutines to report and handle errors.
;;
.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc

printf PROTO C :VARARG

extern SPStart_:dword
extern SP_:dword
extern GetNextEntry_:dword
extern Next_W_:near
extern RSP_:dword
extern RSPStart_:dword
extern IP_:dword
extern Compiling_:byte
extern CSPStart_:dword
extern CSP_:dword
extern MessageBuffer_:byte
extern MessageBufferPos_:dword

;;==============================================================================
;; PUBLIC
public UnknownWordError_

;;==============================================================================
;; DATA
.data
	UNKNOWN_WORD_FMT         db "Unknown word: %s", 10, 0

;;==============================================================================
;; CODE
.code

;-------------------------------------------------------------------------------
; UnknownWordError
;
; Last update:  11/25/2017
; Description:  Prints an error for an unknown word and resets interpreter state
;
; Input:  ecx contains a pointer to the unknown word
;
; Returns:      Nothing
;
UnknownWordError_ proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi
		push eax

; printf("Unknown word: %s", word);
		push ecx                 ; arg2 = word
		push offset UNKNOWN_WORD_FMT  ; arg1 = format
		call printf              ; printf(HI)
		add esp, 8               ; Clear stack

; Reset interpreter state
		call ResetInterpreterState

Exit:
		pop eax
		pop edi
		pop esi
		pop ebp
		ret
UnknownWordError_ endp


;-------------------------------------------------------------------------------
; ResetInterpreterState
;
; Last update:  11/25/2017
; Description:  Resets the parameter stack, return stack, instruction pointer, GetNextEntry func ptr.
;
; Returns:      Nothing
;
ResetInterpreterState proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Reset parameter stack
		mov esi, SPStart_      ; esi = SPStart_
		mov SP_, esi           ; SP_ = SPStart_

; Reset return stack
		mov esi, RSPStart_     ; esi = RSPStart_
		mov RSP_, esi          ; SP_ = RSPStart_

; Reset instruction pointer
		mov IP_, 0

; Reset GetNextWord
		mov edi, offset GetNextEntry_  ; edi = &GetNextEntry_
		mov [edi], Next_W_             ; GetNextEntry_ = Next_W_

; Reset code stack
		mov esi, CSPStart_       ; esi = CSPStart_
		mov SP_, esi             ; SP_ = start of stack

; Reset MessageBufferPos
		mov esi, offset MessageBuffer_  ; esi = &MessageBuffer_
		mov MessageBufferPos_, esi      ; MessageBufferPos_ = start of MessageBuffer
		mov al, 0                 ; al = \NUL
		mov [esi], al             ; Set first char of MessageBuffer to \NUL

; Reset Compiling
		mov al, 0                ; al = false
		mov Compiling_, al       ; Compiling_ = false

Exit:
		pop edi
		pop esi
		pop ebp
		ret
ResetInterpreterState endp

end