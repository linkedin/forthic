;;==============================================================================
;; File:            Literals.asm
;; Created date:    11/22/2017
;; Last update:     11/28/2017
;; Author:          Rino Jose
;; Description:     Contains subroutines for checking to see if WordBuffer_
;;                  can be treated as a literal.
;;

.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc
include Entry.inc

extern WordBuffer_:byte
extern SP_:dword
extern NOPEntry_:Entry
extern Compiling_:byte
extern DP_:dword
extern IP_:dword

sscanf_s PROTO C string:NEAR PTR BYTE,  :VARARG


;;==============================================================================
;; PUBLIC
public FindLiteralEntry_
public PushParam0PseudoEntry_

;;==============================================================================
;; DATA
.data
	IntValue                           dd ?
	IntegerFormat                      db "%d"
	PushParam0PseudoEntry_            Entry { }

;;==============================================================================
;; CODE
.code

;-------------------------------------------------------------------------------
; FindLiteralEntry_:  Tries to find a literal pseudo entry for word in WordBuffer_
;
; Last update:  11/24/2017
; Description:  If the word in WordBuffer_ can be treated as a literal (e.g,
;               a number), this returns a pseudo entry that pushes the literal
;               onto the stack in binary format.
;
; Returns:      EAX has address of PseudoEntry to execute or 0 if not a literal
;
FindLiteralEntry_ proc
; Initialize a stack frame pointer
        push ebp
        mov ebp,esp
		push esi
		push edi
		push ebx

; Call sscanf on WordBuffer_
		push offset IntValue        ; arg3 = Address of destination value
		push offset IntegerFormat   ; arg2 = Address of format string
		push offset WordBuffer_     ; arg1 = Address of buffer to parse
		call sscanf_s               ; sscanf_s(WordBuffer_, IntegerFormat, IntValue)
		add esp, 12                 ; Clean up stack

; If couldn't parse an integer, return NULL
		cmp eax, 0                  ; Check parse result
		mov eax, 0                  ; result = NULL
		jle Exit                    ; unsuccessful parse (0 is no fields parsed, -1 means error)

; Check if compiling
		mov bl, Compiling_          ; bl = Compiling_
		test bl, bl                 ; if compiling, 
		jnz Compile                 ; Compile

; If not compiling, push value onto parameter stack and return NOPEntry_
		mov esi, IntValue           ; esi = IntValue
		mov edi, SP_                ; edi = &SP_
		mov [edi], esi              ; *SP_ = Intvalue
		sub SP_, 4                  ; Advance SP_
		jmp Exit

; If compiling, compile a PushParam0PseudoEntry_ followed by the value
Compile:
		mov edi, DP_           ; edi = DP_
		mov [edi], offset PushParam0PseudoEntry_ ; DP_ = &PushParam0PseudoEntry_
		add edi, 4             ; edi = DP_ + 4  (next parameter slot)
		mov esi, IntValue      ; esi = IntValue
		mov [edi], esi         ; Store IntValue in next parameter slot
		add edi, 4             ; edi = DP_ + 4 (next next param slot)
		mov DP_, edi           ; DP_ = next param slot

; Return NOP since we've already done the work
		mov eax, offset NOPEntry_    ; (result ) eax = NOPEntry_

Exit:
; Restore the caller's stack frame pointer
		pop ebx
		pop edi
		pop esi
        pop ebp
        ret
FindLiteralEntry_ endp


;-------------------------------------------------------------------------------
; PushIntegerPseudoRoutine_:  Push value in next parameter onto stack
;
; Last update:  11/26/2017
; Description:  This is used when compiling a literal into a definition.
;
; inputs:       eax = &entry
;               IP_ = &value
;
; Returns:      Nothing
;
PushIntegerPseudoRoutine_ proc
; Initialize a stack frame pointer
        push ebp
        mov ebp,esp
		push esi
		push edi
		push ebx

; Push value onto stack
		mov ecx, IP_                ; ecx = IP_
		mov esi, [ecx]              ; esi = value
		mov edi, SP_                ; edi = &SP_
		mov [edi], esi              ; *SP_ = Intvalue
		sub SP_, 4                  ; Advance SP_

; Advance IP_
		add IP_, 4                  ; IP_ += 4

; Restore the caller's stack frame pointer
		pop ebx
		pop edi
		pop esi
        pop ebp
        ret
PushIntegerPseudoRoutine_ endp

end