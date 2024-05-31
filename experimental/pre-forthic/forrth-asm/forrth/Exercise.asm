;;==============================================================================
;; File:            ProcessInput_.asm
;; Created date:    11/19/2017
;; Last update:     11/26/2017
;; Author:          Rino Jose
;; Description:     Contains subroutines for parsing the MessageBuffer_ and
;;                  initiating the execution of instructions
.model flat,c

.data

	ReturnStack_                     dd 32 DUP(0)
	RSP_                             dd ?
	RSPStart_                        dd ?



.code

;-------------------------------------------------------------------------------
; extern "C" void ProcessInput_()
;
; Last update:  11/26/2017
; Description:  Parses words starting at the beginning of MessageBuffer_ and
;               initiaties their execution. Returns when the first \0 char
;               is found.
;
; Returns:      Nothing
;
Exercise_ proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi
		push ebx

; Check addresses
		mov edx, offset ReturnStack_
		mov RSP_, offset ReturnStack_    ; You can store a value directly at a memory location
		add RSP_, 31*4
		mov eax, RSP_
		mov RSPStart_, eax
		mov edi, RSP_                    ; Get pointer to start of stack

		; This shows how to dereference a pointer and store a value in it
		mov eax, 5
		mov [edi], eax                   ; ReturnStack_ has a 5

		; This advances a pointer and stores another value
		sub RSP_, 4
		mov eax, 6
		mov edi, RSP_                    ; Get pointer to start of stack
		mov [edi], eax                   ; ReturnStack_ + 4 has a 6

		; This advances a pointer and stores bytes of stack space used
		sub RSP_, 4
		mov eax, RSPStart_
		sub eax, RSP_
		mov edi, RSP_                    ; Get pointer to start of stack
		mov [edi], eax                   ; ReturnStack_ + 4 has a 6


; Restore the caller's stack frame pointer
Exit:
		pop ebx
		pop edi
		pop esi
		pop ebp
		ret
Exercise_ endp

end