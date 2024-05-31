;;==============================================================================
;; File:            Dictionary.asm
;; Created date:    11/20/2017
;; Last update:     11/28/2017
;; Author:          Rino Jose
;; Description:     Contains subroutines for searching and managing dictionary
;;
;;                  The dictionary contains a contiguous block of entries. The
;;                  first entry is a special entry with all fields = 0, in
;;                  particular the Previous link.
;;
;;                  LastEntry_ points to the most recent entry. After initialization,
;;                  it should always be valid.
.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc
include Entry.inc


extern WordBuffer_:byte
extern LastEntry_:dword
extern Dictionary_:Entry

;;==============================================================================
;; PUBLIC
public FindEntry_


;;==============================================================================
;; CODE
.code

;-------------------------------------------------------------------------------
; FindEntry_: Searches Dictionary_ for entry with word matching WordBuffer_
;
; Last update:  11/20/2017
; Description:  Searches Dictionary_ backwards starting from LastEntry_ looking for an
;               entry matching the word WordBuffer_.
;
;               If an entry is found, return its address; if not, then return 0.
;
; Returns:      EAX has address of Entry to execute or 0 if no next instruction
;
FindEntry_ proc
; Initialize a stack frame pointer
        push ebp
        mov ebp,esp
		push esi
		push edi
		push ebx

; Start at LastEntry_
		mov ecx, LastEntry_    ; ecx = cur_entry

		mov eax, [ecx]     ; Get first 4 chars of current entry.
		test eax, eax      ; If chars are 0, then dictionary is uninitialized so
		cmovz ecx, eax     ; set ecx (cur entry) to NULL

@@:
; If cur_entry is 0, return NULL
		xor ebx, ebx       ; result = NULL
		test ecx, ecx      ; If cur_entry is NULL, exit
		jz Exit

; Match word
		mov ebx, ecx       ; ebx = ecx = current entry
		call MatchWord     ; Match word
        mov ecx, [ebx+Entry.Previous]  ; ecx = previous entry
		test eax, eax      ; If no match, check previous entry
		jz @B

Exit:
		mov eax, ebx     ; eax = result

; Restore the caller's stack frame pointer
		pop ebx
		pop edi
		pop esi
        pop ebp
        ret
FindEntry_ endp


;-------------------------------------------------------------------------------
; MatchWord: Returns 1 if WordBuffer_ matches word pointed to in ecx
;
; Last update:  11/20/2017
; Description:  Matches first 32 chars of WordBuffer_ and string pointed to in ecx
;
; NOTE: We assume that WordBuffer_ and *ecx are space padded
;
; Input:        ECX points to a string to compare
;
; Returns:      EAX is 1 if strings match; 0 otherwise
;
MatchWord proc
; Initialize a stack frame pointer
        push ebp
        mov ebp,esp
		push esi
		push edi
		push ebx

        mov eax, 0                          ; result = 0
        mov esi, offset WordBuffer_         ; esi = WordBuffer_
        mov edi, ecx                        ; edi = string to check
        mov ecx, 8                          ; Match 32 characters (8 double words)

; Compare the arrays for equality
        repe cmpsd
        jne Exit                            ; if strings are different

		mov eax, 1

Exit:

; Restore the caller's stack frame pointer
		pop ebx
		pop edi
		pop esi
        pop ebp
        ret
MatchWord endp

end