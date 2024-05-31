;;==============================================================================
;; File:            Engine.asm
;; Created date:    11/19/2017
;; Last update:     11/27/2017
;; Author:          Rino Jose
;; Description:     Contains subroutines for parsing the MessageBuffer_ and
;;                  initiating the execution of instructions
.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc
include Entry.inc

extern MessageBuffer_:byte
extern GetNextEntry_:dword
extern DP_:dword
extern FindEntry_:near
extern FindLiteralEntry_:near
extern WordBuffer_:byte
extern UnknownWordError_:near
extern MessageBufferPos_:dword
extern Compiling_:byte
extern CSP_:dword
extern CSPStart_:dword

;;==============================================================================
;; PUBLIC
public Next_W_
public GetNextWord_


;;==============================================================================
;; CODE
.code

;-------------------------------------------------------------------------------
; extern "C" void Execute_()
;
; Last update:  11/27/2017
; Description:  Stores input in MessageBuffer_ and then parses and executes them
;               until the first \0 char is found.
;
; Input:        arg1: src string
;               arg2: num chars
;
; Returns:      Nothing
;
Execute_ proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi
		push ebx

		; Store input
		mov eax, [ebp+8]    ; eax = src
		mov edx, [ebp+12]   ; edx = num chars
		call StoreInput     ; Store input in MessageBuffer_

		; Process input
		call ProcessInput

; Restore the caller's stack frame pointer
Exit:
		pop ebx
		pop edi
		pop esi
		pop ebp
		ret
Execute_ endp


;-------------------------------------------------------------------------------
; StoreInput
;
; Last update:  11/27/2017
; Description:  This function copies up to BUFFERLEN chars from src to MessageBuffer_
;
; Input:        eax: src string
;               edx: num chars
;
; Returns:      Nothing
StoreInput proc

; Initialize a stack frame pointer
        push ebp
        mov ebp,esp
		push esi
		push edi

;; Copy num chars and assume MessageBuffer_ can handle it
		mov ecx, edx       ; ecx = num chars
		mov esi, eax       ; esi = src
		mov edi, offset MessageBuffer_  ; edi = buffer
		rep movsb

;; NUL terminate string
		mov eax, 0
		mov [edi], eax

; Restore the caller's stack frame pointer
		pop edi
		pop esi
        pop ebp
        ret

StoreInput endp


;-------------------------------------------------------------------------------
; ProcessInput
;
; Last update:  11/26/2017
; Description:  Parses words starting at the beginning of MessageBuffer_ and
;               initiaties their execution. Returns when the first \0 char
;               is found.
;
; Returns:      Nothing
;
ProcessInput proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi
		push ebx

; When starting to process input, get next instruction from MessageBuffer_
; using Next_W_.
		mov edx, offset Next_W_               ; edx = Next_W_
		mov [GetNextEntry_], edx              ; GetNextEntry_ = Next_W_

; Set MessageBufferPos_ to start of MessageBuffer_
		mov edx, offset MessageBuffer_       ; edx = &MessageBuffer_
		mov [MessageBufferPos_], edx         ; MessageBufferPos_ = &MessageBuffer_

; Loop until there are no more instructions to execute
@@:
		call GetNextEntry_    ; eax = &entry
		test eax, eax         ; If no instruction,
		jz PopCodeStack       ; pop code stack and continue

		; TODO: Simplify the logic
		; If compiling && !entry.immediate, compile entry
		mov ecx, eax          ; ecx = &entry
		add ecx, Entry.Immediate  ; ecx = &entry.Immediate
		mov bh, [ecx]         ; bh = Immediate
		not bh                ; bh = !Immediate
		mov bl, Compiling_    ; bl = Compiling_
		and bh, bl            ; compiling && !entry.immediate
		jnz Compile           ; if compiling && !entry.immediate, compile

		; Execute instruction
		mov ebx, eax           ; ebx = &entry
		add ebx, Entry.Routine ; ebx = &entry.routine
		mov ecx, [ebx]         ; ecx = entry.routine
		call ecx               ; execute!
		jmp @B

Compile:
		mov edi, DP_           ; edi = DP_
		mov [edi], eax         ; DP_ = &entry
		add edi, 4             ; edi = DP_ + 4
		mov DP_, edi           ; DP_ = next param slot
		jmp @B

PopCodeStack:
; If code stack is empty, exit
		mov esi, CSP_          ; esi = code stack pointer
		mov edi, CSPStart_     ; edi = start of code stack
		cmp esi, edi
		je Exit                ; If CSP_ == CSPStart_, nothing left to do

; Pop code stack into MessageBuffer_ and continue
		add esi, 4             ; esi = CSP_ + 4
		mov edi, [esi]         ; edi = previous MessageBufferPos_
		mov MessageBufferPos_, edi  ; Restore previous MessageBufferPos_
		mov CSP_, esi          ; CSP_ = CSP_ + 4
		jmp @B

; Restore the caller's stack frame pointer
Exit:
		pop ebx
		pop edi
		pop esi
		pop ebp
		ret
ProcessInput endp


;-------------------------------------------------------------------------------
; Next_W_: Returns next instruction based on "next word" in MessageBuffer_
;
; Last update:  11/20/2017
; Description:  If there are no more characters in the MessageBuffer_, return 0.
;
;               Otherwise, parse next word out of MessageBuffer_ and attempt to
;               find an entry/pseudo entry for the word.
;
;               If an entry is found, return its address; if not, then return 0.
;
; Returns:      EAX has address of Entry to execute or 0 if no next instruction
;
Next_W_ proc
; Initialize a stack frame pointer
        push ebp
        mov ebp,esp
		push esi
		push edi

; Get next word from MessageBuffer_
		call GetNextWord_            ; Copy next word into WordBuffer_
		test eax, eax               ; If num chars copied is 0, exit
		jz Exit                     ; NOTE: The return value is correctly 0


; Look up word
		call FindEntry_             ; Try to find WordBuffer_ in dictionary
		test eax, eax               ; If not 0, return result
		jnz Exit

; If no entry, try to treat as literal
		call FindLiteralEntry_      ; Try to treat WordBuffer_ as a literal
		test eax, eax               ; If not 0, return result
		jnz Exit

; If cannot treat as literal, call error
		mov ecx, offset WordBuffer_  ; ecx = WordBuffer_
		Call UnknownWordError_

; Restore the caller's stack frame pointer
Exit:
		pop edi
		pop esi
        pop ebp
        ret
Next_W_ endp


;-------------------------------------------------------------------------------
; GetNextWord_:  Copies next word in MessageBuffer_ to WordBuffer_, returning num chars copied
;
; Last update:  11/20/2017
; Description:  If there are no more characters in the MessageBuffer_, return 0.
;
;               Otherwise, parse next word out of MessageBuffer_ into WordBuffer_
;               padding with spaces.
;
; Returns:      EAX has the number of characters copied
;
GetNextWord_ proc
; Prolog
        push ebp
        mov ebp,esp
		push esi
		push edi
		push ebx

		xor ecx, ecx                ; ecx = 0 (num chars)

; Skip spaces
		mov esi, MessageBufferPos_  ; esi is one buffer the current char position
		dec esi
@@:
		inc esi                    ; esi++
		mov dl, [esi]              ; edx = cur_char
		cmp dl, 0                  ; if cur_char == 0, return
		je Exit

		cmp dl, ' '                ; if cur_char == ' ', loop
		je @B


; If next char is \0, return 0
		cmp dl, 0                  ; if cur_char == 0, return
		je Exit

; Copy non-space characters to word buffer
		mov edi, offset WordBuffer_  ; edi = start WordBuffer_
@@:
		movsb                      ; Copy char from MessageBuffer_ to WordBuffer_ and advance
		inc ecx                    ; num_chars++
		mov dl, [esi]

		cmp dl, 0                  ; if cur_char == 0, return
		je Exit

		cmp dl, ' '                ; if cur_char == ' ', return
		je Exit
		jmp @B

Exit:
; Pad WordBuffer_ with spaces
		mov ebx, ecx               ; ebx = num chars copied
		test ebx, ebx
		jz Return

		mov MessageBufferPos_, esi  ; Store current message buffer pos
		sub ecx, MAXWORDLEN-1         ; ecx = -(num chars to pad)
		neg ecx                    ; ecx = num chars to pad
		mov al, ' '
		rep stosb                  ; Pad WordBuffer_ with ' '

Return:
; Prepare result
		mov eax, ebx               ; eax = num chars copied

; Epilog
        pop ebx
		pop edi
		pop esi
        pop ebp
        ret
GetNextWord_ endp

end