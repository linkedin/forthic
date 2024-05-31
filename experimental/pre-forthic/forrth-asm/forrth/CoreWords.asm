;;==============================================================================
;; File:            CoreWords.asm
;; Created date:    11/21/2017
;; Last update:     12/02/2017
;; Author:          Rino Jose
;; Description:     Adds basic lexicon to Dictionary_
;;
.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc
include Entry.inc
include DictionaryMacros.inc

printf PROTO C :VARARG
LoadForrthBlock PROTO C forrth_id:NEAR PTR BYTE, dest:NEAR PTR BYTE


extern DP_:dword
extern LastEntry_:dword
extern AddDefinitionWords_:near
extern MessageBufferPos_:dword
extern SP_:dword
extern Compiling_:byte
extern DictionaryStringPos_:dword
extern PushParam0PseudoEntry_:Entry
extern CSP_:dword
extern GetNextWord_:near
extern WordBuffer_:byte

;;==============================================================================
;; PUBLIC
public AddCoreWords_


;;==============================================================================
;; DATA
.data
	FORMAT_WITH_NEWLINE      db "%s", 10, 0
	PRINT_HI                 db "PRINT-HI", 0
	HI                       db "HOWDY", 10, 0

	INTERPRET                db "INTERPRET", 0
	DOT_QUOTE                db '."', 0
	L_PAREN                  db "(", 0
	HASH                     db "#", 0
	DOT_S                    db ".S", 0
	LOAD                     db "LOAD", 0


;;==============================================================================
;; CODE
.code

;-------------------------------------------------------------------------------
; AddCoreWords_
;
; Last update:  11/26/2017
; Description:  Adds first entry to the Dictionary_. This has all of its fields
;               set to zero.
;;
; Returns:      Nothing
;
AddCoreWords_ proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

		; Add PRINT-HI word
		NewEntry  PRINT_HI, PrintHiRoutine, ZERO_PARAMS, NOT_IMMEDIATE

		; L_PAREN: Paren comments
		NewEntry  L_PAREN, SkipPastRParen, ZERO_PARAMS, IMMEDIATE

		; HASH: Skipline comments
		NewEntry  HASH, SkipLine, ZERO_PARAMS, IMMEDIATE

		; DOT_QUOTE: String literal
		NewEntry  DOT_QUOTE, ParseStringLiteral, ZERO_PARAMS, IMMEDIATE

		; DOT_S: Print string
		NewEntry  DOT_S, PrintString, ZERO_PARAMS, NOT_IMMEDIATE

		; Add INTERPRET word
		NewEntry  INTERPRET, InterpretRoutine, ZERO_PARAMS, NOT_IMMEDIATE

		; Add LOAD word
		NewEntry  LOAD, LoadRoutine, ZERO_PARAMS, IMMEDIATE

		call AddDefinitionWords_

; Restore the caller's stack frame pointer
Exit:
		pop edi
		pop esi
		pop ebp
		ret
AddCoreWords_ endp


;-------------------------------------------------------------------------------
; PrintHiRoutine
;
; Last update:  11/21/2017
; Description:  Prints "Hi" to stdout using printf
;
; Returns:      Nothing
;
PrintHiRoutine proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

		push offset HI           ; &HI on stack as first arg
		call printf              ; printf(HI)
		add esp, 4               ; "pop" &HI

		pop edi
		pop esi
		pop ebp
		ret
PrintHiRoutine endp

;-------------------------------------------------------------------------------
; SkipPast: MACRO
;
; Last update:  12/01/2017
; Description:  Iterates esi till just past specified char or until 0
;
; Input:        esi contains address of string to check
;
; Modifies:     esi, edx
;
SkipPast MACRO char
		dec esi
@@:
		inc esi            ; esi++
		mov dl, [esi]      ; edx = cur_char
		cmp dl, 0          ; if cur_char == 0, return
		je @F

		cmp dl, char       ; if cur_char != char, we're done
		je @F              
		jmp @B             ; Loop
@@:
		inc esi            ; Go one past char
ENDM


;-------------------------------------------------------------------------------
; SkipPastRParen
;
; Last update:  12/01/2017
; Description:  Skips characters in message buffer until 0 or past ')'
;
; Returns:      Nothing
;
SkipPastRParen proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Skip past ')'
		mov esi, MessageBufferPos_  ; esi is one buffer the current char position
		SkipPast ')'

Exit:
		mov MessageBufferPos_, esi  ; Update MessageBufferPos_
; Restore stack frame
		pop edi
		pop esi
		pop ebp
		ret
SkipPastRParen endp


;-------------------------------------------------------------------------------
; SkipLine
;
; Last update:  12/01/2017
; Description:  Skips characters in message buffer until 0 or past '\n'
;
; Returns:      Nothing
;
SkipLine proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Skip past '\n'
		mov esi, MessageBufferPos_  ; esi is one buffer the current char position
		SkipPast 10        ; Skip past '\n' char

Exit:
		mov MessageBufferPos_, esi  ; Update MessageBufferPos_
; Restore stack frame
		pop edi
		pop esi
		pop ebp
		ret
SkipLine endp


;-------------------------------------------------------------------------------
; ParseStringLiteral
;
; Last update:  12/02/2017
; Description:  When in execution mode, this skips past spaces and pushes the
;               MessageBufferPos_ onto the param stack. It then skips past
;               the next '"' character and updates the MessageBufferPos_
;
; Returns:      Nothing
;
ParseStringLiteral proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Skip past spaces
		mov esi, MessageBufferPos_  ; esi is one buffer the current char position
		SkipPast ' '        ; Skip past ' ' char

; Handle compiling of string literals
		mov dl, Compiling_          ; dl = Compiling_
		test dl, dl                 ; if compiling, 
		jnz Compile                 ; Compile

; Push MessageBufferPos_ onto param stack
		mov edi, SP_                ; edi = &SP_
		mov [edi], esi              ; *SP_ = &string
		sub SP_, 4                  ; Advance SP_

; TODO: Handle escape chars
; Convert next '"' to a \NUL
		SkipPast '"'        ; Skip past '"' char
		dec esi             ; esi = address of '"'
		mov dl, 0           ; dl = \NUL
		mov [esi], dl       ; String now terminates with a \NUL
		inc esi             ; esi = &Next char
		jmp Exit

Compile:
; Copy from start of string to DictionaryStringPos_ until '"' or \NUL
		mov edi, DictionaryStringPos_  ; edi = Next free string location
@@:
		movsb                      ; Copy char from MessageBuffer_ to DictionaryStrings_ and advance
		mov dl, [esi]

		cmp dl, 0                  ; if cur_char == 0, return
		je @F

		cmp dl, '"'                ; if cur_char == '"', return
		je @F
		jmp @B
@@:

		mov dl, 0                  ; dl = \NUL
		mov [edi], dl              ; NUL terminate string
		inc edi                    ; Go to next free char in DictionaryStrings_
		mov edx, edi               ; edx = &next free string
		inc esi                    ; Go to next char in MessageBuffer_
		mov ecx, esi               ; ecx = new MessageBufferPos_

; Create a pseudoentry that pushes its parameter (&string) onto the stack
		mov edi, DP_           ; edi = DP_
		mov [edi], offset PushParam0PseudoEntry_ ; DP_ = &PushParam0PseudoEntry_
		add edi, 4             ; edi = DP_ + 4  (next parameter slot)
		mov esi, DictionaryStringPos_      ; esi = &string
		mov [edi], esi         ; Store &string in next parameter slot
		add edi, 4             ; edi = DP_ + 4 (next next param slot)
		mov DP_, edi           ; DP_ = next param slot

; Update pointers
		mov DictionaryStringPos_, edx  ; DictionaryStringPos_ = &next free string
		mov esi, ecx                   ; esi = new MessageBufferPos_

Exit:
		mov MessageBufferPos_, esi  ; Update MessageBufferPos_

; Restore stack frame
		pop edi
		pop esi
		pop ebp
		ret
ParseStringLiteral endp


;-------------------------------------------------------------------------------
; PrintString
;
; Last update:  12/02/2017
; Description:  Pops param stack and prints value as string
;
; Returns:      Nothing
;
PrintString proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Pop string from param stack and push onto program stack
		add SP_, 4         ; "Pop" string

; Print string:  printf("%s\n", string)
		mov esi, SP_       ; esi = &&string
		mov edi, [esi]     ; edi = &string
		push edi           ; Push string onto program stack
		push offset FORMAT_WITH_NEWLINE  ; "%s\n"
		call printf        ; printf(string)
		add esp, 8         ; Pop string from program stack

; Restore stack frame
		pop edi
		pop esi
		pop ebp
		ret
PrintString endp


;-------------------------------------------------------------------------------
; LoadRoutine
;
; Last update:  12/02/2017
; Description:  Gets next word form MessageBuffer_ and uses that to load a forrth
;               block to interpret.
;
; Returns:      Nothing
;
LoadRoutine proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi
		push ebx

; Get next word and uses it as a forrth block ID
		call GetNextWord_  ; WordBuffer_ has next word
		mov edi, offset WordBuffer_  ; edi = &WordBuffer_
		add edi, eax       ; edi = one past string
		mov dl, 0          ; dl = \NUL
		mov [edi], dl      ; NUL terminate string

; Find next free spot in MessageBuffer_
		mov esi, MessageBufferPos_  ; Start at MessageBufferPos_
		SkipPast 0                  ; esi = next free spot
		mov ebx, esi                ; ebx = start of string

; Load file and copy contents of file to MessageBuffer
		push esi                  ; arg1: dest
		push offset WordBuffer_   ; arg0: forrth_id
		call LoadForrthBlock      ; eax has num chars
		add esp, 8                ; clean up program stack

; Push MessageBuffer onto code stack
		mov esi, MessageBufferPos_ ; esi = original MessageBufferPos_
		mov edi, CSP_              ; edi = CSP_
		mov [edi], esi             ; *CSP_ = MessageBufferPos to return to
		sub CSP_, 4                ; CSP_ = next free slot

; Set MessageBuffer to start of file
		mov MessageBufferPos_, ebx ; MessageBufferPos_ = &copied string

; Restore stack frame
		pop ebx
		pop edi
		pop esi
		pop ebp
		ret
LoadRoutine endp



;-------------------------------------------------------------------------------
; InterpretRoutine
;
; Last update:  12/02/2017
; Description:  Pops a string off the stack, copies it to the MessageBuffer_, 
;               Pushes MessageBufferPos_ onto CodeStack_ and sets MessageBufferPos_
;               to start of input string.
;
; Returns:      Nothing
;
InterpretRoutine proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

		; Find next free position in MessageBufferPos_
		mov edi, MessageBufferPos_  ; edi = current position in MessageBuffer
		mov al, 0                   ; al = \NUL
		repne scasb                 ; edi = position of first \NUL
		inc edi                     ; edi = next free position in MessageBuffer
		mov ecx, edi                ; ecx = next free position in MessageBuffer (i.e., start of copied string)

		; Pop string off stack
		add SP_, 4                  ; "Pop" s tring
		mov edx, SP_                ; ecx = &&string
		mov esi, [edx]              ; esi = &string

		; Copy string to MessageBuffer_
@@:
		movsb                      ; Copy char from param stack (esi) to free MessageBuffer_ position (edi)
		mov dl, [esi]              ; dl = last copied char

		cmp dl, 0                  ; if cur_char != 0, loop
		je @F
		jmp @B
@@:
		inc edi                    ; edi = next free position in MessageBuffer_

		; Push MessageBufferPos_ onto code Stack
		mov esi, MessageBufferPos_ ; esi = original MessageBufferPos_
		mov edi, CSP_              ; edi = CSP_
		mov [edi], esi             ; *CSP_ = MessageBufferPos to return to
		sub CSP_, 4                ; CSP_ = next free slot

		; Set MessageBufferPos_ to start of input string
		mov MessageBufferPos_, ecx ; MessageBufferPos_ = &copied string

		pop edi
		pop esi
		pop ebp
		ret
InterpretRoutine endp

end