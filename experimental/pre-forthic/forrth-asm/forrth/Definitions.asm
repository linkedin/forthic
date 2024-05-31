;;==============================================================================
;; File:            Definitions.asm
;; Created date:    11/27/2017
;; Last update:     11/28/2017
;; Author:          Rino Jose
;; Description:     Contains words for creating and executing definitions
;;

.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc
include Entry.inc
include DictionaryMacros.inc

extern DP_:dword
extern LastEntry_:dword
extern GetNextWord_:near
extern WordBuffer_:byte
extern Compiling_:byte
extern RSP_:dword
extern IP_:dword
extern Compiling_:byte
extern Next_W_:near
extern GetNextEntry_:near


;;==============================================================================
;; PUBLIC
public Next_I_
public AddDefinitionWords_
public ExitDefinition_

public ExitDefinitionEntry_


;;==============================================================================
;; DATA
.data
	COLON                    db ":", 0
	SEMICOLON                db ";", 0

	ExitDefinitionEntry_     Entry  { }


;;==============================================================================
;; CODE
.code


;-------------------------------------------------------------------------------
; AddDefinitionWords_
;
; Last update:  11/27/2017
; Description:  Adds { : } and { ; } words
;;
; Returns:      Nothing
;
AddDefinitionWords_ proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

		; Add { : } word
		NewEntry  COLON, StartDefinition, ZERO_PARAMS, NOT_IMMEDIATE

		; Add { ; } word
		NewEntry  SEMICOLON, EndDefinition, ZERO_PARAMS, IMMEDIATE

; Restore the caller's stack frame pointer
Exit:
		pop edi
		pop esi
		pop ebp
		ret
AddDefinitionWords_ endp


;-------------------------------------------------------------------------------
; StartDefinition
;
; Last update:  11/25/2017
; Description:  Run when { : } is executed. Starts creating a new definition
;
; inputs:       eax = &entry
;
; Returns:      Nothing
;
StartDefinition proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Reads next word and uses it as the EntryWord for the next entry
		call GetNextWord_        ; WordBuffer_ = next word, eax = num chars
		mov esi, offset WordBuffer_    ; esi = WordBuffer_
		SetEntryWord                   ; new_entry.EntryWord = word

; Sets Routine
		mov esi, offset ExecuteDefinition  ; esi = ExecuteDefinition
		SetRoutine                     ; new_entry.Routine = ExecuteDefinition

; Sets the Previous entry
		SetPrevious                    ; new_entry.Previous = LastEntry_

; Sets Immediate flag
		SetImmediate 0                 ; new_entry.immediate = false

; Sets Compiling_ flag
		mov al, 1                      ; al = 1
		mov Compiling_, al             ; Compiling = true

; Push DP_ onto return stack and then set DP_ to first parameter of entry
		mov esi, DP_                ; esi = DP_
		mov edi, RSP_               ; edi = RSP_
		mov [edi], esi              ; *RSP_ = IP_
		sub RSP_, 4                 ; Advance RSP_
		add esi, Entry.Parameter    ; esi = new_entry.Parameter
		mov DP_, esi                ; DP_ = new_entry.Parameter

; Restore the caller's stack frame pointer
		pop edi
		pop esi
		pop ebp
		ret
StartDefinition endp


;-------------------------------------------------------------------------------
; EndDefinition
;
; Last update:  11/26/2017
; Description:  Run when { ; } is executed. Ends the current definition.
;
; inputs:       eax = &entry
;
; Returns:      Nothing
;
EndDefinition proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi
		push ebx

; Compile ExitDefinition into next parameter slot
		mov edi, DP_             ; edi = &next_param
		mov eax, offset ExitDefinitionEntry_ ; eax = &ExitDefinitionEntry_
		mov [edi], eax           ; next_param = &ExitDefinitionEntry_
		add edi, 4             ; edi = DP_ + 4
		mov DP_, edi           ; DP_ = next param slot

; Pop return stack, restore DP_ pointer, and note num params
		mov esi, RSP_            ; esi = RSP_
		add esi, 4               ; esi = &(top of ReturnStack_)
		mov eax, DP_             ; Note current DP_, which is one parameter after the last in the entry
		mov edi, [esi]           ; edi = previous DP_ = &entry
		mov DP_, edi             ; Restore DP_
		mov RSP_, esi            ; Finish pop of ReturnStack_

		add edi, Entry.Parameter ; edi = &(entry.parameter[0])
		sub eax, edi             ; eax = num parameter bytes in entry
		cdq                      ; eax => edx:eax
		mov ebx, 4               ; 4 bytes per parameter
		idiv ebx                 ; eax = num params

; Complete entry
		mov ecx, eax             ; ecx = num_parameters
		CompleteEntry            ; Updates LastEntry_ and DP_ for entry

; Clear compiling flag
		mov al, 0                ; al = 0
		mov Compiling_, al       ; Compiling = false

; Restore the caller's stack frame pointer
		pop ebx
		pop edi
		pop esi
		pop ebp
		ret
EndDefinition endp


;-------------------------------------------------------------------------------
; ExecuteDefinition
;
; Last update:  11/25/2017
; Description:  Routine that executes all definitions
;
; inputs:       eax = &entry
;
; Returns:      Nothing
;
ExecuteDefinition proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Push IP_ onto return stack
		mov esi, IP_                ; esi = IP_
		mov edi, RSP_               ; edi = RSP_
		mov [edi], esi              ; *RSP_ = IP_
		sub RSP_, 4                 ; Advance RSP_

; Set IP_ = first parameter of entry being executed
		mov ecx, eax                ; ecx = &entry
		add ecx, Entry.Parameter    ; ecx = &entry.Parameter
		mov IP_, ecx              ; IP_ = entry.Parameter[0]

; Set GetNextEntry_ to Next_I_
		mov ecx, offset GetNextEntry_         ; ecx = &GetNextEntry_
		mov [ecx], offset Next_I_             ; GetNextEntry_ = Next_I_

; Restore the caller's stack frame pointer
		pop edi
		pop esi
		pop ebp
		ret
ExecuteDefinition endp


;-------------------------------------------------------------------------------
; ExitDefinition_
;
; Last update:  11/25/2017
; Description:  Last instruction in a definition that does bookkeeping and
;               exits a routine
;
; inputs:       eax = &entry
;
; Returns:      Nothing
;
ExitDefinition_ proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Pop previous IP_ from return stack
		add RSP_, 4        ; Pop return stack
		mov esi, RSP_      ; esi = RSP_
		mov eax, [esi]     ; eax = *RSP_
		mov IP_, eax       ; IP_ = *RSP_

; Set GetNextEntry_ to Next_W_ if return stack is empty
		test eax, eax
		jnz Exit           ; If stack is not empty, exit

		mov ecx, offset GetNextEntry_     ; ecx = &GetNextEntry_
		mov [ecx], offset Next_W_         ; GetNextEntry_ = Next_W_

Exit:
		pop edi
		pop esi
		pop ebp
		ret
ExitDefinition_ endp


;-------------------------------------------------------------------------------
; Next_I_: Returns next instruction of defined word being executed.
;
; Last update:  11/25/2017
; Description:  Returns address that IP_ points to and advances IP by 4
;
; Returns:      EAX has address of Entry to execute
;
Next_I_ proc
; Initialize a stack frame pointer
        push ebp
        mov ebp,esp
		push esi
		push edi

		; Get instruction
		mov esi, IP_
		mov eax, [esi]     ; result = eax = instruction

		; Advance IP_ to next instruction
		add IP_, 4       ; IP_ += 4

; Restore the caller's stack frame pointer
		pop edi
		pop esi
        pop ebp
        ret
Next_I_ endp


end