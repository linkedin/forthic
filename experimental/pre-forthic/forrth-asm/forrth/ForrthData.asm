;;==============================================================================
;; File           : ForrthData.asm
;; Created date   : 11/19/2017
;; Last update    : 12/02/2017
;; Author         : Rino Jose
;; Description    : Defines global data for forrth interpreter and Initialize_
;;                  to set it up.
;;

.model flat,c

;;==============================================================================
;; INCLUDES
include Constants.inc
include Entry.inc
include DictionaryMacros.inc

extern AddCoreWords_:near
extern ExitDefinitionEntry_:Entry
extern ExitDefinition_:near
extern PushParam0PseudoEntry_:Entry
extern PushIntegerPseudoRoutine_:near
extern DictionaryStrings_:byte

;;==============================================================================
;; PUBLIC
public ReturnStack_
public RSP_
public RSPStart_
public IP_

public Stack_
public SP_
public SPStart_

public CodeStack_
public CSP_
public CSPStart_

public MessageBuffer_
public MessageBufferPos_

public GetNextEntry_

public WordBuffer_
public Dictionary_
public DP_
public DictionaryStringPos_
public LastEntry_
public Compiling_

public NOPEntry_


;;==============================================================================
;; DATA
.data

	;;--------------------------------------------------------------------------
	;; Items related to executing definitions
	;;
	;; IP_:           If executing a definition, IP_ points to the next instruction
	;; ReturnStack_:  Holds IP_'s to return to for nested definitions
	;; RSP_:          Points to the next free slot on the return stack
	;; RSPStart_:     Holds start of the return stack
	IP_                  dd 0
	ReturnStack_         dd RETSTACKLEN DUP(0)
	RSP_                 dd 0
	RSPStart_            dd 0

	;;--------------------------------------------------------------------------
	;; Parameter stack
	;;
	;; Stack_:        Used to pass parameters between words
	;; SP_:           Points to the next free slot on the parameter stack
	;; SPStart_:      Holds start of the parameter stack
	Stack_               dd STACKLEN DUP(0)
	SP_                  dd 0
	SPStart_             dd 0

	;;--------------------------------------------------------------------------
	;; Code stack
	;;
	;; CodeStack_:    Tracks next part of input to process
	;; CSP_:          Points to the next free slot on the code stack
	;; CSPStart_:     Holds start of the code stack
	CodeStack_           dd RETSTACKLEN DUP(0)
	CSP_                 dd 0
	CSPStart_            dd 0

	;;--------------------------------------------------------------------------
	;; Items related to processing input
	;;
	;; MessageBuffer_:     Holds current string being parsed
	;; MessageBufferPos_:  Next part of MessageBuffer_ to read
	;; WordBuffer_:        Holds last word parsed from MessageBuffer_
	;; Compiling_:         If true, compile entry into a definition; otherwise execute it
	MessageBuffer_       db (BUFFERLEN+1) DUP(0)
	MessageBufferPos_    dd 0
	WordBuffer_          db (MAXWORDLEN+1)     DUP(0)
	Compiling_           db 0

	;;--------------------------------------------------------------------------
	;; Dictionary items
	;;
	;; Dictionary_:          Holds words that a Forrth interpreter knows
	;; DP_:                  Points to next free slot in the Dictionary_
	;; DictionaryStringPos:  Points to next free string in DictionaryStrings_
	;; LastEntry_:           Points to the last entry in the Dictionary_
	;; GetNextEntry_:        Function pointer -- either Next_W_ or Next_I_
	;; NOPEntry_:            Pseudo entry that does nothing
	Dictionary_          dd DICTIONARY_SIZE   DUP(0)
	DP_                  dd 0
	DictionaryStringPos_ dd 0
	LastEntry_           dd 0
	GetNextEntry_        dd 0
	NOPWord              db "NOP", 0
	NOPEntry_ Entry      { }


;;==============================================================================
;; CODE
.code


;-------------------------------------------------------------------------------
; extern "C" void Initialize_()
;
; Last update:  11/24/2017
; Description:  Initializes Forrth state and adds basic lexicon.
;
;               This creates a start entry in the dictionary that has 0 for
;               all fields.
;
;               After this, all entries can be added in a consistent way.
;
; Returns:      Nothing
;
Initialize_ proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

		; Initialize Compiling_
		mov al, 0                                    ; eax = false
		mov Compiling_, al                           ; Compiling = false

		; Initialize DP_
		mov esi, offset Dictionary_                  ; esi = &Dictionary_
		mov DP_, esi                                 ; DP_ = start of Dictionary_

		; Initialize DictionaryStringPos_
		mov esi, offset DictionaryStrings_           ; esi = &DictionaryStrings_
		mov DictionaryStringPos_, esi                ; DP_ = start of DictionaryStrings_

		; Initialize stack pointers
		mov esi, offset ReturnStack_                 ; esi = &ReturnStack_
		add esi, RETSTACKLEN                         ; esi = end of return stack
		mov RSP_, esi                                ; RSP_ = end of return stack
		mov RSPStart_, esi                           ; RSPStart_ = end of return stack

		; Initialize return stack pointers
		mov esi, offset Stack_                       ; esi = &Stack_
		add esi, STACKLEN                            ; esi = end stack
		mov SP_, esi                                 ; SP_ = end of stack
		mov SPStart_, esi                            ; SPStart_ = end of stack

		; Initialize code stack pointers
		mov esi, offset CodeStack_                   ; esi = &Stack_
		add esi, RETSTACKLEN                         ; esi = end stack
		mov CSP_, esi                                ; SP_ = end of stack
		mov CSPStart_, esi                           ; SPStart_ = end of stack

		call AddStartEntry                           ; Create start entry for Dictionary_
		call AddCoreWords_                           ; Adds initial Forrth words

		; Set up NOPEntry
		mov esi, offset NOPWord                      ; esi = &NOPWord
		CopyWord offset NOPEntry_                    ; NOPEntry_.EntryWord = "NOP"

		mov edi, offset NOPEntry_                    ; edi = &NOPEntry_
		add edi, Entry.Routine                       ; edi = &NOPEntry_.Routine
		mov [edi], offset NOPRoutine                 ; NOPEntry_.Routine = NOPRoutine

		mov edi, offset NOPEntry_                    ; edi = &NOPEntry_
		add edi, Entry.Immediate                     ; edi = &NOPEntry_.Immediate
		mov al, 1                                    ; al = true
		mov [edi], al                                ; NOPEntry_.Immediate = true


		; Set up ExitDefinitionEntry
		mov edi, offset ExitDefinitionEntry_         ; edi = &ExitDefinitionEntry_
		add edi, Entry.Routine                       ; edi = &ExitDefinitionEntry_.Routine
		mov [edi], offset ExitDefinition_            ; ExitDefinitionEntry_.Routine = ExitDefinition

		mov edi, offset ExitDefinitionEntry_         ; edi = &ExitDefinitionEntry_
		add edi, Entry.Immediate                     ; edi = &ExitDefinitionEntry_.Immediate
		mov al, 0                                    ; al = false
		mov [edi], al                                ; ExitDefinitionEntry_.Immediate = false

		; Set up PushParam0PseudoEntry_
		mov edi, offset PushParam0PseudoEntry_      ; edi = &PushParam0PseudoEntry_
		add edi, Entry.Routine                       ; edi = &PushParam0PseudoEntry_.Routine
		mov [edi], offset PushIntegerPseudoRoutine_  ; PushParam0PseudoEntry_.Routine = PushIntegerPseudoRoutine_

		mov edi, offset PushParam0PseudoEntry_      ; edi = &PushParam0PseudoEntry_
		add edi, Entry.Immediate                     ; edi = &PushParam0PseudoEntry_.Immediate
		mov al, 0                                    ; al = false
		mov [edi], al                                ; ExitDefinitionEntry_.Immediate = false

; Restore the caller's stack frame pointer
Exit:
		pop edi
		pop esi
		pop ebp
		ret
Initialize_ endp


;-------------------------------------------------------------------------------
; extern "C" void AddStartEntry()
;
; Last update:  11/21/2017
; Description:  Adds first entry to the Dictionary_. This has all of its fields
;               set to zero.
;;
; Returns:      Nothing
;
AddStartEntry proc
; Initialize a stack frame pointer
		push ebp
		mov ebp,esp
		push esi
		push edi

; Start at DP_ and write zeroes for each field
		mov edi, DP_             ; edi = DP_
		mov ecx, MAXWORDLEN+1    ; ecx = num bytes in EntryWord
		add ecx, 12              ; ecx += (num bytes for Routine + Previous + Parameter)
		mov esi, ecx             ; esi = num bytes of entry
		mov al, 0                ; Zero out first entry
		rep stosb
		mov ecx, 0               ; ecx = 0 parameters
		CompleteEntry            ; Updates LastEntry_ and DP_ for entry
		
; Restore the caller's stack frame pointer
Exit:
		pop edi
		pop esi
		pop ebp
		ret
AddStartEntry endp


;-------------------------------------------------------------------------------
; NOPRoutine_:  Does nothing
;
; Last update:  11/24/2017
; Description:  Does nothing
;
NOPRoutine proc
		ret
NOPRoutine endp

end