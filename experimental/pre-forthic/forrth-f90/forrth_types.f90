!!==============================================================================
!! File:            forrth_types.f90
!! Created date:    12/28/2017
!! Last update:     01/03/2018
!! Author:          Rino Jose
!! Description:     Defines all Forrth types
!!

MODULE forrth_types  
USE constants

    implicit none

    !!--------------------------------------------------------------------------
    !! StringIndexes:  Marks a word in a string (esp. message buffer)
    !! Last update:    12/30/2017
    TYPE StringIndexes
        integer :: s_start
        integer :: s_end
    END TYPE StringIndexes


    !!--------------------------------------------------------------------------
    !! String:         Convenience to abstract away string definition
    !! Last update:    01/01/2018
    !!
    !! Note:           Ideally, strings would be dynamically sized. However,
    !!                 gfortran does not support this yet. For now, all string
    !!                 parameters will have the same max length.
    TYPE String
        character(len=STRING_LEN) :: str
    END TYPE String

    !!--------------------------------------------------------------------------
    !! ItemPointer:    "Points" to an item in an item array in the items module
    !! Last update:    01/01/2018
    TYPE ItemPointer
        integer :: item_type
        integer :: item_index
    END TYPE ItemPointer


    !!--------------------------------------------------------------------------
    !! ItemPointerArray: Helps maintain an array of ItemPointers
    !! Last update:      01/01/2018
    TYPE ItemPointerArray
        integer :: cur_size = 0
        integer :: num_items = 0
        type (ItemPointer), dimension(:), allocatable :: items
    END TYPE ItemPointerArray


    !!--------------------------------------------------------------------------
    !! MessageBuffer:  Maintains a buffer and the current string within it
    !! Last update:    01/02/2018
    TYPE MessageBuffer
        character (len=MESSAGE_BUFFER_LEN) :: buffer
        type(StringIndexes) :: cur_string
    END TYPE MessageBuffer


    !!--------------------------------------------------------------------------
    !! InstructionPointer: Maintains current entry and instruction
    !! Last update:        01/03/2018
    TYPE InstructionPointer
        integer :: entry_index = 0
        integer :: ip = 1
    END TYPE InstructionPointer


    !!--------------------------------------------------------------------------
    !! ForrthState:    State of the Forrth interpreter
    !! Last update:    01/01/2018
    !!
    !! Fields:
    !!      dictionary: an array of integer indices into the entry_items array
    !!      last_entry: an index into dictionary giving the latest entry
    !!      cur_def:    index of entry being currently defined
    !! 
    TYPE ForrthState
        type(MessageBuffer), dimension(1:NUM_MSG_BUFFERS) :: message_buffers
        integer :: msg_buffer_index

        integer, dimension(1:NUM_ENTRIES) :: dictionary
        integer :: last_entry

        logical :: is_executing_definition

        logical :: is_compiling
        integer :: cur_def

        type(ItemPointer), dimension(1:STACKLEN) :: param_stack
        integer :: stack_index

        type(InstructionPointer), dimension(1:RETSTACKLEN) :: return_stack
        integer :: return_stack_index
    END TYPE ForrthState


    INTERFACE
        !!--------------------------------------------------------------------------
        !! entry_routine: Function type for entry functions
        !! Last update:    01/01/2018
        SUBROUTINE entry_routine(f, entry_index)
            IMPORT :: ForrthState
            type (ForrthState) :: f
            integer :: entry_index
        END SUBROUTINE entry_routine
    END INTERFACE


    !!--------------------------------------------------------------------------
    !! ForrthEntry:    Entry in a Forrth dictionary
    !! Last update:    01/01/2018
    TYPE ForrthEntry
        character (len=WORD_LEN) :: word
        integer :: index
        type (ItemPointerArray) :: parameters
        procedure (entry_routine), nopass, pointer :: entry_routine => null()
        logical :: is_immediate = .false.
    END TYPE ForrthEntry

END MODULE forrth_types

