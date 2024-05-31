!!==============================================================================
!! File:            forrth_sr.f90
!! Created date:    12/28/2017
!! Last update:     01/03/2018
!! Author:          Rino Jose
!! Description:     Defines functions for manipulating ForrthState
!!

MODULE forrth_sr
    USE constants
    USE forrth_types
    USE items

    implicit none

CONTAINS

    !!--------------------------------------------------------------------------
    !! init_items:     Initializes forrth state
    !! Last update:    01/01/2018
    SUBROUTINE init_forrth(f)
        type (ForrthState) :: f

        f%msg_buffer_index = 0
        f%last_entry = 0
        f%is_executing_definition = .false.
        f%is_compiling = .false.
        f%stack_index = 0
        f%return_stack_index = 0
    END SUBROUTINE init_forrth


    !!--------------------------------------------------------------------------
    !! reset_forrth:   Resets forrth state
    !! Last update:    01/02/2018
    SUBROUTINE reset_forrth(f)
        type (ForrthState) :: f

        f%msg_buffer_index = 0
        f%is_executing_definition = .false.
        f%is_compiling = .false.

        f%return_stack_index = 0
        f%stack_index = 0

    END SUBROUTINE reset_forrth


    !!--------------------------------------------------------------------------
    !! add_dictionary_entry: Adds a new entry to the dictionary
    !! Last update:          01/01/2018
    SUBROUTINE add_dictionary_entry(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index

        ! Advance last_entry and set entry_index to it
        f%last_entry = f%last_entry + 1
        f%dictionary(f%last_entry) = entry_index
    END SUBROUTINE add_dictionary_entry


    !!--------------------------------------------------------------------------
    !! find_entry:     Searches for entry in dictionary
    !! Last update:    01/01/2018
    !! Returns:        entry item index or 0 if not found
    integer FUNCTION find_entry(f, w)
        type(ForrthState), intent(in) :: f
        character(len=*), intent(in) :: w
        integer :: i
        type(ForrthEntry) :: e

        do i=f%last_entry,1,-1
            e = entry_items(f%dictionary(i))
            if ( trim(w) == trim(e%word) ) then
                find_entry = e%index
                return
            endif
        end do
        find_entry = 0
    END FUNCTION find_entry


    !!--------------------------------------------------------------------------
    !! string_indexes_to_word:  Converts StringIndexes to a char array
    !! Last update:    01/02/2018
    character(len=WORD_LEN) FUNCTION string_indexes_to_word(f, w)
        type(ForrthState), intent(in) :: f
        type(StringIndexes) :: w
        integer :: buf_index

        buf_index = f%msg_buffer_index
        string_indexes_to_word = f%message_buffers(buf_index)%buffer(w%s_start:w%s_end)
    END FUNCTION string_indexes_to_word


    !!--------------------------------------------------------------------------
    !! load_msg_buffer:  Loads next message buffer with string
    !! Last update:      01/02/2018
    !! Returns:          OK: Everything was fine
    !!                   ERR_TOO_LONG: str was too long
    !!                   ERR_OVERFLOW: Too many message buffers
    SUBROUTINE load_msg_buffer(forrth, str, result)
        type(ForrthState), intent(inout) :: forrth
        character(len=*), intent(in) :: str
        integer, intent(out) :: result
        integer :: buffer_index

        if ( len_trim(str) > MESSAGE_BUFFER_LEN ) then
            result = ERR_TOO_LONG
            return
        end if

        if ( forrth%msg_buffer_index .EQ. NUM_MSG_BUFFERS ) then
            result = ERR_OVERFLOW
            return
        end if

        ! Load next buffer
        buffer_index = forrth%msg_buffer_index + 1
        forrth%message_buffers(buffer_index)%buffer = trim(str)
        forrth%message_buffers(buffer_index)%cur_string%s_start = 1
        forrth%message_buffers(buffer_index)%cur_string%s_end = len_trim(str)
        forrth%msg_buffer_index = buffer_index

        result = OK

    END SUBROUTINE load_msg_buffer


    !!--------------------------------------------------------------------------
    !! get_next_instruction: Returns next instruction in currently executing def
    !! Last update:          01/03/2018
    SUBROUTINE get_next_instruction(f, result)
        type(ForrthState) :: f
        type(ForrthEntry), intent(out) :: result
        type(ForrthEntry) :: cur_definition
        type(ItemPointer) :: item_pointer

        type(InstructionPointer) :: instruction_pointer

        ! Get instruction from current definition
        instruction_pointer = f%return_stack(f%return_stack_index)
        cur_definition = entry_items(instruction_pointer%entry_index)
        item_pointer = cur_definition%parameters%items(instruction_pointer%ip)
        if ( item_pointer%item_type .NE. T_ENTRY ) then
            print *, "Error: Instruction should have been an Entry"
            CALL reset_forrth(f)
            return
        endif

        result = entry_items(item_pointer%item_index)

        ! Advance IP
        instruction_pointer%ip = instruction_pointer%ip + 1
        f%return_stack(f%return_stack_index) = instruction_pointer
    END SUBROUTINE get_next_instruction


END MODULE forrth_sr