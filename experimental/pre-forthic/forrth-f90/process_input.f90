!!==============================================================================
!! File:            process_input.f90
!! Created date:    12/28/2017
!! Last update:     01/03/2017
!! Author:          Rino Jose
!! Description:     Implements subroutines to process input in a Forrth state
!!
MODULE process_input_sr

    USE forrth_types
    USE parse_input_sr
    USE forrth_sr
    USE items
    USE core_words

    IMPLICIT none

CONTAINS

    !!--------------------------------------------------------------------------
    !! process_input: Processes input stored in f%message_buffer
    !! Last update: 01/01/2018
    SUBROUTINE process_input(f)
    type(ForrthState) :: f
    type(ForrthEntry) :: e
    integer :: i, length, status
    type(ItemPointer) :: item_pointer

    do while(.true.)
        ! Get next entry
        CALL get_next_entry(f, e, status)
        if (status == ERROR) then
            CALL reset_forrth(f)
            return
        elseif ( status == END_OF_BUFFER ) then
            ! If more buffers on stack, pop message buffer stack
            if ( f%msg_buffer_index .GT. 1 ) then
                f%msg_buffer_index = f%msg_buffer_index - 1
                cycle
            else
                f%msg_buffer_index = 0
                return
            endif                
        end if

        ! If is immediate, execute it
        if ( e%is_immediate ) then
            CALL e%entry_routine(f, e%index)
        ! If compiling, compile into current entry
        elseif ( f%is_compiling ) then
            item_pointer%item_type = T_ENTRY
            item_pointer%item_index = e%index
            CALL add_item_pointer(item_pointer, entry_items(f%cur_def)%parameters)

        ! Otherwise, execute entry
        else
            CALL e%entry_routine(f, e%index)
        endif
    end do
    
    END SUBROUTINE process_input


    !!--------------------------------------------------------------------------
    !! get_next_entry: Returns next entry
    !! Last update:    12/28/2017
    !! Description:    If executing, returns next instruction in a definition;
    !!                 otherwise, reads next word from message buffer and returns
    !!                 associated entry.
    !!
    !! Returns:        OK:            Entry was found
    !!                 END_OF_BUFFER: At the end of the message buffer
    !!                 ERROR:         Something went wrong
    !!
    SUBROUTINE get_next_entry(f, e, result)
    type(ForrthState) :: f
    type(ForrthEntry), intent(out) :: e
    integer, intent(out) :: result

    type(StringIndexes) :: w
    integer :: word_result
    integer :: entry_index
    integer :: buf_index
    integer :: literal_result

    ! Get the message buffer index
    buf_index = f%msg_buffer_index

    if ( buf_index == 0 ) then
        result = END_OF_BUFFER
        return
    endif

    ! If we're executing a definition, get next instruction in definition
    if ( f%is_executing_definition ) then
        ! Get next instruction and advance IP
        CALL get_next_instruction(f, e)
        result = OK
        return
    else
        CALL get_word(f, w, word_result)

        if ( word_result == OK ) then
            ! Look up word in dictionary
            entry_index = find_entry(f, string_indexes_to_word(f, w))

            ! If found word, return
            if ( entry_index .NE. 0 ) then
                result = OK
                e = entry_items(entry_index)
                return
            ! Otherwise, try treating as a literal and return ERROR if cannot
            else
                CALL treat_as_literal(f, string_indexes_to_word(f, w), entry_index, literal_result)

                if ( literal_result .EQ. OK ) then
                    result = OK
                    e = entry_items(entry_index)
                else
                    result = ERROR
                    print *, "Unknown word: ", string_indexes_to_word(f, w)
                endif
                return
            endif
            
        elseif ( word_result == END_OF_BUFFER) then
            result = END_OF_BUFFER
            return

        else
            print *, "Error!"
            result = ERROR
            return
        endif
    endif
    result = ERROR
    END SUBROUTINE get_next_entry
END MODULE process_input_sr