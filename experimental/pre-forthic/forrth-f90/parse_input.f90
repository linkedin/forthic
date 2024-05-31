!!==============================================================================
!! File:            parse_input.f90
!! Created date:    12/28/2017
!! Last update:     12/30/2017
!! Author:          Rino Jose
!! Description:     Implements parsing of Forrth input
!!

MODULE parse_input_sr
    USE constants
    USE forrth_types

CONTAINS

    !!--------------------------------------------------------------------------
    !! get_word:       Gets next word from the Forrth message buffer
    !! Last update:    12/30/2017
    !! Description:    Parses next word out of message buffer
    !!
    !!                 See specs/PARSE_INPUT.txt for details
    !!
    !! The result will either be OK or END_OF_BUFFER
    !! This modifies the start of forrth%cur_string as words are parsed out.
    SUBROUTINE get_word(forrth, word, result)
        implicit none
        type (ForrthState) :: forrth
        type (StringIndexes), intent(out) :: word
        integer, intent(out) :: result

        integer :: state
        integer :: i
        character :: c
        integer :: buf_index
        type(StringIndexes) :: cur_string

        buf_index = forrth%msg_buffer_index

        ! If nothing to parse return
        cur_string = forrth%message_buffers(buf_index)%cur_string

        if ( cur_string%s_start > cur_string%s_end) then
            result = END_OF_BUFFER
            return
        endif
        
        state = START
        result = ERROR
        do i=cur_string%s_start, cur_string%s_end
            if ( result == OK ) exit

            c = forrth%message_buffers(buf_index)%buffer(i:i)

            if ( state == START ) then
                if ( c == ' ' .OR. c == NEWLINE ) then
                    cycle
                elseif ( c == '#' ) then
                    word%s_start = i
                    state = CHECK_COMMENT
                    cycle
                elseif ( c == '"' ) then
                    word%s_start = i
                    state = COLLECT_STRING
                    cycle
                else                    
                    word%s_start = i
                    state = NORMAL
                    cycle
                endif

            elseif ( state == NORMAL ) then
                if ( c == ' ' .OR. c == NEWLINE ) then
                    word%s_end = i-1
                    state = START
                    result = OK
                    cycle
                elseif ( c == '#' ) then
                    state = CHECK_COMMENT
                    cycle
                endif

            elseif ( state == CHECK_COMMENT ) then
                if ( c == ' ') then
                    word%s_end = i-2
                    state = SKIP_COMMENT
                    if ( word%s_start < word%s_end ) then
                        result = OK
                    else
                        result = ERROR
                    endif
                    cycle
                else                    
                    state = NORMAL
                    cycle
                endif
            
            elseif ( state == SKIP_COMMENT ) then
                if ( c == NEWLINE ) then
                    state = START
                    cycle
                endif

            elseif ( state == COLLECT_STRING ) then
                if ( c == '"' ) then
                    word%s_end = i
                    state = START
                    result = OK
                    cycle
                else
                    cycle
                endif
            endif
        end do
    
        cur_string%s_start = i
        forrth%message_buffers(buf_index)%cur_string = cur_string
        if (result == OK) return

        ! Check end state
        if (state == NORMAL .OR. state == COLLECT_STRING) then
            word%s_end = cur_string%s_end
            result = OK
        else
            result = END_OF_BUFFER
        endif
        return
    END SUBROUTINE get_word

END MODULE parse_input_sr
