!!==============================================================================
!! File:            forrthtran.f90
!! Created date:    12/28/2017
!! Last update:     01/02/2018
!! Author:          Rino Jose
!! Description:     Runs Forrth in a fortran environment
!!
PROGRAM forrthtran
    USE constants
    USE forrth_types
    USE forrth_sr
    USE process_input_sr
    USE items
    USE core_words

    IMPLICIT none

    integer :: i_err, entry_index
    type (ForrthState) :: forrth
    character (len=MESSAGE_BUFFER_LEN) :: buffer
    integer :: load_result

    ! Initialize items and the Forrth state
    CALL init_items
    CALL init_forrth(forrth)
    CALL add_core_words(forrth)

    ! Run REPL
    do while( .true. )
        ! Read input into buffer
        READ(unit=*, fmt='(a)', iostat=i_err) buffer
        if ( i_err .NE. 0 ) exit

        ! Load forrth message buffer and process
        CALL load_msg_buffer(forrth, buffer, load_result)
        CALL process_input(forrth)
    end do


END PROGRAM forrthtran