!!==============================================================================
!! File:            forrth_entry_sr.f90
!! Created date:    01/03/2018
!! Last update:     01/03/2018
!! Author:          Rino Jose
!! Description:     Defines functions for manipulating ForrthEntries
!!

MODULE forrth_entry_sr
    USE constants
    USE forrth_types
    USE items

CONTAINS

    !!--------------------------------------------------------------------------
    !! compile_entry:  Compiles entry into current definition
    !! Last update:  01/03/2018
    !!
    SUBROUTINE compile_entry(f, entry_index)
        type(ForrthState) :: f        
        integer :: entry_index

        ! TODO: Test cur size of ItemPointerArray
    END SUBROUTINE compile_entry

END MODULE forrth_entry_sr