!!==============================================================================
!! File:            constants_module.f90
!! Created date:    12/28/2017
!! Last update:     12/28/2017
!! Author:          Rino Jose
!! Description:     Defines all Forrth constants
!!
MODULE constants
    implicit none

    ! Size of message buffer
    integer, parameter :: MESSAGE_BUFFER_LEN = 2048
    integer, parameter :: NUM_MSG_BUFFERS = 10

    ! Word size
    integer, parameter :: WORD_LEN = 40

    ! Filename size
    integer, parameter :: FILENAME_LEN = 128

    ! File units
    integer, parameter :: LOAD_UNIT = 15

    ! Size of return and parameter stacks
    integer, parameter :: STACKLEN = 32
    integer, parameter :: RETSTACKLEN = 32

    ! Number of entries in the Dictionary
    integer, parameter :: NUM_ENTRIES = 512

    character, parameter :: NEWLINE = char(10)

    ! Parser states
    integer, parameter :: START=1, &
                          NORMAL=2, &
                          CHECK_COMMENT=3, &
                          COLLECT_STRING=4, &
                          SKIP_COMMENT=5

    ! Num items to add at a time
    integer, parameter :: ITEMS_TO_ADD = 5

    ! Item types
    integer, parameter :: T_INTEGER=1, &
                          T_REAL=2, &
                          T_STRING=3, &
                          T_ENTRY=4
    integer, parameter :: STRING_LEN = 255
    integer, parameter :: FIRST_TYPE_INDEX=1, LAST_TYPE_INDEX=4
    logical, parameter :: IMMEDIATE = .true., NOT_IMMEDIATE = .false.

    ! Return values
    integer, parameter :: OK = 0, &
                          ERROR = 1, &
                          ERR_TOO_LONG = 2, &
                          ERR_OVERFLOW = 3, &
                          END_OF_BUFFER = 4

END MODULE constants