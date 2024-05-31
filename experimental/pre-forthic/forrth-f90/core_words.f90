!!==============================================================================
!! File:            core_words.f90
!! Created date:    01/02/2018
!! Last update:     01/04/2018
!! Author:          Rino Jose
!! Description:     Defines core words for Forrth
!!
MODULE core_words
    USE constants
    USE forrth_types
    USE parse_input_sr
    USE items
    USE forrth_sr

    implicit none

    type(ItemPointer) :: ExitDefEntryPointer

CONTAINS

    !!--------------------------------------------------------------------------
    !! print_hi:    Prints "HOWDY"
    !! Last update: 01/02/2018
    SUBROUTINE print_hi(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index
        print *, "HOWDY"
    END SUBROUTINE print_hi


    !!--------------------------------------------------------------------------
    !! execute_definition: Starts the execution of a definition
    !! Last update:        01/02/2018
    SUBROUTINE execute_definition(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index
        integer :: ret_stack_index

        ! Check for overflow
        if ( f%return_stack_index .GE. RETSTACKLEN) then
            print *, "Error: Return stack overflow!"
            CALL reset_forrth(f)
            return
        endif

        ! Push current InstructionPointer onto return stack
        ret_stack_index = f%return_stack_index + 1
        f%return_stack_index = ret_stack_index

        ! Set IP to first instruction of current entry
        f%return_stack(ret_stack_index)%entry_index = entry_index
        f%return_stack(ret_stack_index)%ip = 1

        ! Set is_executing_definition
        f%is_executing_definition = .true.
    END SUBROUTINE execute_definition


    !!--------------------------------------------------------------------------
    !! exit_definition: Starts the execution of a definition
    !! Last update:        01/03/2018
    SUBROUTINE exit_definition(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index
        
        f%return_stack_index = f%return_stack_index - 1

        if ( f%return_stack_index .EQ. 0 ) then
            f%is_executing_definition = .false.
        endif
    END SUBROUTINE exit_definition


    !!--------------------------------------------------------------------------
    !! begin_definition:  Sets up new definition entry
    !! Last update:       01/02/2018
    SUBROUTINE begin_definition(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index
        type (ForrthEntry) :: new_entry
        type(StringIndexes) :: w
        integer :: word_result

        ! Set forrth to "is compiling"
        f%is_compiling = .true.

        ! Get next word to use for new entry word
        CALL get_word(f, w, word_result)
        if ( word_result .NE. OK ) then
            print *, "Can't get word to create a definition"
            CALL reset_forrth(f)
            return
        endif

        ! Create new entry item and point f%cur_def to it
        new_entry%word = string_indexes_to_word(f, w)
        new_entry%entry_routine => execute_definition
        CALL add_entry_item(new_entry)

        f%cur_def = new_entry%index
    END SUBROUTINE begin_definition


    !!--------------------------------------------------------------------------
    !! end_definition: Ends the current definition
    !! Last update:        01/03/2018
    SUBROUTINE end_definition(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index

        ! Compile ExitDefinitionEntry into current entry
        CALL add_item_pointer(ExitDefEntryPointer, entry_items(f%cur_def)%parameters)

        ! Add entry to forrth dictionary
        CALL add_dictionary_entry(f, f%cur_def)

        ! Turn off compiling mode
        f%is_compiling = .false.

    END SUBROUTINE end_definition


    !!--------------------------------------------------------------------------
    !! push_first_param: Pushes first param onto stack
    !! Last update:      01/03/2018
    SUBROUTINE push_first_param(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index
        integer :: cur_index
        type(ForrthEntry) :: cur_entry

        ! Check for stack overflow
        cur_index = f%stack_index
        if ( cur_index .GE. STACKLEN ) then
            print *, "Error: Stack overflow"
            CALL reset_forrth(f)
            return
        endif

        ! Get entry
        cur_entry = entry_items(entry_index)

        ! Push cur_entry's first param onto stack
        f%stack_index = cur_index + 1
        f%param_stack(f%stack_index) = cur_entry%parameters%items(1)
    END SUBROUTINE push_first_param


    !!--------------------------------------------------------------------------
    !! interpret:   Pops string from stack and interprets it
    !! Last update: 01/04/2018
    SUBROUTINE interpret(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index
        integer :: cur_stack_index
        type(ItemPointer) :: param1
        integer :: load_result

        ! Pop string off stack
        cur_stack_index = f%stack_index
        param1 = f%param_stack(cur_stack_index)
        f%stack_index = cur_stack_index - 1

        if ( param1%item_type .NE. T_STRING ) then
            print *, "Error: Expected string for interpret"
            CALL reset_forrth(f)
            return
        endif

        ! Push onto message buffer
        CALL load_msg_buffer(f, string_items(param1%item_index)%str, load_result)

    END SUBROUTINE interpret


    !!--------------------------------------------------------------------------
    !! load:        Loads string from file and interprets it
    !! Last update: 01/04/2018
    !! Description: Reads next word as the id of a forrth block. Constructs a
    !!              filename like BLOCK-<id>.forrth
    SUBROUTINE load(f, entry_index)
        type(ForrthState) :: f
        integer :: entry_index
        integer :: word_result, load_result
        type(StringIndexes) :: w
        character(len=FILENAME_LEN) :: filename
        integer :: stat
        character (len=MESSAGE_BUFFER_LEN) :: line, buffer
        integer :: isize

        ! Get next word to use for new entry word
        CALL get_word(f, w, word_result)
        if ( word_result .NE. OK ) then
            print *, "Can't get word for LOAD"
            CALL reset_forrth(f)
            return
        endif

        ! Construct filename
        filename = "BLOCK-" // trim(string_indexes_to_word(f, w)) // ".forrth"

        ! Open file
        open(LOAD_UNIT, file=filename, status='OLD', iostat=stat)
        if ( stat .NE. OK ) then
            print *, "Error: Problem opening file: ", filename
            CALL reset_forrth(f)
            return
        endif

        ! Read file
        buffer = ""
        do while( .true. )
            READ(unit=LOAD_UNIT, fmt='(a)', advance='NO', size=isize, iostat=stat) line

            ! If at EOF, close file and exit
            if ( isize .EQ. 0 ) then
                close(LOAD_UNIT)
                exit
            endif
            
            buffer = trim(buffer) // trim(line) // NEWLINE
        end do

        ! Push file contents onto message buffer
        CALL load_msg_buffer(f, trim(buffer), load_result)
    END SUBROUTINE load


    !!--------------------------------------------------------------------------
    !! add_core_words:  Adds core words to forrth dictionary
    !! Last update:     01/02/2018
    SUBROUTINE add_core_words(f)
        type(ForrthState) :: f
        procedure(entry_routine), pointer :: routine

        type(ItemPointer) :: item
        type(ItemPointerArray) :: item_array

        CALL add_item_pointer(item, item_array)

        ! Add ExitDefEntry that all definitions use
        CALL add_exit_def_entry(f)

        routine => print_hi
        CALL add_entry(f, "PRINT-HI", routine, NOT_IMMEDIATE)

        routine => begin_definition
        CALL add_entry(f, ":", routine, NOT_IMMEDIATE)

        routine => end_definition
        CALL add_entry(f, ";", routine, IMMEDIATE)

        routine => interpret
        CALL add_entry(f, "INTERPRET", routine, NOT_IMMEDIATE)

        routine => load
        CALL add_entry(f, "LOAD", routine, NOT_IMMEDIATE)
    END SUBROUTINE add_core_words


    !!--------------------------------------------------------------------------
    !! add_entry:    Adds entry to forrth dictionary
    !! Last update:  01/02/2018
    SUBROUTINE add_entry(f, word, routine, immediate)
        type(ForrthState) :: f
        character(len=*) :: word
        procedure(entry_routine), pointer :: routine
        logical :: immediate
        type (ForrthEntry) :: forrth_entry

        ! Set up entry
        forrth_entry%word = word
        forrth_entry%is_immediate = immediate
        forrth_entry%entry_routine => routine

        ! Add entry to entry items
        CALL add_entry_item(forrth_entry)

        ! Add to forrth state
        CALL add_dictionary_entry(f, forrth_entry%index)
    END SUBROUTINE add_entry


    !!--------------------------------------------------------------------------
    !! add_exit_def_entry:    Adds exit def entry to entry items
    !! Last update:  01/03/2018
    !!
    !! This also stores the exit def entry's index in exit_def_entry_index
    SUBROUTINE add_exit_def_entry(f)
        type(ForrthState) :: f
        type (ForrthEntry) :: forrth_entry

        ! Set up entry
        forrth_entry%is_immediate = NOT_IMMEDIATE
        forrth_entry%entry_routine => exit_definition

        ! Add entry to entry items
        CALL add_entry_item(forrth_entry)

        ! Store index
        ExitDefEntryPointer%item_type = T_ENTRY
        ExitDefEntryPointer%item_index = forrth_entry%index

    END SUBROUTINE add_exit_def_entry


    !!--------------------------------------------------------------------------
    !! add_push_item_entry: Adds an entry that pushes an item pointer onto stack
    !! Last update:  01/04/2018
    SUBROUTINE add_push_item_entry(item_pointer, new_entry)
        type(ItemPointer), intent(in) :: item_pointer
        type (ForrthEntry), intent(out) :: new_entry

        ! Set up entry
        new_entry%is_immediate = NOT_IMMEDIATE
        new_entry%entry_routine => push_first_param
        CALL add_item_pointer(item_pointer, new_entry%parameters)

        ! Add entry to entry items
        CALL add_entry_item(new_entry)
    END SUBROUTINE add_push_item_entry


    !!--------------------------------------------------------------------------
    !! treat_as_literal:    Tries to treat word as a literal
    !! Last update:         01/03/2018
    !! Description:         If word can be viewed as a string, number, or other
    !!                      literal, create a PushFirstParamEntry that pushes
    !!                      the value of the literal onto the param stack.
    !!
    !!                      If word could be treated as literal, result = OK and
    !!                      entry_index has the entry. Otherwise, result = ERROR.
    SUBROUTINE treat_as_literal(f, word, entry_index, result)
        type(ForrthState) :: f
        character(len=*) :: word
        integer, intent(out) :: entry_index
        integer, intent(out) :: result
        integer :: int_val
        real :: real_val
        integer :: int_stat, real_stat
        type(ItemPointer) :: value
        type(ForrthEntry) :: new_entry

        result = ERROR

        ! See if word is a string
        if ( word(1:1) == '"' ) then
            CALL add_string_item(word(2:len_trim(word)-1), value)
            CALL add_push_item_entry(value, new_entry)
            entry_index = new_entry%index
            result = OK
            return
        endif

        ! See if word is an integer
        read(word, *, iostat=int_stat) int_val
        if ( int_stat == OK ) then
            CALL add_integer_item(int_val, value)
            CALL add_push_item_entry(value, new_entry)
            entry_index = new_entry%index
            result = OK
            return
        endif

        ! See if word is a real
        read(word, *, iostat=real_stat) real_val
        if ( real_stat == OK ) then
            CALL add_real_item(real_val, value)
            CALL add_push_item_entry(value, new_entry)
            entry_index = new_entry%index
            result = OK
            return
        endif

    END SUBROUTINE treat_as_literal
END MODULE core_words