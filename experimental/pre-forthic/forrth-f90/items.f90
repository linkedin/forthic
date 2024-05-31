!!==============================================================================
!! File:            items.f90
!! Created date:    01/01/2018
!! Last update:     01/04/2018
!! Author:          Rino Jose
!! Description:     Maintains list of all forrth items
!!

MODULE items
    USE constants
    USE forrth_types

    implicit none

    ! Item counts
    integer, parameter :: INITIAL_SIZE=10
    integer, dimension(FIRST_TYPE_INDEX:LAST_TYPE_INDEX) :: item_counts
    integer, dimension(FIRST_TYPE_INDEX:LAST_TYPE_INDEX) :: max_size
    integer :: i

    ! TODO: Come up with a way to free numbers and strings

    ! Item arrays
    integer, dimension(:), allocatable :: integer_items
    real, dimension(:), allocatable :: real_items
    type(String), dimension(:), allocatable :: string_items
    type(ForrthEntry), dimension(:), allocatable :: entry_items


    CONTAINS

    !!--------------------------------------------------------------------------
    !! init_items:     Initializes item arrays and counts
    !! Last update:    01/01/2018
    SUBROUTINE init_items
        ! Initialize array of counts and max sizes
        item_counts = 0
        max_size = INITIAL_SIZE

        ! Allocate initial space for item lists
        allocate( integer_items(INITIAL_SIZE) )
        allocate( real_items(INITIAL_SIZE) )
        allocate( string_items(INITIAL_SIZE) )
        allocate( entry_items(INITIAL_SIZE) )
    END SUBROUTINE init_items


    !!--------------------------------------------------------------------------
    !! add_string_item: Adds a new string item
    !! Last update:     01/04/2018
    SUBROUTINE add_string_item(word, value)
        character(len=*), intent(in) :: word
        type(ItemPointer), intent(out) :: value

        type(String), dimension(:), allocatable :: tmp_array
        integer :: my_count, my_max, new_size, new_index

        ! Resize array if we reach the max size
        my_count = item_counts(T_STRING)
        my_max = max_size(T_STRING)        
        if ( my_count .GE. my_max ) then
            new_size = my_count * 2
            allocate(tmp_array(new_size))
            tmp_array(1:my_count) = string_items
            deallocate(string_items)
            call move_alloc(tmp_array, string_items)
            max_size(T_STRING) = new_size
        endif

        ! Add new item
        new_index = my_count+1
        value%item_type = T_STRING
        value%item_index = new_index
        string_items(new_index)%str = word
        item_counts(T_STRING) = new_index
    END SUBROUTINE add_string_item


    !!--------------------------------------------------------------------------
    !! add_integer_item: Adds a new integer item
    !! Last update:  01/04/2018
    SUBROUTINE add_integer_item(int_val, value)
        integer, intent(in) :: int_val
        type(ItemPointer), intent(out) :: value

        integer, dimension(:), allocatable :: tmp_array
        integer :: my_count, my_max, new_size, new_index

        ! Resize array if we reach the max size
        my_count = item_counts(T_INTEGER)
        my_max = max_size(T_INTEGER)        
        if ( my_count .GE. my_max ) then
            new_size = my_count * 2
            allocate(tmp_array(new_size))
            tmp_array(1:my_count) = integer_items
            deallocate(integer_items)
            call move_alloc(tmp_array, integer_items)
            max_size(T_INTEGER) = new_size
        endif

        ! Add new item
        new_index = my_count+1
        value%item_type = T_INTEGER
        value%item_index = new_index
        integer_items(new_index) = int_val
        item_counts(T_INTEGER) = new_index
    END SUBROUTINE add_integer_item


    !!--------------------------------------------------------------------------
    !! add_real_item: Adds a new real item
    !! Last update:   01/04/2018
    SUBROUTINE add_real_item(real_val, value)
        real, intent(in) :: real_val
        type(ItemPointer), intent(out) :: value

        real, dimension(:), allocatable :: tmp_array
        integer :: my_count, my_max, new_size, new_index

        ! Resize array if we reach the max size
        my_count = item_counts(T_REAL)
        my_max = max_size(T_REAL)        
        if ( my_count .GE. my_max ) then
            new_size = my_count * 2
            allocate(tmp_array(new_size))
            tmp_array(1:my_count) = real_items
            deallocate(real_items)
            call move_alloc(tmp_array, real_items)
            max_size(T_REAL) = new_size
        endif

        ! Add new item
        new_index = my_count+1
        value%item_type = T_REAL
        value%item_index = new_index
        real_items(new_index) = real_val
        item_counts(T_REAL) = new_index
    END SUBROUTINE add_real_item


    !!--------------------------------------------------------------------------
    !! add_entry:      Adds ForrthEntry to items
    !! Last update:    01/01/2018
    SUBROUTINE add_entry_item(new_entry)
        type(ForrthEntry) :: new_entry
        type(ForrthEntry), dimension(:), allocatable :: tmp_array
        integer :: my_count, my_max, new_size, new_index

        ! Resize array if we reach the max size
        my_count = item_counts(T_ENTRY)
        my_max = max_size(T_ENTRY)        
        if ( my_count .GE. my_max ) then
            new_size = my_count * 2
            allocate(tmp_array(new_size))
            tmp_array(1:my_count) = entry_items
            deallocate(entry_items)
            call move_alloc( tmp_array, entry_items )
            max_size(T_ENTRY) = new_size
        endif

        ! Add new entry
        new_index = my_count+1
        new_entry%index = new_index
        entry_items(new_index) = new_entry
        item_counts(T_ENTRY) = new_index
    END SUBROUTINE add_entry_item


    !!--------------------------------------------------------------------------
    !! add_item_pointer: Adds ItemPointer to ItemPointerArray
    !! Last update:      01/03/2018
    SUBROUTINE add_item_pointer(item_pointer, item_pointer_array)
        type(ItemPointer) :: item_pointer
        type(ItemPointerArray) :: item_pointer_array
        type (ItemPointer), dimension(:), allocatable :: tmp_array
        integer :: new_index


        ! If empty, allocate space
        if ( item_pointer_array%cur_size == 0) then
            allocate( item_pointer_array%items(ITEMS_TO_ADD) )
            item_pointer_array%cur_size = ITEMS_TO_ADD
        endif

        ! If out of space, reallocate
        if ( item_pointer_array%num_items .GE. item_pointer_array%cur_size ) then
            allocate(tmp_array(item_pointer_array%num_items + ITEMS_TO_ADD))
            tmp_array(1:item_pointer_array%num_items) = item_pointer_array%items
            deallocate(item_pointer_array%items)
            CALL move_alloc(tmp_array, item_pointer_array%items)
            item_pointer_array%cur_size = item_pointer_array%num_items + ITEMS_TO_ADD
        endif

        ! Add item_pointer
        new_index = item_pointer_array%num_items + 1
        item_pointer_array%items(new_index) = item_pointer
        item_pointer_array%num_items = new_index
    END SUBROUTINE add_item_pointer


END MODULE items