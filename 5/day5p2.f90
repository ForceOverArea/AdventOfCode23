module day5funcs
    use iso_fortran_env
    use useful
    implicit none

    type lookup_table
        integer(int64) :: src, dest, rlen
    contains
        procedure :: lookup
    end type lookup_table

    type ltable_manager
        type(lookup_table), allocatable, dimension(:) :: tables
    contains 
        procedure :: map_value
        procedure :: release_ltable_manager
    end type ltable_manager

contains

    function create_lookup_table(dest_st, src_st, rlen) result(res)
        !! I'm proud of this one. Shoutout to Fortran for the index offset functionality!
        integer(int64), intent(in) :: dest_st, src_st, rlen
        type(lookup_table) :: res

        res%src = src_st
        res%dest = dest_st
        res%rlen = rlen
    end function create_lookup_table

    function lookup(this, num) result(res)
        class(lookup_table) :: this
        integer(int64), intent(in) :: num
        integer(int64) :: res
        integer(int64), dimension(this%src:this%src + this%rlen) :: ltable
        integer(int64) :: i

        ltable = [(i, i = this%dest, this%dest + this%rlen-1)]
        
        res = ltable(num)
    end function lookup

    function read_line_to_lookup_table(line) result(res)
        !! Reads a line of text and parses it into a lookup table
        character(len=200), intent(in) :: line
        type(lookup_table) :: res

        character(len=200), allocatable, dimension(:) :: text
        integer(int64), allocatable, dimension(:) :: nums

        ! print *, 'Reading line ', line

        text = split(line, ' ')
        allocate(nums(size(text)))
        read(text, '(I10)') nums ! You guys believe in this?

        res = create_lookup_table(nums(1), nums(2), nums(3))

        deallocate(nums, text)
    end function read_line_to_lookup_table

    function map_value(this, value) result(res)
        !! Maps an input to its respective output 
        class(ltable_manager), intent(in) :: this
        integer(int64), intent(in) :: value
        integer(int64) :: res
        integer :: i

        do i = 1, size(this%tables)
            if (this%tables(i)%src <= value                                  &
                .and.                 value <= this%tables(i)%src + this%tables(i)%rlen - 1) &
                
                res = value + this%tables(i)%dest - this%tables(i)%src
        end do
    end function map_value

    subroutine release_ltable_manager(this)
        !! Releases the resources tied up in an ltable_manager type 
        class(ltable_manager), intent(inout) :: this

        deallocate(this%tables)
    end subroutine release_ltable_manager

    function read_lines_to_ltable_manager(lines) result(res)
        character(len=200), allocatable, dimension(:), intent(in) :: lines
        type(ltable_manager) :: res
        integer :: i

        allocate(res%tables(size(lines)))
        print *, 'Are we making it here at least? '

        res%tables = [(read_line_to_lookup_table(lines(i)), i = 1, size(lines))]

    end function read_lines_to_ltable_manager

    function minarray(arr) result(res)
        integer(int64), dimension(:), intent(in) :: arr
        integer(int64) :: res
        integer :: i

        res = arr(1)
        do i = 2, size(arr)
            if (arr(i) < res) res = arr(i)
        end do
    end function minarray

    subroutine min_val_from_seed(units, files, seeds)
        character(len=200), dimension(:), intent(in) :: files
        integer(int64), allocatable, dimension(:), intent(inout) :: seeds
        integer, dimension(:), intent(in) :: units
        character(len=200), allocatable, dimension(:) :: lines
        integer :: i, j
        type(ltable_manager) :: map

        do i = 2, size(files)
            lines = read_file_to_lines(units(i), files(i))
            map = read_lines_to_ltable_manager(lines)
            print *, 'Allocated ltable manager for ', files(i)
            deallocate(lines)

            seeds = [( map%map_value(seeds(j)), j = 1, size(seeds) )]

            call map%release_ltable_manager()
        end do
    end subroutine min_val_from_seed

end module day5funcs

program day5prog
    use iso_fortran_env
    use useful
    use day5funcs
    implicit none

    integer :: unit_a = 87
    integer :: unit_b = 88
    integer :: unit_c = 89
    integer :: unit_d = 90
    integer :: unit_e = 91
    integer :: unit_f = 92
    integer :: unit_g = 93
    integer :: unit_h = 94

    character(len=200) :: file_a = 'input/a-seeds.txt'
    character(len=200) :: file_b = 'input/b-seed-soil.txt'
    character(len=200) :: file_c = 'input/c-soil-fert.txt'
    character(len=200) :: file_d = 'input/d-fert-wat.txt'
    character(len=200) :: file_e = 'input/e-wat-light.txt'
    character(len=200) :: file_f = 'input/f-light-temp.txt'
    character(len=200) :: file_g = 'input/g-temp-hum.txt'
    character(len=200) :: file_h = 'input/h-hum-loc.txt'

    integer, dimension(8) :: units
    character(len=200), dimension(8) :: files
    character(len=200), allocatable, dimension(:) :: seed_txt, lines, dyn_mem
    integer(int64), allocatable, dimension(:) :: seeds
    integer(int64) :: seed_range, seed_start, min_val, k

    integer :: i
    integer(int64) :: j

    units = [unit_a, unit_b, unit_c, unit_d, unit_e, unit_f, unit_g, unit_h]
    files = [file_a, file_b, file_c, file_d, file_e, file_f, file_g, file_h]

    lines = read_file_to_lines(unit_a, 'input/a-seeds.txt')

    min_val = 0
    do i = 1, size(lines) ! For i in size lines
        seed_txt = split(lines(j), ' ')
        read(seed_txt(1), '(I10)') seed_range
        read(seed_txt(2), '(I10)') seed_start
        deallocate(seed_txt)

        do j = seed_start, seed_start + seed_range
           if 

        end do
    end do

    print *, 'The solution to day 5, part 1 is: ', min_val

    deallocate(lines)
end program day5prog