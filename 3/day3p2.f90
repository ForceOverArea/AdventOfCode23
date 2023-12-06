module day3funcs
    use useful
    implicit none

    type ipair 
        integer :: first = 0, second = 0
    end type ipair

    type range_vec
        type(ipair), allocatable, dimension(:) :: ranges
    end type range_vec

    abstract interface 
        function predicate_fn(ch) result(res)
            character(len=1), intent(in) :: ch
            logical :: res
        end function predicate_fn
    end interface

contains
    function get_number_ranges(string) result(ranges)
        !! Returns the start and stop positions of numbers in a given string.
        character(len=200), intent(in) :: string
        type(range_vec) :: ranges
        integer :: i, count
        logical :: in_digit

        ! Count the number of allocations we need
        in_digit = .false.
        count = 0
        do i = 1, len(string)
            if (is_digit(string(i:i)) .and. .not. in_digit) then
                in_digit = .true.
                count = count + 1
            else if (.not. is_digit(string(i:i))) then
                in_digit = .false.
            end if
        end do

        allocate(ranges%ranges(count))

        ! Get the start and stop point for each number
        in_digit = .false.
        count = 1
        do i = 1, len(string)
            if (is_digit(string(i:i)) .and. .not. in_digit) then
                ! print *, 'Found starting digit ', string(i:i)
                in_digit = .true.
                ranges%ranges(count)%first = i
            else if (.not. is_digit(string(i:i)) .and. in_digit) then
                ! print *, 'Found ending digit ', string(i-1:i-1)
                in_digit = .false.
                ranges%ranges(count)%second = i-1
                count = count + 1
            end if
        end do
    end function get_number_ranges

    function is_adjacent_to_symbol(num, surr_lines) result(res)
        type(ipair), intent(in) :: num
        character(len=200), dimension(:), intent(in) :: surr_lines
        logical :: res
        integer :: i, j, start, end

        res = .false.

        if (num%first == 1) then 
            start = 1
        else 
            start = num%first - 1
        end if

        if (num%second == 200) then 
            end = 200
        else 
            end = num%second + 1
        end if

        do i = 1, size(surr_lines)
            do j = start, end
                if (surr_lines(i)(j:j) /= '.' .and. surr_lines(i)(j:j) /= ' ' .and. .not. is_digit(surr_lines(i)(j:j))) then
                    res = .true.
                    return
                end if
            end do
        end do
    end function is_adjacent_to_symbol

    function point_in_range(j, ranges) result(res)
        integer, intent(in) :: j
        type(ipair), dimension(:), intent(in) :: ranges
        logical :: res
        integer :: i

        res = .false.
        do i = 1, size(ranges)
            if (ranges(i)%first <= j .or. j <= ranges(i)%second) then 
                res = .true.
                return
            end if
        end do
    end function point_in_range

    function check_adjacent_to_char(text, i, j, predicate) result(finds)
        !! Returns a 2D array indicating whether the given predicate is true
        !! for all values surrounding the given location in the body of text.
        !! The center index of the array contains a count of how many times
        !! out of 8 the predicate returned true
        character(len=200), allocatable, dimension(:), intent(in) :: text
        integer, intent(in) :: i, j
        procedure(predicate_fn) :: predicate
        integer, dimension(-1:1, -1:1) :: finds
        integer :: k, l

        finds(:, :) = 0

        do k = -1, +1
            do l = -1, +1
                if (predicate(text(i+k)(j+l:j+l))) then
                    finds(k, l) = 1
                    finds(0, 0) = finds(0, 0) + 1
                end if
            end do
        end do
    end function check_adjacent_to_char  

    function is_gear(text, i, j) result(res)
        character(len=200), allocatable, dimension(:), intent(in) :: text
        integer, intent(in) :: i, j
        logical :: res, top_is_one_number, bot_is_one_number, top_is_two_numbers, bot_is_two_numbers, left, right
        integer, dimension(-1:1, -1:1) :: found_nums
        integer :: gear_count
        found_nums = check_adjacent_to_char(text, i, j, is_digit)

        top_is_one_number = sum(found_nums(-1, :)) >= 1 .and. found_nums(-1, 0) == 0
        bot_is_one_number = sum(found_nums(+1, :)) >= 1 .and. found_nums(+1, 0) == 0
        top_is_two_numbers = found_nums(-1, -1) == 1 .and. found_nums(-1, +1) == 1
        bot_is_two_numbers = found_nums(+1, -1) == 1 .and. found_nums(+1, +1) == 1
        left = found_nums(0, -1) == 1
        right = found_nums(0, +1) == 1

        gear_count = 0
        if (top_is_one_number)  gear_count = gear_count + 1
        if (bot_is_one_number)  gear_count = gear_count + 1
        if (top_is_two_numbers) gear_count = gear_count + 2
        if (bot_is_two_numbers) gear_count = gear_count + 2
        if (left)               gear_count = gear_count + 1
        if (right)              gear_count = gear_count + 1

        res = gear_count == 2
    end function is_gear

    function gear_ratio_if_gear(char, lines, ranges) result(ratio)
        character(len=1), intent(in) :: char
        character(len=200), allocatable, dimension(:), intent(in) :: lines
        type(ipair), allocatable, dimension(:), intent(in) :: ranges
        integer :: ratio
        integer :: i, j 

        ratio = -1

        do i = 1, size(lines)
            do j = 1, 200
                if (lines(i)(j:j) == '*') then
                    if (point_in_range(j, ranges(i-1:i+1))) then

                    end if
                end if
            end do
        end do
    end function gear_ratio_if_gear

end module day3funcs

program day3prog
    use useful
    use day3funcs
    implicit none

    character(len=200), allocatable, dimension(:) :: lines
    type(range_vec), allocatable, dimension(:) :: ranges
    type(ipair) :: pos
    integer :: i, j, total, num
    integer :: read_unit = 87

    total = 0

    lines = read_file_to_lines(read_unit, 'input.txt')
    allocate(ranges(size(lines)))

    do i = 1, size(lines)

        ranges(i) = get_number_ranges(lines(i))
        do j = 1, size(ranges(i)%ranges)
            
            if (i == 1) then
            
                if (is_adjacent_to_symbol(ranges(i)%ranges(j), lines(1:2))) then
                    pos = ranges(i)%ranges(j)
                    read(lines(i)(pos%first:pos%second), '(I10)') num
                    total = total + num

                    print *, 'Found part number: ', num, ' from ', pos%first, ' to ', pos%second, &
                    ' on line ', i, ', making the total: ', total
                end if
            
            else if (i == size(lines)) then

                if (is_adjacent_to_symbol(ranges(i)%ranges(j), lines(size(lines)-1:size(lines)))) then
                    pos = ranges(i)%ranges(j)
                    read(lines(i)(pos%first:pos%second), '(I10)') num
                    total = total + num
                    
                    print *, 'Found part number: ', num, ' from ', pos%first, ' to ', pos%second, &
                    ' on line ', i, ', making the total: ', total                 
                end if

            else
                
                if (is_adjacent_to_symbol(ranges(i)%ranges(j), lines(i-1:i+1))) then
                    pos = ranges(i)%ranges(j)
                    read(lines(i)(pos%first:pos%second), '(I10)') num
                    total = total + num
                    
                    print *, 'Found part number: ', num, ' from ', pos%first, ' to ', pos%second, &
                    ' on line ', i, ', making the total: ', total
                end if
            
            end if
        end do
    end do
    deallocate(lines, ranges)

    print *, 'The solution to day 3, part 1 is: ', total

end program day3prog