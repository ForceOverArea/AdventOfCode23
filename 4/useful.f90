module useful
    implicit none
contains

    function read_file_to_lines(unit, filepath) result(lines)
        !! Reads a file to an array of strings, each 200 characters long.
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filepath
        character(len=200) :: line
        character(len=200), allocatable, dimension(:) :: lines
        integer :: ios, n, i

        open(unit, &
            file=filepath, &
            status='old', &
            access='sequential', &
            form='formatted', &
            action='read', &
            iostat=ios)

        if (ios /= 0) &
            error stop 'error: failed to open file'

        n = 0

        ! Count the number of lines in the file
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) &
                exit
            n = n + 1
        end do

        ! Allocate array for lines of text
        print *, 'File contains ', n, ' lines.'
        allocate(lines(n))
        rewind(unit)

        ! Store all lines in array
        do i = 1, n
            read(unit, '(A)') lines(i)
        end do
    end function read_file_to_lines

    function is_digit(char) result(res)
        !! Returns a logical value indicating if the given character is a digit
        character(len=1), intent(in) :: char
        logical :: res

        ! In my initial solution, I had a switch statement. Here, I just 
        ! check if the value is a legit ASCII char for a digit.
        if (iachar(char) <= 57 .and. iachar(char) >= 48) then
            res = .true.
            return
        end if
        res = .false.
    end function is_digit

    function contains_n(string, char) result(count)
        !! Returns the number of instances of a character exist in a given string
        character(len=*), intent(in) :: string
        character(len=1), intent(in) :: char
        integer :: count, i

        count = 0

        do i = 1, len(string)
            if (string(i:i) == char) then
                count = count + 1
            end if
        end do
    end function contains_n

    function split(string, delim) result(substrs)
        !! Returns an array of substrings separated by the given delimiter
        !! in `string`. The resulting array of substrings must be deallocated
        !! manually.
        character(len=*), intent(in) :: string
        character(len=1), intent(in) :: delim
        character(len=200), allocatable, dimension(:) :: substrs
        integer :: i, splits, last_delim, n

        splits = contains_n(string, delim)

        allocate(substrs(splits + 1))

        n = 1
        last_delim = 0
        do i = 1, len(string)
            if (string(i:i) == delim) then
                substrs(n) = string(last_delim+1:i-1)
                n = n + 1
                last_delim = i
            end if
        end do

        substrs(n) = string(last_delim+1:len(string)) ! Grab last substr
    end function split

    subroutine remove(string, substr) 
        !! Removes the pattern `substr` from anywhere 
        !! it is found within `string`.
        character(len=*), intent(inout) :: string
        character(len=*), intent(in) :: substr
        integer :: i, endi

        do i = 1, len(string)
            endi = i + len(substr) - 1
            if (string(i:endi) == substr) then
                string = string(:i-1)//string(endi+1:)
            end if
        end do
    end subroutine remove

    function string_has(string, substr) result(res)
        !! Checks for a given substring within a given string
        character(len=*), intent(in) :: string, substr
        logical :: res
        integer :: i, endi

        res = .false.

        do i = 1, len(string)
            endi = i + len(substr) - 1
            if (string(i:endi) == substr) then
                res = .true.
            end if
        end do
    end function string_has

end module useful