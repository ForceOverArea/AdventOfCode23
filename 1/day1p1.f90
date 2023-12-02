!! Day 1 of advent of code 2023 in good 'ol Fortran
module day1funcs
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

    function first_digit(string) result(digit)
        !! Returns the last digit character found in a string of 200 characters
        character(len=200), intent(in) :: string
        character(len=1) :: digit
        integer :: i

        digit = 'A'

        do i = 1, 200
            if (is_digit(string(i:i))) then
                digit = string(i:i)
                exit
            end if
        end do 
    end function first_digit

    function last_digit(string) result(digit)
        !! Returns the last digit character found in a string of 200 characters
        character(len=200), intent(in) :: string
        character(len=1) :: digit
        integer :: i, j

        digit = 'A'

        do j = 1, 200
            i = 201 - j ! Count down from 200 to 1

            if (is_digit(string(i:i))) then
                digit = string(i:i)
                exit
            end if
        end do 
    end function last_digit

end module day1funcs

program day1prog
    use day1funcs
    implicit none

    character(len=200), allocatable, dimension(:) :: lines
    character(len=1)    :: dig1, dig2
    character(len=2)    :: num
    integer             :: n, i, total, val
    integer             :: read_unit = 87

    lines = read_file_to_lines(read_unit, 'input.txt')
    n     = size(lines)
    total = 0

    do i = 1, n
        dig1 = first_digit(lines(i))
        dig2 =  last_digit(lines(i))

        num = dig1//dig2

        read(num, '(I10)') val

        print *, 'Calculated value: ', val, ' on line ', i

        total = total + val
    end do

    print *, 'The answer to day 1, part 1 is: ', total
    deallocate(lines)

end program day1prog