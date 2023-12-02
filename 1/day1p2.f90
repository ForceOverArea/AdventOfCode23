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

    function unspell_digit(string) result(digit)
        !! Translates a spelled out digit to a digit character.
        !! e.g. 'one' -> '1'
        character(len=*), intent(in) :: string
        character(len=1) :: digit
    
        select case (string)
        case ('zero')
            digit = '0'
        case ('one')
            digit = '1'
        case ('two')
            digit = '2'
        case ('three')
            digit = '3'
        case ('four')
            digit = '4'
        case ('five')
            digit = '5'
        case ('six')
            digit = '6'
        case ('seven')
            digit = '7'
        case ('eight')
            digit = '8'
        case ('nine')
            digit = '9'
        case default
            digit = 'A'
        end select
    end function

    function is_spelled_digit(string, pos) result(digit)
        character(len=200), intent(in) :: string
        integer, intent(in) :: pos
        character(len=1) :: digit
        character(len=5) :: spelled_digits(0:9) = &
            (/'zero ', 'one  ', 'two  ', 'three', 'four ', &
              'five ', 'six  ', 'seven', 'eight', 'nine '/)
        character(len=5) :: word
        integer :: lengths(0:9) = &
            (/4, 3, 3, 5, 4, 4, 3, 5, 5, 4/)
        integer :: i, n

        digit = 'A'
        do i = 0, 9
            n = lengths(i)
            word = spelled_digits(i)

            if ( string(pos:pos+n-1) == word(1:n) ) then
                print *, 'Match found! (', string(pos:pos+n-1), ')'
                digit = unspell_digit(string(pos:pos+n-1))
                exit
            end if
        end do
    end function is_spelled_digit

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
            else if (is_spelled_digit(string, i) /= 'A') then
                digit = is_spelled_digit(string, i)
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
            else if (is_spelled_digit(string, i) /= 'A') then
                digit = is_spelled_digit(string, i)
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

    print *, 'The answer to day 1, part 2 is: ', total
    deallocate(lines)

end program day1prog