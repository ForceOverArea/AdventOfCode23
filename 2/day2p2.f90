module day2funcs
    implicit none

    integer :: line_len = 200
    integer :: MAX_RED = 12
    integer :: MAX_BLUE = 14
    integer :: MAX_GREEN = 13

    type pull
        !! 
        character(len=1) :: color
        integer :: count
    end type pull

    type round
        !! Represents a round in a game (i.e. the cubes pulled in one grab)
        integer :: red = 0, blue = 0, green = 0
        type(pull), allocatable, dimension(:) :: pulls
    contains
        procedure :: check_round
        procedure :: release_round
    end type round

    type game
        !! Represents a game with several rounds.
        type(round), allocatable, dimension(:) :: rounds
    contains 
        procedure :: check_game
        procedure :: min_set
        procedure :: release_game
    end type game

contains
    function read_file_to_lines(unit, filepath) result(lines)
        !! Reads a file to an array of strings, each 200 characters long.
        !! The resulting array of strings must be deallocated manually.
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

    ! Reusable string operations

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

    ! END Reusable string operations 
    ! START Constructor functions

    function new_pull(pull_str) result(newp)
        !!
        character(len=200), intent(in) :: pull_str ! input will look like ' 4 blue' 
        type(pull) :: newp
        character(len=200), allocatable, dimension(:) :: pull_info
        pull_info = split(pull_str(2:), ' ') ! ignore leading whitespace
        
        read(pull_info(1), '(I10)') newp%count
        newp%color = pull_info(2)(1:1) ! only read 1st char

        deallocate(pull_info)
    end function new_pull

    function new_round(round_str) result(newr)
        character(len=200), intent(in) :: round_str
        type(round) :: newr
        character(len=200), allocatable, dimension(:) :: pull_strs
        integer :: i

        pull_strs = split(round_str, ',')
        allocate(newr%pulls(size(pull_strs)))

        !! This loop is where each round is checked
        do i = 1, size(pull_strs)

            newr%pulls(i) = new_pull(pull_strs(i))

        end do
        deallocate(pull_strs)
    end function new_round

    function new_game(game_str) result(newg)
        character(len=200), intent(in) :: game_str
        type(game) :: newg
        character(len=200), allocatable, dimension(:) :: round_strs
        integer :: i

        round_strs = split(game_str, ';')   ! Everything after the colon
        allocate(newg%rounds(size(round_strs)))
        do i = 1, size(round_strs)

            newg%rounds(i) = new_round(round_strs(i))

        end do
        deallocate(round_strs)
    end function new_game

    ! END Constructor functions
    ! START Procedures

    function check_game(this) result(res)
        class(game), intent(inout) :: this
        logical :: res
        integer :: i

        res = .true. ! Innocent until proven guilty
        
        do i = 1, size(this%rounds)
            res = this%rounds(i)%check_round()
            if (.not. res) return
        end do
    end function check_game

    function min_set(this) result(res)
        class(game) :: this
        integer, dimension(3) :: res
        integer :: i, j, red, blue, green
        type(pull) :: p

        red = 0
        blue = 0
        green = 0

        do i = 1, size(this%rounds)
            do j = 1, size(this%rounds(i)%pulls)
                p = this%rounds(i)%pulls(j)
                select case (p%color)
                case ('r')
                    if (p%count > red) red = p%count
                case ('b')
                    if (p%count > blue) blue = p%count
                case ('g')
                    if (p%count > green) green = p%count
                case default
                    error stop 'error: found bad color value in type(game)'
                end select 
            end do
        end do

        res = (/red, blue, green/)
    end function min_set

    function check_round(this) result(res)
        class(round), intent(inout) :: this
        logical :: res
        integer :: i, j

        res = .true. ! Innocent until proven guilty

        do i = 1, size(this%pulls)
            do j = 1, size(this%pulls)
                select case (this%pulls(j)%color)
                    case ('r')
                        res = this%pulls(j)%count <= MAX_RED
                    case ('b')
                        res = this%pulls(j)%count <= MAX_BLUE
                    case ('g')
                        res = this%pulls(j)%count <= MAX_GREEN
                    case default
                        error stop 'error: found a bad color character!'
                end select
                if (.not. res) return ! Exit the function when a false is found
            end do
        end do
    end function check_round

    subroutine release_game(this)
        class(game) :: this
        integer :: i

        do i = 1, size(this%rounds)
            call this%rounds(i)%release_round()
        end do
        deallocate(this%rounds)
    end subroutine release_game

    subroutine release_round(this)
        class(round) :: this 
        deallocate(this%pulls)
    end subroutine release_round

end module day2funcs

program day2prog
    use day2funcs
    implicit none

    integer :: read_unit = 87
    integer :: i, id, total
    character(len=200), allocatable, dimension(:) :: lines, dyn_stor
    character(len=200) :: grabs_str, game_id
    type(game) :: my_game
    integer, dimension(3) :: rbg

    total = 0
    lines = read_file_to_lines(read_unit, 'input.txt')

    do i = 1, size(lines)
        dyn_stor  = split(lines(i), ':')

        game_id   = dyn_stor(1)         ! Get the id number text
        grabs_str = dyn_stor(2)         ! Get all the grabs from the bag
        deallocate(dyn_stor)

        dyn_stor = split(game_id, ' ')
        call remove(game_id, 'Game ')
        read(game_id, '(I10)') id       ! Get the id number as an int
        deallocate(dyn_stor)

        my_game = new_game(grabs_str)

        rbg = my_game%min_set()

        total = total + (rbg(1) * rbg(2) * rbg(3))

        call my_game%release_game()
    end do
    deallocate(lines) ! Prevent memory leak.

    print *, 'The solution to day 2, part 1 is: ', total

end program day2prog