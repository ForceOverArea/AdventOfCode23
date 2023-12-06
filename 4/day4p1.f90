module day4funcs
    use useful
    implicit none
contains

end module day4funcs

program day4prog
    use useful
    use day4funcs
    implicit none

    integer :: read_unit = 87
    character(len=200), allocatable, dimension(:) :: lines, dyn_mem, dyn_mem2
    character(len=200) :: card_txt, winningnos_txt
    character(len=3) :: num
    integer :: i, j, card_score, total, tmp

    lines = read_file_to_lines(read_unit, 'input.txt')
    total = 0
    do i = 1, size(lines)

        dyn_mem = split(lines(i), '|')
        card_txt = dyn_mem(1)(10:)      ! We can ignore the card number for this part
        winningnos_txt = dyn_mem(2)
        deallocate(dyn_mem)

        card_score = 0
        dyn_mem = split(card_txt, ' ')

        do j = 1, size(dyn_mem)
            ! print *, dyn_mem(j)
            read(dyn_mem(j), '(I10)') tmp
            if (tmp == 0) cycle

            num = '  '
            if (0 < tmp .and. tmp < 10) then
                write(num(3:3), '(I1)') tmp
            else
                write(num(2:3), '(I2)') tmp
            end if
            call remove(num, ' ')

            ! print *, 'Checking for "', num, '"'
            if (string_has(winningnos_txt, num) .and. num /= '0 ') then
                
                if (card_score > 0) then
                    card_score = card_score * 2
                    print *, 'Found winning number "', num, '" for a new card score of ', card_score,' points.'
                else
                    card_score = 1
                    print *, 'Found winning number "', num, '" for a new card score of ', card_score,' points.'
                end if

            end if

        end do
        deallocate(dyn_mem)

        total = total + card_score
        print *, 'Card ', i, ' had a score of ', card_score, ' points, for a total of ', total

    end do
    deallocate(lines)

    ! Soln is NOT 25362
    print *, 'The solution to day 4, part 1 is ', total

end program day4prog