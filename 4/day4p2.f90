module day4funcs
    use useful
    implicit none
contains

    function number_of_matches(card) result(matches)
        character(len=200), intent(in) :: card
        integer :: matches

        character(len=200), allocatable, dimension(:) ::  dyn_mem
        character(len=200) :: card_txt, winningnos_txt
        character(len=3) :: num
        integer :: j, tmp

        dyn_mem = split(card, '|')
        card_txt = dyn_mem(1)(10:)      ! We can ignore the card number for this part
        winningnos_txt = dyn_mem(2)
        deallocate(dyn_mem)

        matches = 0
        dyn_mem = split(card_txt, ' ')
        do j = 1, size(dyn_mem)
            ! print *, '"', dyn_mem(j), '"'
            read(dyn_mem(j), '(I10)') tmp
            if (tmp == 0) cycle

            num = '   '
            if (tmp < 10) then
                ! print *, tmp
                write(num(3:3), '(I1)') tmp
            else
                write(num(2:3), '(I2)') tmp
            end if

            ! print *, 'looking for "', num, '"'
            if (string_has(winningnos_txt, num) .and. num /= '0  ') then
                ! print *, 'found match: ', num
                matches = matches + 1
            end if
        end do
        deallocate(dyn_mem)
    end function number_of_matches

    subroutine count_cards(cards, counts, idx)
        !! alters a mutable array of how many of each card you have. 
        !! I'm as surprised as you are that this isn't recursive.
        character(len=200), dimension(:), intent(in) :: cards
        integer, dimension(:), intent(inout) :: counts
        integer, intent(in) :: idx
        integer :: i, n

        n = number_of_matches(cards(idx))
        print *, 'Card ', idx, ' won copies of cards ', [(i, i = idx+1, idx+n)]

        ! If we have 2 of the card that won n cards, then we add n won copies to that card's count 
        counts(idx+1:idx+n) = counts(idx+1:idx+n) + (counts(idx))
    end subroutine count_cards

end module day4funcs


program day4prog
    use useful
    use day4funcs
    implicit none

    integer :: read_unit = 87
    character(len=200), allocatable, dimension(:) :: lines
    integer, allocatable, dimension(:) :: counts
    integer :: i

    lines = read_file_to_lines(read_unit, 'input.txt')
    
    allocate(counts(size(lines)))
    counts(:) = 1

    do i = 1, size(lines)
        call count_cards(lines, counts, i)
        ! print *, 'Copies: ', counts
    end do
    deallocate(lines)

    print *, 'The solution to day 4, part 2 is ', sum(counts)
    deallocate(counts)
end program day4prog