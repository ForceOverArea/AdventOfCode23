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
    card_txt = dyn_mem(1)(8:)      ! We can ignore the card number for this part
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

    recursive function calculate_won_cards(card, next_cards) result(num_won)
        character(len=200) :: card
        character(len=200), dimension(:) :: next_cards
        integer :: num_won
        integer :: i, n

        ! TODO: take heed!
        ! Copies of scratchcards are scored like normal scratchcards and 
        ! have the same card number as the card they copied. So, if you 
        ! win a copy of card 10 and it has 5 matching numbers, it would 
        ! then win a copy of the same cards that the original card 10 won: 
        ! cards 11, 12, 13, 14, and 15.

        n = number_of_matches(card)
        num_won = n
        do i = 1, n
            ! print *, 'Calculating won cards from cards ', 2, ' to ', n 
            num_won = num_won + calculate_won_cards(next_cards(1), next_cards(2:))
        end do
    end function calculate_won_cards

end module day4funcs

program day4prog
    use useful
    use day4funcs
    implicit none

    integer :: read_unit = 87
    character(len=200), allocatable, dimension(:) :: lines
    integer :: i, n, total

    lines = read_file_to_lines(read_unit, 'demo.txt')
    total = 0
    do i = 1, size(lines)
        n = calculate_won_cards(lines(i), lines(i+1:size(lines)))
        print *, 'checking the next ', n, ' cards after card ', i
        total = total + n
        print *, 'Found ', n, ' total cards from card ', i
    end do
    deallocate(lines)

    print *, 'The solution to day 4, part 2 is ', total

end program day4prog