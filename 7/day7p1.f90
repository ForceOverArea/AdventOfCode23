module day7funcs
    use useful
    implicit none

    character(len=13) :: CARDS = "AKQJT98765432"

    type camel_cards_hand
        integer, dimension(13) :: counts
    end type camel_cards_hand

    type cch_type
        integer :: strength
        integer :: highest
    end type cch_type

contains

    function actual_len(string) result(length)
        !! Returns the length of a string before any whitespace is found
        character(len=*), intent(in) :: string
        integer :: length
        integer :: i

        length = 0
        do i = 1, len(string)
            if (string(i:i) /= ' ') then
                length = length + 1
            else 
                return
            end if
        end do
    end function actual_len

    function read_hand(hand) result(cch)
        !! Counts all the cards in a hand as text.
        character(len=200), intent(in) :: hand
        type(camel_cards_hand) :: cch

        character(len=200) :: counted_hand
        integer :: i

        do i = 1, len(CARDS)
            counted_hand = hand
            call remove(counted_hand, CARDS(i:i))
            select case (CARDS(i:i))
            case ('A') 
                cch%counts(13) = 5 - actual_len(counted_hand)
            case ('K') 
                cch%counts(12) = 5 - actual_len(counted_hand)
            case ('Q') 
                cch%counts(11) = 5 - actual_len(counted_hand)
            case ('J') 
                cch%counts(10) = 5 - actual_len(counted_hand)
            case ('T') 
                cch%counts( 9) = 5 - actual_len(counted_hand)
            case ('9') 
                cch%counts( 8) = 5 - actual_len(counted_hand)
            case ('8') 
                cch%counts( 7) = 5 - actual_len(counted_hand)
            case ('7') 
                cch%counts( 6) = 5 - actual_len(counted_hand)
            case ('6') 
                cch%counts( 5) = 5 - actual_len(counted_hand)
            case ('5') 
                cch%counts( 4) = 5 - actual_len(counted_hand)
            case ('4') 
                cch%counts( 3) = 5 - actual_len(counted_hand)
            case ('3') 
                cch%counts( 2) = 5 - actual_len(counted_hand)
            case ('2') 
                cch%counts( 1) = 5 - actual_len(counted_hand)
            case default
                error stop 'Found illegal card! The king of beers?'
            end select
        end do
    end function read_hand

    function score_hand(cch) result(score)
        !! Determines the 'type' of the camel cards hand
        type(camel_cards_hand), intent(in) :: cch
        type(cch_type) :: score

        integer :: i, high_count

        do i = 1, 13
            cch%counts(i)
        end do  
    end function score_hand

end module day7funcs

program day7prog
    use day7funcs
    use useful
    implicit none


end program day7prog