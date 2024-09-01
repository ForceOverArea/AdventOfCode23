module day6funcs
    implicit none
contains

    function calculate_distance(time, hold_for) result(distance)
        integer, intent(in) :: time, hold_for
        integer :: distance

        distance = (time - hold_for) * hold_for
    end function calculate_distance


    function will_break_record(time, hold_for, record) result(res)
        integer, intent(in) :: time, hold_for, record
        logical :: res

        integer :: traveled

        traveled = calculate_distance(time, hold_for)

        res = .false.

        if ( traveled > record ) then
            res = .true.
            print *, 'Found winning hold time ', hold_for, ' which travels ', traveled, ' mm.'
        end if
    end function will_break_record

end module day6funcs

program day6prog
    use day6funcs
    implicit none

    integer, dimension(4) :: times, records 
    integer :: i, j, winners, margin_of_error

    times   = [46,  80,   78,   66  ]
    records = [214, 1177, 1402, 1024]

    ! times   = [7, 15, 30 ]
    ! records = [9, 40, 200]

    margin_of_error = 1

    do i = 1, size(times)
        winners = 0
        do j = 1, times(i)
            if (will_break_record(times(i), j, records(i))) then
                print *, 'incrementing winners'
                winners = winners + 1
            end if
        end do
        print *, 'Found ', winners, ' ways to win for race ', i
        margin_of_error = margin_of_error * winners
    end do

    print *, 'The solution to day 6, part 1 is ', margin_of_error
end program day6prog