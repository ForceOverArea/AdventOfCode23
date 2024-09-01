module day6funcs
    use iso_fortran_env, only: dp => real64
    implicit none
contains

    function calculate_distance(time, hold_for) result(distance)
        real(dp), intent(in) :: time, hold_for
        real(dp) :: distance

        distance = (time - hold_for) * hold_for
    end function calculate_distance


    function will_break_record(time, hold_for, record) result(res)
        real(dp), intent(in) :: time, hold_for, record
        logical :: res

        real(dp) :: traveled

        traveled = calculate_distance(time, hold_for)

        res = .false.

        if ( traveled > record ) then
            res = .true.
            print *, 'Found winning hold time ', hold_for, ' which travels ', traveled, ' mm.'
        end if
    end function will_break_record

end module day6funcs

program day6prog
    use iso_fortran_env, only: dp => real64
    use day6funcs
    implicit none

    real(dp) :: time, record, first_winner, last_winner, j
    integer :: i

    time   = 46807866.0
    record = 214117714021024.0

    do i = 1, int(time)
        ! print *, i
        if (will_break_record(time, real(i, dp), record)) then
            first_winner = real(i)
            exit
        end if
    end do

    print *, 'Taking it from the top'

    do i = 1, int(time)
        ! print *, j
        j = time - real(i, dp)
        if (will_break_record(time, j, record)) then
            last_winner = j
            exit
        end if
    end do

    print *, 'The solution to day 6, part 2 is ', last_winner - first_winner + 1
end program day6prog