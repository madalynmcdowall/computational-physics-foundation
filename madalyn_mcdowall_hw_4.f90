! Purpose: Calculate the apparent magnitude of a binary star system

program binary_star_system_apparent_magnitude
    implicit none

    ! Variable Dictionary
    real :: input_time         ! The time in days entered by the user
    real :: phase_time         ! The time within the 6.4-day cycle
    real :: apparent_magnitude ! The calculated apparent magnitude
    real, parameter :: CYCLE_PERIOD = 6.4
    real, parameter :: PI = 3.141592653
    integer :: n_cycles        ! Number of full cycles completed

    ! Prompting the user for the arbitrary time in days
    write(*,*) "Enter an arbitrary time in days:"
    read(*,*) input_time

    ! Computing the phase time using integer arithmetic
    n_cycles = int(input_time / CYCLE_PERIOD)
    phase_time = input_time - n_cycles * CYCLE_PERIOD

    ! Checking and correcting floating point imprecision
    if (phase_time < 0.0) then
        phase_time = phase_time + CYCLE_PERIOD
    end if

    ! Evaluating the piecewise function
    if (phase_time >= 0.0 .and. phase_time < 0.9) then
        apparent_magnitude = 2.5
    else if (phase_time >= 0.9 .and. phase_time < 2.3) then
        apparent_magnitude = 3.335 - log(1.352 + cos(PI * &
            (phase_time - 0.9) / 0.7))
    else if (phase_time >= 2.3 .and. phase_time < 4.4) then
        apparent_magnitude = 2.5
    else if (phase_time >= 4.4 .and. phase_time < 5.2) then
        apparent_magnitude = 3.598 - log(1.998 + cos(PI * &
            (phase_time - 4.4) / 0.4))
    else if (phase_time >= 5.2 .and. phase_time < 6.4) then
        apparent_magnitude = 2.5
    end if

    ! Displaying the resulting values
    write(*,*)
    write(*,'(A,F6.2,A)') "Input Time: ", input_time, " days"
    write(*,'(A,I3,A)')   "Full Cycles: ", n_cycles, " full cycles"
    write(*,'(A,F6.2,A)') "Phase Time: ", phase_time, " days"
    write(*,'(A,F6.4,A)') "Apparent Magnitude: ", apparent_magnitude, " mag"

end program binary_star_system_apparent_magnitude
