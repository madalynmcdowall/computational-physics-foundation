! Purpose: Apply Simpson's Rule to evaluate the definite integral 
! (int_1^20) [(x + cos(x)) * e^(cos(x)) + a * e^(-(x - b)² / c)] dx
!
! Convergence Test:
!   n     Result
!   2     184.487411
!   4     9528.191406
!   6     189.766113
!   8     2615.496338
!   10    281.874054
!   100   883.337708
!   200   1023.939941
!   300   993.877930
!   400   993.624329
!   500   993.629822
!   600   993.630127
!   700   993.630676
!   800   993.631409
!   900   993.631897
!   1000  993.630615
!   1500  993.632996
!   2000  993.631653
!
!  The most accurate value (approximately): 993.631
!  I chose 993.631 because after around n = 400, the results stop changing 
!  in any meaningful way and stay steady near 993.63. This shows that the 
!  Simpson’s Rule calculation has settled into a stable answer. Once the 
!  program has enough subintervals to capture the sharp spike around 
!  x = 15.15, adding more points doesn’t really change the outcome anymore.

program integration_using_the_simpson_rule
    implicit none

    ! Variable Dictionary
    integer :: i                    ! Counter for looping through subintervals
    integer :: n                    ! Number of subintervals
    real :: s                       ! Step size (width of each subinterval)
    real :: x                       ! Current x position in the interval
    real :: fx                      ! Function value at x
    real :: integral_value          ! This stores the final computed integral
    real :: a = 4000.0, b = 15.15, c = 0.01  ! Constants in the function 
    real :: x_lower = 1.0, x_upper = 20.0    ! Integration bounds

    ! Prompt user for number of subintervals
    write(*,*) "Please enter the number of subintervals (must be even):"
    read(*,*) n

    ! Ensure that the user picked an even number
    if (mod(n, 2) /= 0) then
        write(*,*) "Error: Number of subintervals must be even."
        stop 1
    end if

    ! Calculating the step size
    s = (x_upper - x_lower) / real(n)

    ! Start integral sum with first and last points
    integral_value = ((x_lower + cos(x_lower)) * exp(cos(x_lower)) + &
                      a * exp(-((x_lower - b)**2) / c)) + &
                     ((x_upper + cos(x_upper)) * exp(cos(x_upper)) + &
                      a * exp(-((x_upper - b)**2) / c))

    ! Looping over all internal points and apply Simpson's weights
    do i = 1, n - 1
        x = x_lower + i * s
        fx = (x + cos(x)) * exp(cos(x)) + &
             a * exp(-((x - b)**2) / c)  ! f(x_i)

        if (mod(i, 2) == 0) then
            integral_value = integral_value + 2.0 * fx  ! Even i → ×2
        else
            integral_value = integral_value + 4.0 * fx  ! Odd i → ×4
        end if
    end do

    ! Completing the Simpson's Rule calculation
    integral_value = integral_value * s / 3.0

    ! Displaying the resulting values
    write(*,'(a,f14.6)') "Integral value (Simpson's Rule): ", integral_value
    write(*,'(a,i6)') "Number of subintervals used: ", n

    stop 0
end program integration_using_the_simpson_rule
