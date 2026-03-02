! Purpose: Calculates the sum of a finite series where each item
! is given by Ci * (x^(2i)/(2i)!)!

program series_summation
    implicit none

    ! Variable Dictionary
    integer :: N               ! Upper bound of summation (0<=N<=6)
    integer :: i               ! Loop counter
    real :: x                  ! Input value (x>0)
    real :: sum_val            ! Resulting value of the series
    real :: factorial          ! Running value of (2i)!
    real :: power              ! Running value of x^(2i)
    real :: sign               ! Alternating coefficient Ci = (-1)^i

    ! Prompting the user for input values to go into the loop
    write(*,*) "Please enter a real value for x (> 0):"
    read(*,*) x
    write(*,*) "Please enter an integer value for N (0 <= N <= 6):"
    read(*,*) N

    ! Initializing values before the loop
    sum_val = 0.0              ! Starts with sum=0
    factorial = 1.0            ! (0)!=1 for the first term
    power = 1.0                ! x^(0)=1 for the first term
    sign = 1.0                 ! C0=1 (Ensures the even index starts positive)

    ! Beginning a loop for evaluating the series
    do i = 0, N
        sum_val = sum_val + sign * (power / factorial)

        ! Updating the values for the next iteration
        power = power * x * x
        factorial = factorial * (2*i + 1) * (2*i + 2)
        sign = -sign
    end do

    ! Displaying the result
    write(*,*)
    write(*,'(A,F10.5)') "The value of the sum is: ", sum_val

    stop 0
end program series_summation
