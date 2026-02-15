! Purpose: Use Simpson's Rule to evaluate a definite integral 
! using modules, subroutines, user-defined function subprograms, 
! and double precision.
!
! Convergence Test:
!   n       Result
!   2       184.4873930847048200
!   4       9528.2622508502918208
!   6       189.7660769526808906
!   8       2615.5138030523257839
!   16      1451.3695456217747051
!   32      867.1370600693569486
!   64      608.7184468633155348
!   128     1066.2405789991867096
!   256     996.1044475536299387
!   300     993.8786781577213105
!   350     993.6834622450243160
!   400     993.6235177516869044
!   450     993.6319005488378480
!   512     993.6314827138287455
!   550     993.6314779728894564
!   600     993.6314780633483679
!   650     993.6314780026071958
!
! The integral converges to approximately 993.631, with 
! about 400 subintervals needed.
!
! Compared to Assignment 6 where single precision was used, 
! the value in which the integral converges is the same, but 
! double precision converges more smoothly and consistently. 
! For example, at n = 256, single precision gave 1023.939941, 
! while double precision gave 996.104447, which shows improved 
! stability.


module integral_module
  implicit none

  ! Variable Dictionary
  integer, parameter :: dp = kind(1.0d0) ! Double precision kind
  real(kind=dp), parameter :: A = 4000.0d0, B = 15.15d0, C = 0.01d0

contains

  subroutine limits_of_integration(x_low, x_upp)
    implicit none

    ! Variable Dictionary
    real(kind=dp), intent(out) :: x_low, x_upp
    x_low = 1.0d0
    x_upp = 20.0d0
    
  end subroutine limits_of_integration

  function integrand(x_in)
    implicit none

    ! Variable Dictionary
    real(kind=dp), intent(in) :: x_in
    real(kind=dp) :: integrand
    real(kind=dp) :: expo_temp

    expo_temp = -((x_in - B)**2) / C
    integrand = (x_in + cos(x_in)) * exp(cos(x_in)) + A * exp(expo_temp)
  end function integrand

end module integral_module

program dp_simpsons_rule
  use integral_module
  implicit none

  ! Variable Dictionary
  real(kind=dp) :: xl, xh      ! Integration limits
  real(kind=dp) :: h           ! Step size
  real(kind=dp) :: x_left, x_mid, x_right
  real(kind=dp) :: integral_value = 0.0d0
  integer :: n, i      ! No. subintervals and loop index

  ! Getting the integration limits from the subroutine
  call limits_of_integration(xl, xh)

  ! Prompting the user for the subinterval count
  write(*,*) "Please enter the number of subintervals n:"
  read(*,*) n

  ! Checking the n value the user gave
  if (mod(n,2) /= 0 .or. n <= 0) then
     write(*,*) "Error: n must be a positive even integer."
     stop 1
  end if

  ! Computing the subinterval width for Simpson's Rule
  h = (xh - xl) / real(n, kind=dp)
	
  ! Implementing Simpson's Rule
  do i = 0, n - 2, 2
     x_left  = xl + real(i, kind=dp) * h
     x_right = xl + real(i + 2, kind=dp) * h
     x_mid   = 0.5d0 * (x_left + x_right)
	
     integral_value = integral_value + (1.0d0 / 6.0d0) * &
          (integrand(x_left) + 4.0d0 * integrand(x_mid) + &
          integrand(x_right)) * (x_right - x_left)
  end do

  ! Displaying the results
  write(*,'(a, f24.16)') "Integral value: ", integral_value
  write(*,'(a, i8)') "Number of subintervals used: ", n

  stop 0
end program dp_simpsons_rule
