! Purpose: Solve a 2D Poisson equation for two charges in a 10cm x 10cm box.
!
! Answer: The electrical potential at cell (25,50) is approximately
! -0.029845 statVolts. Moreover, the program took 2592 iterations to converge.
!
! The Gnuplot commands used to generate the heat map:
! set terminal png
! set output "poisson.png"
! plot "poisson_problem.dat" using 1:2:3 with image

program poisson_equation
    implicit none

    ! Variable Dictionary
    integer, parameter :: NX=100, NY=100    ! No. cells in x- and y-direction
    real, parameter :: H=0.1                ! Cell size (cm)
    real, parameter :: PI=3.141592654       ! Constant value of pi
    real, dimension(0:NY+1,0:NX+1) :: u     ! Old est. of potential
    real, dimension(0:NY+1,0:NX+1) :: unew  ! Updated est. of potential
    real, dimension(0:NY+1,0:NX+1) :: q     ! Array for electric charge density
    real :: max_change=1.0                  ! Maximum change per iteration
    integer :: niter=0                      ! No. iterations
    integer :: i, j                         ! Loop indices
    real :: x, y                            ! Cell center coordinates
    integer :: lun1                         ! LUN for I/O

    u = 0.0
    unew = u
    q = 0.0
    q(25,25) = -4.0  ! Negative charge at (25,25)
    q(75,75) =  4.0  ! Positive charge at (75,75)

    ! Utilizing an iteration loop
    do while (max_change > 1.0e-5)
        do i=1,NY
            do j=1,NX
                unew(i,j) = (u(i+1,j) + u(i-1,j) + u(i,j+1) + u(i,j-1) &
                            + 4.0 * PI * (H**2) * q(i,j)) / 4.0
            end do
        end do
        
        max_change = maxval(abs(u - unew))
        
        if (mod(niter,200) == 0) then
            write(*,'(a,i6,a,es12.5)') ' Iteration ', niter, &
                ', max change = ', max_change
        end if
        
        u = unew
        niter = niter + 1
    end do

    ! Displaying the final results
    write(*,*) 'It took ', niter, ' iterations to converge'
    write(*,'(a,f10.6,a)') 'The electrical potential at (25,50) = ', &
        unew(25,50), ' statVolts'
    
    ! Writing out the potential in Gnuplot format
    open(newunit=lun1, file='poisson_problem.dat', action='WRITE', &
         status='replace')
    
    x = 0.5*H
    do j=0,NX+1
        y = 0.5*H
        do i=0,NY+1
            write(lun1,*) x, y, unew(i,j)
            y = y + H
        end do
        write(lun1,*) ' '
        x = x + H
    end do
    
    close(unit=lun1)
    
    stop 0
end program poisson_equation
