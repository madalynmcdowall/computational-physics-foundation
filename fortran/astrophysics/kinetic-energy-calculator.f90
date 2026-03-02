! Purpose: To compute the kinetic energy of a particle using both the
!          non-relativistic and relativistic formulas.
!
! Formulas we'll be focusing on:
!   Non-relativistic:
!      T_NR = (1/2) * m * v^2
!
!   Relativistic:
!      T_R = m * c^2 * (gamma - 1)
!      where gamma = 1 / sqrt(1 - v^2/c^2)
!
! Numerical Stability:
!   - Directly computing (gamma - 1) can cause cancellation
!     when v << c. Instead, this program will use the rearranged stable form:
!        T_R = m * v^2 / ( sqrt(1 - beta^2) * (1 + sqrt(1 - beta^2)) )
!     where beta = v/c.
!
! Discussion:
!   - In the non-relativistic limit (v << c), the two formulas
!     agree: T_R ≈ T_NR. This is because relativistic effects are
!     negligible at low speeds, and the Lorentz factor gamma is nearly 1.
!   - For higher speeds approaching the speed of light, relativistic corrections 
!     become visible. Moreover, corrections become significant, and T_R differs 
!     from T_NR.
!
! Restrictions to this program::
!   - This program uses single precision REAL variables only (no double/quad 
!     precision).

program kinetic_energy
  implicit none

  ! Variable Dictionary
  real :: m                                   ! Mass of the particle (kg)
  real :: v                                   ! Velocity of the particle (m/s)
  real :: c                                   ! Speed of light (m/s)
  real :: beta                                ! Dimensionless ratio (v/c) used in 
                                              ! relativistic calculations
  real :: sqrtterm                            ! Will be used as a temporary variable 
                                              ! for sqrt(1 - beta^2)
  real :: T_NR                                ! Non-relativistic kinetic energy (J)
  real :: T_R                                 ! Relativistic kinetic energy (J)
  real, parameter :: c_const = 2.99792458e8   ! The known value of the speed of light (m/s)

  ! User input
  print *, 'Enter particle mass (kg):'        ! Prompting the user to provide the 
                                              ! particle's mass
  read (*,*) m
  print *, 'Enter particle speed (m/s):'      ! Prompting the user to provide the 
                                              ! particle's velocity
  read (*,*) v

  ! Input validation
  if (v < 0.0) then                           ! Check if velocity is a negative number
     print *, 'Error: Speed must be non-negative.'
     stop                                     ! Stops the program if the input is invalid
  end if

  c = c_const                                 ! Assigning the constant value of the speed of 
                                              ! light to variable c
  
  if (v >= c) then                            ! Checks if the velocity exceeds the speed of light
     print *, 'Warning: v >= c (not physically allowed).'
     print *, 'Computation will still proceed, but results are invalid.'
  end if

  ! Non-relativistic kinetic energy calculation
  T_NR = 0.5 * m * v * v                      ! Classical formula: T = 1/2 m v^2

  ! Preparing to calculate the relativistic kinetic energy
  beta = v / c                                ! Compute the dimensionless ration v/c

  if (beta >= 1.0) then                       ! If beta >= 1, the speed is non-physical
     sqrtterm = 0.0                           ! This will help avoid sqrt of a negative number
  else
     sqrtterm = sqrt(1.0 - beta*beta)         ! Compute sqrt(1 - (v/c)^2)
  end if

  ! Relativistic kinetic energy (stable form)
  if (beta >= 1.0) then                       ! Hand non-physical speeds
     print *, 'Error: Speed exceeds or equals the speed of light. Results invalid.'
     T_R = huge(1.0)                          ! Return a very large number to indicate error
  else                         
     T_R = m * v * v / ( sqrtterm * (1.0 + sqrtterm) ) ! Numerically stable formula to avoid 
                                                       ! cancellation for small v
  end if

  ! Output results
  print *
  print '(A)', 'Results (single precision):'
  write(*,'(A,E15.6,A)') '  Non-relativistic T_NR = ', T_NR, ' J'    ! Print classical KE
  write(*,'(A,E15.6,A)') '  Relativistic   T_R  = ', T_R,  ' J'      ! Print relativistic KE

  ! Check agreement in non-relativistic limit
  if (abs(T_R - T_NR) / max(1.0e-30, T_NR) < 1.0e-6) then
     print '(A)', '  Note: the two energies agree (within tolerance) in the non-relativistic limit.'
  else
     print '(A)', '  Note: the two energies differ (relativistic corrections visible).'
  end if

end program kinetic_energy
