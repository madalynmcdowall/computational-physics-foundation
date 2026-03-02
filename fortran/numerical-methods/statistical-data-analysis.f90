! Purpose: Reads a large data set from a file and calculates the mean, 
! median, and standard deviation. The resulting values will then be
! placed into another file.

program dataset_analysis
  implicit none
  
  ! Variable Dictionary
  real, allocatable :: d(:)        ! Data array
  integer :: lun_in, lun_out       ! LUNs
  character(len = 64) :: input_file, output_file ! Input and output file names
  integer :: n = 0, i, j, ierror, min_position   ! Counters and I/O status
  real :: mean, median, std_dev    ! Variables for mean, median, and std dev
  real :: sort_value               ! Temporary variable for sorting
  
  ! Requesting the input data file name and opening it
  write(*,*) 'Please enter the name of the input file:'
  read(*,*) input_file
  open(newunit=lun_in, file=trim(input_file), status='OLD', iostat=ierror)
  if(ierror /= 0) then
    write(*,*) 'Data file open failed'
    stop 1
  end if
  
  ! Counting the number of points, n, in the input data file
  do
     read(lun_in, *, iostat=ierror)
     if(ierror /= 0) exit
     n = n + 1
  end do

  if(n == 0) then
     write(*,*) 'No data found'
     stop 2
  end if
  
  ! Rewinding and reading the data into an array
  rewind(lun_in)
  allocate(d(n))
  read(lun_in, *) d
  close(lun_in)

  ! Calculating the mean first and the standard deviation second
  mean = sum(d) / n
  std_dev = sqrt(sum((d - mean)**2) / n)
  
  ! Sorting the data using selection sort (as seen on Wikipedia)
  do i = 1, n-1
     min_position = i
     do j = i+1, n
        if(d(j) < d(min_position)) min_position = j
     end do
     sort_value = d(i)
     d(i) = d(min_position)
     d(min_position) = sort_value
   end do
  
  ! Calculating the median
  if(mod(n,2) /= 0) then 
    median = d((n+1)/2)
  else
    median = (d(n/2) + d(n/2+1)) / 2.0
  end if
  
  ! Writing the results into the output data file
  write(*,*) 'Please enter the name of the output file:'
  read(*,*) output_file
  open(newunit=lun_out, file=trim(output_file), status='REPLACE', iostat=ierror)
  if(ierror /= 0) then
    write(*,*) 'Cannot open output file'
    stop 3
  end if
  
  ! Formatting how information is presented in output file      
  write(lun_out,'(a)') 'Data Analysis Results'
  write(lun_out,'(a,i0)') 'Number of Data Points: ', n
  write(lun_out,'(a,f15.8)') 'Mean: ', mean
  write(lun_out,'(a,f15.8)') 'Standard Deviation: ', std_dev
  write(lun_out,'(a,f15.8)') 'Median: ', median
  write(lun_out,'(a/a)') 'Sorted Data:'
  write(lun_out,'(f15.8)') d  ! This will display all of the data
  
  close(lun_out)
  deallocate(d)
  write(*,*) 'The analysis is complete.'
  
  stop 0
end program dataset_analysis
