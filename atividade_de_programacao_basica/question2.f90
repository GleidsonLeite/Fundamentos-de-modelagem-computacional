program question2
  implicit none
  real, dimension (3) :: numbers
  real :: aux
  integer :: i, j
  ! Read numbers
  do i = 1, 3
    print *, "Type a number for position ", i
    read *, numbers(i)
  end do
    
  aux = 0
  do i = 1, 2
    do j = 2, 3
      if ( numbers(i)>numbers(j) ) then
        aux = numbers(i)
        numbers(i) = numbers(j)
        numbers(j) = aux
      end if
    end do
  end do

  print *, "Numbers in ascending order:"
  do i = 1, 3
    print *, numbers(i)
  end do
end program question2