program question1
  implicit none
  real :: side1, side2, side3
  character(len=20) :: triangleType
  print *, 'Type the length of triangle side 1'
  read *, side1

  print *, 'Type the length of triangle side 2'
  read *, side2

  print *, 'Type the length of triangle side 3'
  read *, side3

  if ( (side1==side2).and.(side2==side3) ) then
    triangleType = "Equilateral"
  else if ( (side1/=side2).and.(side2/=side3).and.(side3/=side1) ) then
    triangleType="Scalene"
  else
    triangleType="Isosceles"
  end if
  
  print *, "This triangle is a ", triangleType, "'s type"
end program question1