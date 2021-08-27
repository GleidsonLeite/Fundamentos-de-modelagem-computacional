! Question 4

! Leia três números que representam os lados de 
! um triângulo qualquer, calcule a área do triângulo e 
! apresente o resultado.

program question4
  implicit none
  real :: side1
  real :: side2
  real :: side3
  real :: area
  real :: perimeter

  print *, 'Type the length of triangle side 1'
  read *, side1

  print *, 'Type the length of triangle side 2'
  read *, side2

  print *, 'Type the length of triangle side 3'
  read *, side3

  perimeter = (side1+side2+side3)/2
  
  ! Heron Formula
  area = sqrt(perimeter*(perimeter-side1)*(perimeter-side2)*(perimeter-side3))
  print *, 'The triangle area: ', area
end program question4