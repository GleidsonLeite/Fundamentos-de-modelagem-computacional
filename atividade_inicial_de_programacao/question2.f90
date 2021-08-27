! Question 2
! Leia um número que representa o raio de um 
! círculo, calcule o comprimento da circunferência, a 
! área do círculo e apresente os resultados.

program question2
  implicit none
  real, parameter :: pi = 3.14159265358979
  real :: circleRadius
  real :: circleCircumference
  real :: circleArea

  print *, 'Type the circle radius'
  read *, circleRadius
  
  circleCircumference = 2*pi*circleRadius
  circleArea = pi*(circleRadius**2)
  
  print *, 'The circle circumference: ', circleCircumference
  print *, 'The circle area: ', circleArea
  

end program question2