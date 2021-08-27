! Question 3
! Leia um número que representa o raio de uma 
! esfera, calcule a área da esfera, o volume da 
! esfera e apresente os resultados.

program question3
  implicit none
  real, parameter :: pi = 3.14159265358979
  real :: sphereRadius
  real :: sphereArea
  real :: sphereVolume

  print *, 'Type the sphere radius'
  read *, sphereRadius

  sphereArea = 4*pi*sphereRadius**2
  sphereVolume = (4.0/3)*pi*sphereRadius**3

  print *, 'The sphere Area: ', sphereArea
  print *, 'The sphere volume: ', sphereVolume
end program question3
