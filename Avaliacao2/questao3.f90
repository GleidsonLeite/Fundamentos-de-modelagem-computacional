program questao3
  implicit none
  real :: V, L, R, i0, t0, i, t, tf, step
  real, external :: didt

  t0 = 0
  i0 = 0
  
  t = t0
  i = i0

  tf = 1
  step = 1E-3


  print *, "Insira o valor da tensão CC"
  read(*,*) V
  print *, "Insira o valor da indutância do indutor"
  read(*,*) L
  print *, "Insira o valor da resistência elétrica"
  read(*,*) R

  call euler(didt, V, L, R, i, t, tf, step)

  print *, "O valor da corrente elétrica no tempo final:"
  print *, i

end program questao3

subroutine euler(didt, V, L, R, i, t, tf, step)
  implicit none
  real :: didt, V, L, R, i, t, tf, step

  do while(t<tf)
    i = didt(V, L, R, i, t)*step + i
    t = t + step
  end do

end subroutine euler

function didt(V, L, R, i, t) result(result)
  implicit none
  real :: V, L, R, i, t
  real :: result

  result = (V-R*i)/L
  
end function didt