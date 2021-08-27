program questao2
  implicit none
  real :: step, tTarget, tInit, iInit, t, i, z, f
  real :: c, l
  real, external :: dzdt
  
  tInit = 0
  iInit = 0
  t = tInit
  i = iInit
  tTarget = 1
  step = 1E-2
  l = 1E-3
  c = 1E-6
  z = 0
  
  do while(t<tTarget)
    i = i + step*z
    z = dzdt(i, l, c)*step + z
    t = t + step
    print *, i, t
  end do

end program questao2

function dzdt(i, l, c) result(result)
  implicit none
  real :: i, l, c
  real :: result
  result = -i/(l*c)
end function dzdt