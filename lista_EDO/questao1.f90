program questao1
  implicit none
  real :: step, tTarget, tInit, iInit, t, i
  real :: c, r
  real, external :: didt
  
  tInit = 0
  iInit = 0
  t = tInit
  i = iInit
  tTarget = 1
  step = 1E-2
  r = 1E3
  c = 1
  
  do while(t<tTarget)
    i = didt(i, r, c)*step + i
    t = t + step
    print *, i, t
  end do

end program questao1

function didt(i, r, c) result(result)
  implicit none
  real :: i, r, c
  real :: result
  result = -i/(r*c)
end function didt