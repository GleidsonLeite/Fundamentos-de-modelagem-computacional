program Euler
  implicit none
  real :: step, xtarget, yInit, xInit, x, y
  real, external :: dfdx
  xInit = 0
  yInit = 2
  x = xInit
  y = yInit
  xtarget = 1
  step = 1E-1

  do while(x<xtarget)
    y = dfdx(x,y)*step + y
    x = x + step
    print *, x, y
  end do
  
end program Euler

function dfdx(x, y) result(result)
  implicit none
  real :: x, y
  real :: result
  result = x-y+2
end function dfdx