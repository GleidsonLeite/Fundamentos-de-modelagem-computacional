program newton
  implicit none
  real, external :: f, derivative1, derivative2
  real :: a, b, x0, tol, deltax
  integer :: niter

  deltax = 1E-1

  print *, "Type the 'a' value"
  read(*,*) a
  print *, "Type the 'b' value"
  read(*,*) b
  print *, "Type a precision"
  read(*,*) tol
  print *, "Type the number of iteractions"
  read(*,*) niter

  call getx0(f, derivative2, deltax, a, b, x0)
  print *, x0

end program newton

function f(x) result(y)
  implicit none
  real :: x
  real :: y
  y = x**2
end function f

function derivative1(f, x, deltax) result(df1dx1)
  implicit none
  real :: f, x, deltax
  real :: df1dx1
  df1dx1 = (f(x+deltax)-f(x-deltax))/(2*deltax)
end function derivative1

function derivative2(f, x, deltax) result(df2dx2)
  implicit none
  real :: f, x, deltax
  real :: df2dx2
  df2dx2 = (f(x+deltax)-2*f(x)+f(x-deltax))/(deltax**2)
end function derivative2

subroutine getx0(f, derivative2, deltax, a, b, x0)
  implicit none
  real :: f, derivative2, deltax, a, b, x0
  if ( f(a)*derivative2(f,a,deltax)>0 ) then
    x0 = a
  else
    x0 = b
  end if
end subroutine getx0