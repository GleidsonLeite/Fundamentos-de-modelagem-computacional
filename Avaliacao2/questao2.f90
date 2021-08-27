program questao2
  implicit none
  real, dimension(3, 3) :: A
  real, dimension(3) :: b, x
  integer :: n, i

  n = 3

  data (A(1,i), i=1,3) /  3.0,  2.0,  -1.0 /
  data (A(2,i), i=1,3) /  1.0,  3.0,  1.0 /
  data (A(3,i), i=1,3) /  2.0,  2.0,  -2.0 /

  data (b(i), i=1,3) /  0.0,  1.0,  2.0 /

  call jordan(n, A, b, x)

  print *, "Resultado do sistema de equações lineares"
  do i = 1, 3
    print *, x(i)
  end do
end program questao2

subroutine jordan(n, A, b, x)
  implicit none
  real, dimension(3, 3) :: A
  real, dimension(3) :: b, x
  real :: m
  integer :: i, j, k, n

  do k = 1, n
    do i = 1, n
      if ( i .ne. k ) then
        m = A(i, k)/A(k, k)
        A(i, k) = 0.0
        do j = k+1, n
          A(i, j) = A(i, j) - m*A(k, j)
        end do
        b(i) = b(i) - m*b(k)
      end if
    end do
  end do

  do i = 1, n
    x(i) = b(i)/A(i, i)
  end do
end subroutine jordan