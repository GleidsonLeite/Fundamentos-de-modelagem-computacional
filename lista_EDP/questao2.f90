program questao2
  implicit none
  real, dimension(1000, 1000) :: A, C
  real, dimension(1000) :: X, Y, Z, B, rho
  real :: ER, E0, AA, BB, D, DX, DY, DL, R, sum, Q
  real, parameter :: pi = 4.D0*DATAN(1.D0)
  integer :: N, M, NT, K1, K2, K3, I, J, K

  ER = 1.0
  E0 = 8.8541E-12
  AA = 1.0
  BB = 1.0
  D = 1.0
  N = 25
  NT = 2*N
  M = sqrt(real(N, 8))
  DX = AA/M
  DY = BB/M
  DL = DX
  A = 0.0

  K = 0
  do K1 = 1, 2
    do K2 = 1, M
      do K3 = 1, M
        K = K + 1
        X(K) = DX*(K2 - 0.5)
        Y(K) = DY*(K3 - 0.5)
      end do
    end do
  end do

  do K1 = 1, N
    Z(K1) = 0.0
    Z(K1+N) = D
  end do

  do I = 1, NT
    do J = 1, NT
      if ( I==J ) then
        A(I,J) = DL*0.8814/(pi*E0)
      else
        R = sqrt( (X(I)-X(J))**2 + (Y(I)-Y(J))**2 + (Z(I)-Z(J))**2 )
        A(I,J) = DL**2/(4*pi*E0*R)
      end if
    end do
  end do

  do K = 1, N
    B(K) = 1.0
    B(K+N) = -1.0
  end do

  call invert(A, C, NT)
  do i = 1, NT
    rho(i) = 0
    do j = 1, NT
      rho(i) = rho(i)+C(i,j)*B(j)
    end do
  end do

  sum = 0.0

  do i = 1, N
    sum = sum + rho(i)
  end do
  Q = sum*(DL**2)
  print *, abs(Q)/2.0
  C = abs(Q)/2.0
end program questao2

subroutine invert(a, C, n)
  implicit none
  real, dimension(1000,1000) :: a, C, L, U
  real, dimension(1000) :: b, d, x
  real :: coeff
  integer :: i, j, k, n

  L = 0.0
  U = 0.0
  b = 0.0
  do k = 1, n-1
    do i = k+1, n
      coeff = a(i,j)/a(k,k)
      L(i,k) = coeff
      do j = k+1, n
        a(i,j) = a(i,j)-coeff*a(k, j)
      end do
    end do
  end do
  
  do i = 1, n
    L(i,i) = 1.0
  end do

  do j = 1, n
    do i = 1, j
      U(i,j) = a(i,j)
    end do
  end do

  do k = 1, n
    b(k) = 1.0
    d(1) = b(1)
    do i = 2, n
      d(i) = b(i)
      do j = 1, i-1
        d(i) = d(i) - L(i,j)*d(j)
      end do
    end do
    x(n) = d(n)/U(n,n)
    do i = n-1, 1, -1
      x(i) = d(i)
      do j = n, i+1, -1
        x(i)=x(i)-U(i,j)*x(j)
      end do
      x(i) = x(i)/U(i,i)
    end do
    do i = 1, n
      C(i,k) = x(i)
    end do
    b(k) = 0.0
  end do
end subroutine invert
