program lista_3
  implicit none
  integer :: m, n
  real, dimension(10,10) :: A, B, C
  call readMatrix(m, n, A, B)
  call sumMatrix(m, n, A, B, C)
  call writeMatrixToFile(m, n, C)
  
end program lista_3

subroutine readMatrix(m, n, A, B)
  implicit none
  integer, intent(out) :: m, n
  real, dimension(10, 10), intent(out) :: A, B
  integer :: i, j
  print *, "Insira o numero de linhas da matriz"
  read *, m
  print *, "Insira o numero de colunas da matriz"
  read *, n
  
  open(file = 'entrada.dat', unit = 1, status = 'old')
  read(1,*)((A(i,j), j = 1,m), i = 1,n )
  read(1,*)((B(i,j), j = 1,m), i = 1,n )
  close(1)
end subroutine readMatrix


subroutine sumMatrix(m, n, A, B, C)
  implicit none
  integer, intent(in) :: m, n
  real, dimension(10, 10), intent(in) :: A, B
  real, dimension(10, 10), Intent(out) :: C
  integer :: i, j

  do i = 1, m
    do j = 1, n
      C(i, j) = A(i, j) + B(i, j)
    end do
  end do
end subroutine sumMatrix

subroutine writeMatrixToFile(m, n, C)
  implicit none
  real, dimension(10, 10), intent(in) :: C
  integer, intent(in) :: m, n
  integer :: i, j
  open(file = 'saida.dat', unit = 1, status = 'unknown')
  
  do i = 1, m
    write(1 ,*)(C(i, j), j = 1, n)
  end do

  close(1)
end subroutine writeMatrixToFile