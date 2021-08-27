program questao2
  implicit none
  real, dimension(10000, 10000) :: A
  real, dimension(10000) :: b, x
  integer :: n, i, niter

  n = 3

  print *, "Digite a ordem do sistem de equações:"
  read (*,*) n

  call readA(A, n)
  call readB(b, n)
  call initX(x, n)

  print *, "Insira a quantidade de iterações que serão executadas"
  read (*,*) niter

  do i = 1, niter
    call seidel(n, A, b, x)
  end do

  print *, "Resultado do sistema de equações lineares"
  do i = 1, n
    print *, x(i)
  end do


  ! Pode-se notar que o método do jordan é mais preciso e demanda recursos menores para o processamento
end program questao2

subroutine initX(x, n)
  implicit none
  real, dimension(10000) :: x
  integer :: n, i
  do i = 1, n
    x(i) = 0
  end do
end subroutine initX

subroutine readA(A, n)
  implicit none
  real,dimension(10000, 10000) :: A
  integer :: n, i, j
  print *, "Insira os valores dos elementos do sistema na equação conforme solicitado"
  do i = 1, n
    do j = 1, n
      print *, "Elemento (", i, j, ")"
      read (*,*) A(i, j)
    end do
  end do
end subroutine readA

subroutine readB(b, n)
  implicit none
  real, dimension(10000) :: b
  integer :: n, i
  print *, "Insira os valores dos elementos do vetor de termos independentes da equação"
  do i = 1, n
    print *, "Elemento (",i,")"
    read (*,*) b(i)
  end do
end subroutine readB

subroutine seidel(n, A, b, x)
  implicit none
  real, dimension(10000, 10000) :: A
  real, dimension(10000) :: b, x
  real :: d
  integer :: i, j, n

  do j = 1, n
    d = b(j)
    do i = 1, n
      if ( j.ne.i ) d = d-A(j,i)*x(i)
    end do
    x(j) = d/A(j,j)
  end do
  
end subroutine seidel