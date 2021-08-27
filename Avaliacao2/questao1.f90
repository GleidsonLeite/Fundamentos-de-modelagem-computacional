program questao1
  implicit none
  real, dimension(1000) :: x, y
  integer :: n, i
  real :: a, b, h, integra

  open(file='dados.dat', unit=1, status='old')

  read (1, *) n
  
  do i = 1, n
    read(1,*) x(i), y(i)
  end do

  print *, "O valor do deslocamento médio do automóvel é:"
  print *, integra(n, x, y)
  

end program questao1

function integra(n, x, y) result(result)
  implicit none
  real, dimension(1000) :: x, y
  real :: a, b, h, aux
  real :: result
  integer :: i, n
  
  a = x(1)
  b = x(n)
  h = (b-a)/n

  aux = 0.0
  do i = 2, (n-1)
    aux = aux+y(i)
  end do

  result = (h/2)*(y(1)+2*aux+y(n))
end function integra
