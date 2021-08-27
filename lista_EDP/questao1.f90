program questao1
  implicit none
  real :: v1, v2, v3, v4
  integer :: ni, nx, ny, i, j, k
  real, dimension(1000, 1000) :: v

  v1 = 10.0
  v2 = 100.0
  v3 = 40.0
  v4 = 0.0
  ni = 200
  nx = 16
  ny = 11

  do i = 1, nx
    do j = 1, ny
      v(i,j) = 0
    end do
  end do

  do i = 2, nx-1
    v(i,1) = v1;
    v(i,ny) = v3;
  end do

  do j = 2, ny-1
    v(i,j) = v4;
    v(nx,j) = v2
  end do

  v(1,1) = 0.5*(v1 + v4)
  v(nx,1) = 0.5*(v1 + v2)
  v(1,ny) = 0.5*(v3 + v4)
  v(nx,ny) = 0.5*(v2 + v3)

  do k = 1, ni
    do i = 2, nx-1
      do j = 2, ny-1
        v(i,j) = 0.25*( v(i+1, j) + v(i-1, j) + v(i,j+1) + v(i,j-1) )
      end do
    end do
  end do

  print *, "Value"

  print *, v(6,6)
  print *, v(9,9)
  print *, v(11,6)
  print *, v(9,3)
end program questao1

