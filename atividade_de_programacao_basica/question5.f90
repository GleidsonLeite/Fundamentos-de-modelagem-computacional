program question5
  implicit none
  integer :: i, number, productValue

  print *, "Insira um valor inteiro positivo"
  read *, number
  
  productValue = 1
  do i = 1, number
    productValue = productValue*i
  end do
  
  print *, productValue
end program question5