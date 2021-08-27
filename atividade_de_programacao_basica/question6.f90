program question6
  implicit none
  integer :: i, numberOfSequences, n1, n2, result

  print *, "Insira a quantidade de vezes que o algoritmo irá repetir"
  read *, numberOfSequences
  
  n1 = 0
  n2 = 1
  print *, n1
  print *, n2
  do i = 3, numberOfSequences
    result = n1 + n2
    print *, result
    n1 = n2
    n2 = result
  end do
  
end program question6