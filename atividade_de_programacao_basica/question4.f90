program question4
  implicit none
  integer :: number
  real :: summation, inverseSum

  do while((number <= 1).or.(number >= 100))
    print *, "Informe um numerto entre 1 e 100"
    read *, number
  end do

  print *, "Somatório", summation(number)
  print *, "Somatório dos inversos", inverseSum(number)
end program question4

function summation(number) result(operationResult)
  implicit none
  integer :: number, i
  real :: operationResult
  operationResult = 0
  
  do i = 1, number
    operationResult = operationResult + i
  end do
end function summation

function inverseSum(number) result(operationResult)
  implicit none
  integer :: number, i
  real :: operationResult
  operationResult = 0
  
  do i = 1, number
    operationResult = operationResult + 1./i
  end do
end function inverseSum