! Questão 1
! Leia um número e escreva seu valor, sua raiz quadrado e seu quadrado

program question1
  implicit none
  real number, numberPoweredBy2
  complex numberSquareRoot

  print *, "Type a number and i will give you it square root and the power of 2"
  read *, number

  numberPoweredBy2 = number**2
  numberSquareRoot = number**0.5

  print *, "Number powered by 2: ", numberPoweredBy2
  print *, "Number square root: ", numberSquareRoot

end program question1