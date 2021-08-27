program questao3
  implicit none
  integer :: i
  real :: media, calculateMedia, calculateMinimalScore, minimalScore, lastScore
  real, dimension(3) :: notas
  do i = 1, 3
    print *, "Insira a nota da prova", i
    read *, notas(i)
  end do
  media = calculateMedia(notas)
  if ( media>=7 ) then
    print *, "Parabéns, você foi aprovado com média", media
  else if( (media<7).and.(media>=5) ) then
    minimalScore = calculateMinimalScore(media)
    print *, "Você está na quarta prova :'("
    print *, "Para passar, você precisará obter a seguinte nota na recuperação", minimalScore
    print *, "Insira o valor da nota obtida na quarta prova"
    read *, lastScore
    media = (media+lastScore)/2
    if ( media>=5 ) then
      print *, "Parabéns, você conseguiu passar :)"
    else
      print *, "Infelizmente, você não conseguiu obter a nota necessária :("
    end if
    print *, "Sua média final foi de ", media
  else
    print *, "Você ficou retido :("
  end if
  
end program questao3

function calculateMedia(notas) result(media)
  implicit none
  real, dimension(3) :: notas
  real :: media, soma
  integer :: i
  soma = 0
  do i = 1, 3
    soma = soma + notas(i)
  end do
  media = soma/3
end function calculateMedia

function calculateMinimalScore(media) result(score)
  implicit none
  real :: media, score
  score = 10-media
end function calculateMinimalScore