program calculos                                                                !inicia o programa
!!!!!!!!!!!!!!!!!!!!!!!!!!!!Definindo o tipo de variávis!!!!!!!!!!!!!!!!!!!!!!!!
implicit none                                                                   !Não especifica nenhum tipo de variável
complex k                                                                       !Diz que k é uma variável complexa
integer n, m, raio, ia, ic, a(5,5)                                                             !Diz que n, m, raio, E e D são variáveis inteiras
real pi, r, area, E, D, x, y                                                    !Diz que pi, x, y, r, area são uma variáveis reais
!!!!!!!!!!!!!Atribuindo valores a variáveis e definindo parâmetros!!!!!!!!!!!!!!
parameter(raio=3)                                                               !Define um nome simbólico para uma constante e não pode ser modificado
parameter(pi=3.14159)
n=1                                                                             !Atribui o valor 1 a variável n
m=0                                                                             !Atribui o valor 3 a variável m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Criando funções!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!k=m+n+pi                                                                       !Define a vairável k como uma somatória
!print*, 'k = ', k                                                              !Imprime o valor de k

!write (*,*) 'Dado o raio r:'                                                   !Escreve no terminal o valor de r
!read (*,*) r                                                                   !lê o valor de r a partir do terminal
!area=4*pi*r*r                                                                  !Cálculo da área de uma esfera
!write (*,*) 'Área da esfera=', area

!write (*,*) 'Dado o número:'
!read (*,*) E
!D=E**(1./2.)                                                                   !Calcula a raíz quadrada. Para o FORTRAN considerar o resto das divisões é necessário acrescentar o ponto depois de cada valor numérico na expressão
!write (*,*) 'Sua raíz quadrada =', D

!write (*,*) 'x='
!read (*,*) x
!y=sin(x)
!write (*,*) 'sen(x)=', y


!write (*,*) 'm='
!read (*,*) m
!if(m .ne. 0)then                                                                !If expandido. Expressão condicional.
!  k=m+n
!  print*,'k=m+n=',k
!elseif (m .eq. 0)then
!  k=(m+n)**2
!  print*, 'k=(m+n)²=',k
!endif

ia=0                                                                            !Cria um Laço Simples para a soma de números inteiros de 1 a 100.
do ic = 1, 2345000, 1
  ia=ia+ic
end do
write(*,*),'Somatório', ia


open(unit=10, file="Treinamento.txt")                                           !Gera um arquivo txt de saída de dados chamado Treinamento
write(10,*)'ia'
write(10,*) ia
200 format(i3)
close(10)


end program calculos                                                            !Termina o programa
