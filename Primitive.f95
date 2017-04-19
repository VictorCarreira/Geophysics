program calculus                                                                !inicia o programa calculus
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!Definindo o tipo de variávis!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none                                                                   !Não especifica nenhum tipo de variável
  complex k                                                                       !Diz que k é uma variável complexa
  integer n, m, raio, ia, ic, a(5,5), i, j, dia                                  !Diz quais são as variáveis inteiras
  real pi, r, area, E, D, x, y, radseg, fatseg, nivrad, radmin, final, inicial, DT!Diz quais são variáveis reais
  !!!!!!!!!!!!!Atribuindo valores a variáveis e definindo parâmetros!!!!!!!!!!!!!!
  parameter(raio=3)                                                               !Define um nome simbólico para uma constante e não pode ser modificado
  parameter(pi=3.14159)
  parameter(m=7)
  parameter(n=3)                                                                  !Atribui o valor 3 a variável m
  parameter (radseg=0.466, fatseg=10.0)                                           !Atribui o valor às variáveis radseg e fatseg
  dia=0

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Criando funções!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!Exemplo 1 INPE
  !k=m+n+pi                                                                       !Define a vairável k como uma somatória
  !print*, 'k = ', k                                                              !Imprime o valor de k

  !!!!!!!!!!!!!Exemplo 2 INPE
  !write (*,*) 'Dado o raio r:'                                                   !Escreve no terminal o valor de r
  !read (*,*) r                                                                   !lê o valor de r a partir do terminal
  !area=4*pi*r*r                                                                  !Cálculo da área de uma esfera
  !write (*,*) 'Área da esfera=', area

  !!!!!!!!!!!!!Exemplo 3 INPE

  !write (*,*) 'Dado o número:'
  !read (*,*) E
  !D=E**(1./2.)                                                                   !Calcula a raíz quadrada. Para o FORTRAN considerar o resto das divisões é necessário acrescentar o ponto depois de cada valor numérico na expressão
  !write (*,*) 'Sua raíz quadrada =', D


  !!!!!!!!!!!!!Exemplo 4 INPE

  !write (*,*) 'x='
  !read (*,*) x
  !y=sin(x)
  !write (*,*) 'sen(x)=', y


  !!!!!!!!!!!!!Exemplo 5 INPE

  !write (*,*) 'm='
  !read (*,*) m
  !if(m .ne. 0)then                                                                !If expandido. Expressão condicional.
  !  k=m+n
  !  print*,'k=m+n=',k
  !elseif (m .eq. 0)then
  !  k=(m+n)**2
  !  print*, 'k=(m+n)²=',k
  !endif

  !!!!!!!!!!!!!Exemplo 6 INPE

  !ia=0                                                                            !Cria um Laço Simples para a soma de números inteiros de 1 a 100.
  !do ic = 1, 2345000, 1                                                           !Varia de 1 até 2345000 em intervalos de 1 em 1. Faça (do)
  !  ia=ia+ic                                                                      !Adiciona o valor de ic a soma (comando)
  !end do
  !write(*,*),'Somatório', ia


  !!!!!!!!!!!!!Exemplo 7 INPE

  !open(unit=10, file="Treinamento.txt")                                           !Gera um arquivo txt de saída de dados chamado Treinamento
  !write(10,*)'ia'
  !write(10,*) ia
  !200 format(i3)
  !close(unit=10)

  !!!!!!!!!!!!!Exemplo 8 INPE

  !do 10 i=1,m                                                                     !Laços Aninhados
  !  print*, 'primeiro do', i
  !  do 20 j=1,n
  !  print*, 'segundo do', i,j
  !  20 continue
  !  10 continue

  !!!!!!!!!!!!!Exemplo 9 INPE

  !open(unit=0, file="laco.txt")                                                   !Gera um arquivo de saída dados em txt do Exemplo 8.
  !write(0,*)'   i        j'
  !write(0,*) i,j
  !200 format(i3)
  !close(unit=0,status='keep')

  !!!!!!!!!!!!!Exemplo 10 INPE
  call cpu_time(inicial)
  print*, 'Entre com o nível de radiação do dia'                                  !Este programa calcula o nivel de radiação e o grau de segurança!
  read*, nivrad                                                                   !o programa utiliza o comando do while, que é utilizado para
  print*,'número de dias de radiação'                                             !executar um bloco repetidas vezes enquanto a condição for
  radmin=radseg/fatseg                                                            !verdadeira. A partir do momento em que a condicao passa a ser
  !falsa, este sai do loop continuando a execucao do restante do
  !programa.

  do while (nivrad .gt. radmin)                                                   !Executa as instruções dentro do laço até que a condição seja satisfeita
    if (nivrad .gt. radseg) then
      print*, dia, nivrad, 'inseguro'
    else
      print*, dia, nivrad, 'seguro'
    end if
    dia=dia+3
    nivrad=nivrad/2.0
  end do

  call cpu_time(final)
  DT=final-inicial
  print '("Tempo = ",f6.3," segundos.")',DT

  open(unit=1, file='radiation level')
  write(1,*)'número de dias de radiação'
  write(1,*)dia, nivrad, radmin, radseg, fatseg, DT
  200 format(i3)
  close(unit=1, status='keep')

end program calculus                                                            !Termina o programa calculus
