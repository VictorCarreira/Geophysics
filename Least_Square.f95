PROGRAM MatricialProduct
!!!!!!!!!!!!!!!!Variáveis!!!!!!!!!!!!!!
  IMPLICIT NONE
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:)::A
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:)::B
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:)::C, D, E
  REAL(KIND=8)::inicial, final, DT
  INTEGER::i,j,k,m,n
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!Funções Intrínsecas!!!!!!!!!!
call cpu_time(inicial)
!E = MATMUL(A,B)                                                                !Função de multiplicação matricial. Só é válido para matrizes conformes.


!!!!!!!!!Produto Matricial!!!!!!!!!!!!

!DO i=1,4
!  DO k=1,5
!    C(i,k)=0
    !DO j=1,3
    !  C(i,k)=C(i,k)+A(i,j)*B(j,k)
    !ENDDO
  !ENDDO
!ENDDO



DO i=1,m
  DO j=1,m
      C(i,j)=0
    DO k=1,n
      C(i,j)=C(i,j)+A(i,k)*B(k,j)
    ENDDO
  ENDDO
ENDDO

!!!!!Gerando o arquivo de saída!!!!!!
OPEN(unit=1, file='Produto Matricial')
WRITE(1,*)'A->',A
WRITE(1,*)'B->',B
WRITE(1,*)'A X B = C'
WRITE(1,*)'C->',C
!WRITE(1,*)'A . B = D'
!WRITE(1,*)'D->',D
WRITE(1,*)'A x B = E, prova real'
WRITE(1,*)'E->',E
WRITE(1,*)"Tempo de Máquina =",DT, 'Segundos'
200 format(i3)
close(unit=1, status='keep')


call cpu_time(final)
DT=final-inicial
print '("Tempo = ",f6.3," segundos.")',DT


END PROGRAM MatricialProduct
