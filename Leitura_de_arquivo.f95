 ! programa fortran para ajuste linear de conjunto de dados
PROGRAM Ajuste_reta

 IMPLICIT NONE
 ! ETAPA 01: DEFINIÇÃO DAS VARIAVEIS DO PROGRAMA!
 REAL(KIND=4):: a,b,sa,sb
 REAL(KIND=4), ALLOCATABLE, DIMENSION (:):: x, y
 INTEGER::i,n,ierr ! indice que identifica erros

 ! ETAPA 02: LEITURA DO ARQUIVO DE DADOS (dados.txt) ! Nao esquecer de pular o cabeçalho do arquivo.
 ! Comando para executar arquivo com valores de x e y
 OPEN(UNIT=1,FILE='dados.txt')
 ! Chamando subrotine para pular linha (o cabeçalho):
 CALL pula_linha(1,ierr)
 ! Chamando subrotina para contar o numero de linhas do arquivo
 CALL conta_linha(1,n)
 PRINT*, 'numero de linhas no arquivo =',n

 ! ALLOCAÇÂO :
  ALLOCATE( x(n), y(n) )

 ! Leitura dos dados no arquivo:
 OPEN(UNIT=1,FILE='dados.txt')
 ! Pular o cabeçalho:
 CALL pula_linha(1,ierr)
 DO i=1,n
   READ(UNIT=1,FMT=*,IOSTAT=ierr) x(i), y(i)
   PRINT*, x(i), y(i)
 ENDDO
  CLOSE(UNIT=1)
! Chamar a subrotina que calcula os coeficientes a e b e as incertezas sa e sb:
 CALL fit_ab(x,y,a,b,sa,sb)
 PRINT*,'coeficientes e incertezas a,b,sa,sb =', a,b,sa,sb

! ETAPA 04: Escrever num arquivo os valores de a e b para plotagem em python.
  OPEN(UNIT=2,FILE='params.txt')
  WRITE(2,*) '       a       b      sa     sb'
  WRITE(2,*) a,b,sa,sb

 CONTAINS

!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!

! Subrotina para pular linhas de um arquivo com cabeçalho
SUBROUTINE pula_linha(fid,ierr)
! Skips the next line in a file
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: fid ! file unit number
 INTEGER, INTENT(OUT) :: ierr ! IOSTAT error catching
 !INTEGER, PARAMETER :: LINELEN=8192 ! length of character strings that hold a single line from a file
 !CHARACTER(LEN=LINELEN) :: tline
 READ(UNIT=fid,FMT='(A)',IOSTAT=ierr)        !tline ! leitura em branco pois só queremos pular a linha
END SUBROUTINE pula_linha

!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!

SUBROUTINE conta_linha(fid,nlines)
! Conta o numero de linhas num arquivo:
 IMPLICIT NONE
 INTEGER, INTENT(IN):: fid ! numero de identificação do arquivo
 INTEGER, INTENT(OUT):: nlines ! numero de linhas contidas no arquivo
 INTEGER::ierr
 ! Ler cada linha até que ierr contenha erro (ou seja, seja /= 0):
 nlines = 0
 DO
   CALL pula_linha(fid,ierr)
   IF (ierr/=0) EXIT
   nlines = nlines + 1
 END DO
 ! Close file:
 CLOSE(UNIT=fid)
END SUBROUTINE conta_linha

!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!

SUBROUTINE ler_linha(fid,tline,ierr)
 ! LÊ a proxima linha do arquivo.
 ! If an error occurs, control is returned to the calling routine without any error thrown.
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: fid ! numero de identificação do arquivo
 CHARACTER(LEN=*), INTENT(OUT) :: tline ! a linha lida, em formato de string
 INTEGER, INTENT(OUT) :: ierr ! IOSTAT error catching
 READ(UNIT=fid,FMT='(A)',IOSTAT=ierr) tline
END SUBROUTINE ler_linha

!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!
!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!-----!

SUBROUTINE fit_ab(x,y,a,b,sa,sb)
 IMPLICIT NONE
 REAL(KIND=4), INTENT(IN), DIMENSION(:) :: x, y     ! Valores da tabela de dados
 REAL(KIND=4), INTENT(OUT) :: a, b, sa, sb ! coeficientes angular e linear da reta ajustada e seus respectivas incertezas
 REAL(KIND=4) :: delta, sig2, mi
 INTEGER :: n
 n = SIZE(x,1)  ! numero de dados na tabela

 ! Calculo de delta, mi e sigma:
  delta = REAL(n,KIND=4) * SUM(x*x) - SUM(x)**2
  mi    = ( 1.0 / REAL(n,KIND=4) ) * (SUM(x) )
  sig2 = (1.0 / REAL(n,KIND=4) ) * SUM(x-mi)**2

  PRINT*, mi, SUM(x-mi)

  sa  = sig2 * ( REAL(n,KIND=4) / delta )
  sb  = sig2 * (SUM(x*x) / delta )

 ! Calculo dos coeficientes a, b via minimos quadrados:
 a = ( REAL(n,KIND=4) * ( SUM(x*y) )  -  SUM(x) * SUM(y) ) / ( delta )
 b = ( SUM(x*x) * SUM(y) - SUM(x*y) * SUM(x) ) / ( delta )

END SUBROUTINE fit_ab

END PROGRAM Ajuste_reta
