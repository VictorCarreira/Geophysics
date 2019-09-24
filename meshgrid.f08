PROGRAM meshgrid
      !------------------------------------------------------------!
      !This module aims to define a 2D and a 3D meshgrid in FORTRAN!
      !------------------------------------------------------------!

      IMPLICIT NONE
     ! PUBLIC
      INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
      INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
      INTEGER(KIND=SP)::nx,ny,nz 
      REAL(KIND=DP), ALLOCATABLE, DIMENSION(:)::x,y,z

      !Inputs são os números de pontos em cada dimensão
      nx=50
      ny=50
      nz=50

      ALLOCATE(x(nx),y(ny),z(nz))

      !testando a função reshape

      WRITE(*,*) shape(x)
      WRITE(*,*) shape(y)
      WRITE(*,*) shape(z)
      
      WRITE(*,*) shape(reshape(x,(/25,25/)))
      WRITE(*,*) shape(reshape(y,(/25,25/)))
      WRITE(*,*) shape(reshape(z,(/25,25/)))











!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE meshgrid2D(x,y,gradeX, gradeY)
      !IMPLICIT NONE 
      REAL(KIND=DP),INTENT(IN), DIMENSION(:):: x, y
      REAL(KIND=DP), INTENT(OUT), DIMENSION(:,:):: gradeX, gradeY
      INTEGER(KIND=SP)::sx,sy,i,j

       sx=size(x)
       sy=size(y)

       ALLOCATE(gradeX(sx,sx), gradeY(sy,sy))

       DO i=1,sx
        DO j=1,sy
          gradeX(i,j)= spread(x,1,sy)
          gradeY(i,j)= spread(y,2,sx)
         END DO
       END DO 


      END SUBROUTINE meshgrid2D



!SUBROUTINE meshgrid3D(xgv, ygv, zgv, X, Y, Z)
!      IMPLICIT NONE
!      REAL(KIND=DP),INTENT(IN):: xgv(:), ygv(:), zgv(:)
!      REAL(KIND=DP),INTENT(OUT):: X(:,:,:), Y(:,:,:), Z(:,:,:)
!      INTEGER(KIND=SP):: sX, sY, sZ, i

!     sX = size(xgv) ; sY = size(ygv) ; sZ = size(zgv)

!      do i=1,sZ
!        X(:,:,i) = spread( xgv, 1, sY )
!        Y(:,:,i) = spread( ygv, 2, sX )
!      end do ! i
!      do i=1,sX
!        Z(i,:,:) = spread( zgv, 1, sY)
!      end do ! i
!END SUBROUTINE meshgrid3D





END PROGRAM meshgrid

