MODULE Potential
!--------------------------------------------------------------------!
!This module should contain all main subroutines related to potential!
!fields based on Blakely(1995)                                       !
!Author: Victor Carreira                                             !
!--------------------------------------------------------------------!
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SP):: 
  INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:)::
  REAL(KIND=DP)::xq,yq,zq,rho,xp,yp,zp,gx,gy,gz,a
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:)::
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:)::
 
CONTAINS

SUBROUTINE sphere(xq,yq,zq,a,rho,xp,yp,zp,gx,gy,gy)
!This subroutine calculates the three components of
!gravitational atraction at a single point due to a
!uniform sphere of homogeneous density.

!INPUTS PARAMETERS:
! Observations points is (xp,yp,zp), and center of 
!sphere is at (xq,yq,zq). Radius of sphere is "a"
!and the density is "rho". Density in units of 
!kg/m³. All distance parameters in units of km.

!OUTPUTS PARAMETERS:
! Gravitational components (gx,gy,gz) in units of 
!mGal.

   IMPLICIT NONE
    REAL(KIND=DP), INTENT(IN)::xq,yq,zq,a,rho,xp,yp,zp
    REAL(KIND=DP), INTENT(OUT)::gx,gy,gz
    REAL(KIND=DP):: km2m, tmass, rx, ry, rz, r, r3
    REAL(KIND=DP), PARAMETER:: gamma, si2mg, pi, km2m
     !Parameters
     gamma=6.67E-11
     si2mg=1.0E5
     pi=3.14159265
     km2m=1.0E3


     ierror=0
     
     rx=xp-xq
     ry=yp-yq
     rz=zp-zq 
     r=SQRT(rx**2+ry**2+rz**2)
     IF(r .eq. 0.0)PAUSE 'Sphere: Bad argument detected!'
     r3=r**3
     tmass=4.0*pi*rho*(a**3)/3.0
     gx=-gamma*tmass*rx/3.0
     gy=-gamma*tmass*ry/3.0
     gz=-gamma*tmass*rz/3.0
   
END SUBROUTINE






END MODULE Potential
