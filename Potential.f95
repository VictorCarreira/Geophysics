MODULE potential
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
  REAL(KIND=DP)::xq,yq,zq,rho,xp,yp,zp,gx,gy,gz,ra
  REAL(KIND=DP)::incl,decl,azim,a,b,c,mi,md,m,bx,by,bz
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:)::
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:)::
 
CONTAINS

!---------------------------------------------------------------------!

SUBROUTINE sphere(xq,yq,zq,ra,rho,xp,yp,zp,gx,gy,gy)
!This subroutine calculates the three components of
!gravitational atraction at a single point due to a
!uniform sphere of homogeneous density.

!INPUTS PARAMETERS:
! Observations points is (xp,yp,zp), and center of 
!sphere is at (xq,yq,zq). Radius of sphere is "ra"
!and the density is "rho". Density in units of 
!kg/m³. All distance parameters in units of km.

!OUTPUTS PARAMETERS:
! Gravitational components (gx,gy,gz) in units of 
!mGal.

   IMPLICIT NONE
    REAL(KIND=DP), INTENT(IN)::xq,yq,zq,ra,rho,xp,yp,zp
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
     tmass=4.0*pi*rho*(ra**3)/3.0
     gx=-gamma*tmass*rx/3.0
     gy=-gamma*tmass*ry/3.0
     gz=-gamma*tmass*rz/3.0
   
END SUBROUTINE

!------------------------------------------------------------------!

SUBROUTINE cylinder(xq,zq,ra,rho,xp,zp,gx,gz)
!This subroutine calculates x and z components of gravitational 
!attraction due to an infinitely extended cylinder lying parallel
!to y axis. X and z components of gravitational attraction are 
!returned. 

!INPUT PARAMETERS:
! Points of observations is (xp,zp). Axis of cylinder parameters
!x,z plane at (xq,zq). Radius of cylinder is "ra" and density is "rho".
!Density in Kg/m³. All distance parameters are in km.

!OUTPUTS PARAMETERS:
! Components of gravitational attraction (gx,gz) in mGal.

    IMPLICIT NONE
      REAL(KIND=DP), INTENT(IN)::xq,zq,ra,rho,xp,zp
      REAL(KIND=DP), INTENT(OUT)::gx,gz
      REAL(KIND=DP):: km2m, tmass, rx, rz, r2
      REAL(KIND=DP), PARAMETER:: gamma, si2mg, pi, km2m
     !Parameters
      gamma=6.67E-11
      si2mg=1.0E5
      pi=3.14159265
      km2m=1.0E3
      
      r2=rx**2+rz**2
      IF(r2 .eq. 0.0)PAUSE 'Cylinder: Bad argument detected!'
      tmass=pi*(ra**2)*rho
      gx=2.0*gamma*tmass*rx/r2
      gz=2.0*gamma*tmass*rz/r2
      gx=gx*si2mg*km2m
      gz=gz*si2mg*km2m

END SUBROUTINE cylinder

!-------------------------------------------------------------------!

SUBROUTINE dircos(incl,decl,azim,a,b,c)
!This subroutine computes direction cosines from inclination and
!declination.

!INPUT PARAMETERS:
!incl: inclination in degrees positive below horizontal.
!decl: declination in degrees positive east of true north.
!azim: azimuth of x axis in degrees positive east of north.

!OUTPUT PARAMETERS:
!a,b,c: the three direction cosines.

 IMPLICIT NONE
    REAL(KIND=DP), INTENT(IN)::incl,decl,azim
    REAL(KIND=DP), INTENT(OUT)::a,b,c
    REAL(KIND=DP):: xincl,xdecl,xazim
    REAL(KIND=DP), PARAMETER:: d2rad
     !Parameters
     d2rad=0.017453293
     
     xincl=incl*d2rad
     xdecl=decl*d2rad
     xazim=azim*d2rad
     a=COS(xincl)*COS(xdecl-xazim)
     b=COS(xincl)*SIN(xdecl-xazim)
     c=SIN(xincl)

END SUBROUTINE dircos




!--------------------------------------------------------------------!

SUBROUTINE dipole(xq,yq,zq,ra,mi,md,m,xp,yp,zp,bx,by,bz)
!This subroutine computes the three components of magnetic induction 
!caused a uniformly magnetized sphere. X axis is north, Z axis is 
!donw. 

!INPUT PARAMETERS:
! Observation point located at (xp,yp,zp). Shpere centered at (xq,yq,zq).
!Magnetization of sphere defined by intensity "m", inclination "mi",
!and declination "md". Units of distance irrelevant but must be consistent.
!All angles in degrees. Intensity of magnetization in A/m. Requires
!subroutine DIRCOS.

!OUTPUT PARAMETERS:
! The three components of magnetic induction (bx,by,bz) in units of nT.

 IMPLICIT NONE
    REAL(KIND=DP), INTENT(IN)::xq,yq,zq,ra,mi,md,m,xp,yp,zp
    REAL(KIND=DP), INTENT(OUT)::bx,by,bz
    REAL(KIND=DP):: mx,my,mz,moment,rx,ry,rz,r2,r5,dot
    REAL(KIND=DP), PARAMETER:: pi,t2nt,cm
     !Parameters
     pi=3.14159265
     t2nt=1.0E9
     cm=1.0E-7
    CALL dircos(mi,md,0.0,mx,my,mz)
     rx=xp-xq
     ry=yp-yq
     rz=zp-zq
     r2=rx**2+ry**2+rz**2
     r=SQRT(r2)
     IF(r .eq. 0.0)PAUSE 'Dipole: Bad argument detected!'
     r5=r**5
     dot=rx*mx+ry*my+rz*mz
     moment=4.0*pi*(ra**3)*m/3.0
     bx=cm*moment*(3.0*dot*rx-r2*mx)/r5
     by=cm*moment*(3.0*dot*ry-r2*my)/r5
     bz=cm*moment*(3.0*dot*rz-r2*mz)/r5
     bx=bx*t2nt
     by=by*t2nt
     bz=bz*t2nt

END SUBROUTINE dipole


!---------------------------------------------------------------------------!

REAL(KIND=DP) FUNCTION schmit(n,m,theta)
!This function return schmidt normalized Legendre polynomial.
!Requires function fac. Based on Press et al.(1986)

!INPUT PARAMETERS:
! Argument of polynomial is "theta", in degrees. Degrees and order of polynomial
!are "n" and "m", respectively. Parameter and "n" must be greater than zero, and
!"m" must be greater than or equal to n.


   REAL(KIND=DP), INTENT(IN)::n,m,theta
   REAL(KIND=DP):: 
   REAL(KIND=DP), PARAMETER::d2rad
   !Parameters
   d2rad=0.017453293





END FUNCTION schmit



END MODULE potential
