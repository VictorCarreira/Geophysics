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
  INTEGER(KIND=SP)::n 
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

IMPLICIT NONE
   INTEGER(KIND=SP),INTENT(IN)::m,n
   INTEGER(KIND=SP)::i
   REAL(KIND=DP), INTENT(IN)::theta
   REAL(KIND=DP):: schmit,pmm,pmm1,fact,somx2,x,pnn,xnorm,fac
   REAL(KIND=DP), PARAMETER::d2rad
   !Parameters
   d2rad=0.017453293
   x=COS(theta*d2rad)
   
   IF(m.lt.0.or.,.gt.n)PAUSE'Schmit: Bad argument detected!'
   pmm=1.0
   
   IF(m.gt.0)THEN
       somx2=SQRT((1.0-x)*(1.0+x))
       fact=1.0
         DO 10 i=1,m
            pmm=-pmm*fact*somx2
            fact=fact+2.0
         10 CONTINUE
    END IF

    IF(n.eq.m)THEN
            schmit=pmm
    ELSE
            pmmp1=x*(2*m+1)*pmm
            IF(n.eq.m+1)THEN
                    schmit=pmmp1
            ELSE
                    DO 11 nn=m+2,n
                    pnn=(x*(2*nn-1)*pmmp1-(nn+m-1)*pmm)/(nn-m)
                    pmm=pmmp1
                    pmmp1=pnn
                    11 CONTINUE
                    schmit=pnn
            END IF          
    END IF
    IF(m.ne.0)THEN
            xnorm=SQRT(2*fac(n-m)/fac(n+m))
            schmit=xnorm*schmit
    END IF


END FUNCTION schmit

!--------------------------------------------------------------------------

REAL FUNCTION fac(n)
!This function calculates the fatorial of n (n!)

IMPLICIT NONE
INTEGER(KIND=SP),INTENT(IN)::n
INTEGER(KIND=SP)::fac,fac2

   IF(n.le.0)PAUSE'fac: Bad argument detected'
   IF(n.eq.0.or.n.eq.1)THEN
           fac=1
   ELSE
           fac=n
           fac2=fac
   30      fac2=fac2-1.0
           fac=fac*fac2
           IF(fac2.gt.2)GO TO 30
   END IF



END FUNCTION fac


!--------------------------------------------------------------------------

SUBROUTINE gbox(x0,y0,z0,x1,y1,z1,x2,y2,z2,rho,g)
!This subroutine computes the vertical attraction of a rectangular prism.
!Sides of prism are parallel to x,y,z axes, and z axis is vertical down.

!INPUT PARAMETERS:
!Observation point is (x0,y0,z0). The prism extends from x1 to x2, from
!y1 to y2, and from z1 to z2 in the x, y, and z directions, respectively.
!Density of prism is rho. All distance parameters in units of km; rho in units
!of kg/m³.

!OUTPUT PARAMETERS:
!Vertical attraction of gravity g, in mGal.

  IMPLICIT NONE
    REAL(KIND=DP), INTENT(IN)::x0,y0,z0,x1,y1,z1,x2,y2,z2,rho
    REAL(KIND=DP), INTENT(OUT)::g
    REAL(KIND=DP),ALLOCATABLE,DIMENSION(:)::isign,x,y,z 
    REAL(KIND=DP), PARAMETER:: gamma,twopi, si2mg, km2m
    
    ALLOCATE(isign(2),x(2),z(2))

    !Parameters
     isign=-1,1
     gamma=6.67E-11
     twopi=6.2831853
     si2mg=1.0E5
     km2m=1.0E3

     x(1)=x0-x1
     y(1)=y0-y1
     z(1)=z0-z1
     x(2)=x0-x2
     y(2)=y0-y2
     z(2)=z0-z2
     sum=0.0
     DO 1 i=1,2
        DO 1 j=1,2
          DO 1 k=1,2
            rijk=SQRT(x(i)**2=y(i)**2+z(i)**2)
            ijk=isign(i)*isign(j)*isign(k)
            arg1=atan2((x(i)*y(j)),(z(k)*rijk))
            IF(arg1.lt.0.0)arg1=arg1+twopi
            arg2=rijk+y(j)
            arg3=rijk+x(i)



END SUBROUTINE gbox































END MODULE potential
