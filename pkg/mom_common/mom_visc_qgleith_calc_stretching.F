#include "MOM_COMMON_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_VISC_QGLEITH_CALC_STRETCHING
C     !INTERFACE:
      SUBROUTINE MOM_VISC_QGLEITH_CALC_STRETCHING(
     I                                 bi, bj, k,
     O                                 stretching, Nsq,
     I                                 myThid )

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */
#include "MOM_VISC.h"
#include "DYNVARS.h"

C#include "PACKAGES_CONFIG.h"

C     == Routine arguments ==
      INTEGER i,j,k,bi,bj
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid


C     == Local variables ==

      _RL sigmaR

      INTEGER iMin, iMax
      INTEGER jMin, jMax


      _RL kernel_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy) 
      _RL kernel_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)    
      _RL kernel_3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL rholoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKm2(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKp2(1-Olx:sNx+Olx,1-Oly:sNy+Oly)     

      _RL buoy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_m1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_p1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)


      _RL N2_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL N2_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL N2_3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C Here we go.....


        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          rhoKm1(i,j)   = 0. _d 0
          rhoKm2(i,j)   = 0. _d 0
          rhoKp1(i,j)   = 0. _d 0
          rhoKp2(i,j)   = 0. _d 0
          rholoc(i,j)   = 0. _d 0
          stretching(i,j)   = 0. _d 0
         ENDDO
        ENDDO


        IF (k.EQ.1) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              CALCULATE DENSITIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This is for the surface layer;  Km1 is going to be the top row,
C and I need 3 rows below that

C--     Start computation of dynamics
        iMin = 1-Olx
        iMax = sNx+Olx
        jMin = 1-Oly
        jMax = sNy+Oly

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k,bi,bj),
     O              rhoKm1,
     I              k, bi, bj, myThid )

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+1,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+1,bi,bj),
     O              rholoc,
     I              k+1, bi, bj, myThid )


          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+2,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+2,bi,bj),
     O              rhoKp1,
     I              k+2, bi, bj, myThid )

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+3,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+3,bi,bj),
     O              rhoKp2,
     I              k+3, bi, bj, myThid )


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC    
C              BUOYANCY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       DO j=jMin,jMax
        DO i=iMin,iMax
         buoy(i,j)=-Gravity*recip_rhoConst*rholoc(i,j)
         buoy_m1(i,j)=-Gravity*recip_rhoConst*rhoKm1(i,j)
         buoy_p1(i,j)=-Gravity*recip_rhoConst*rhoKp1(i,j)
        ENDDO
       ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              N^2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C recip_drC is the distance to the cell center...
C At the top row is it the distance from the center to the 
C free surface?  This would make it half of what it is over the other
C cells in the column, and would explain why I'm getting a junk answer

C a small number (1. _d -9) is added to N^2 to prevent divsion by zero

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        sigmaR= maskC(i,j,k,bi,bj)
     &                *0.25*recip_drC(k)*rkSign
     & *(-3*rhoKm1(i,j)+4*rholoc(i,j)-rhoKp1(i,j))
        N2_1(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

        sigmaR= maskC(i,j,k,bi,bj)
     &                *0.25*recip_drC(k)*rkSign
     &                *(rhoKp1(i,j)-rhoKm1(i,j))
        N2_2(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

        sigmaR= maskC(i,j,k,bi,bj)
     &                *0.25*recip_drC(k)*rkSign
     &                *(rhoKp2(i,j)-rholoc(i,j))
        N2_3(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

       ENDDO
      ENDDO

C Take a weighted nearest neighbors average to smooth it out a bit
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
        N2_1(i,j) = (4*N2_1(i,j) + N2_1(i+1,j) +
     &  N2_1(i-1,j) + N2_1(i,j-1) + N2_1(i,j+1))/8.0

        N2_2(i,j) = (4*N2_2(i,j) + N2_2(i+1,j) +
     &  N2_2(i-1,j) + N2_2(i,j-1) + N2_2(i,j+1))/8.0

        N2_3(i,j) = (4*N2_3(i,j) + N2_3(i+1,j) +
     &  N2_3(i-1,j) + N2_3(i,j-1) + N2_3(i,j+1))/8.0


        ENDDO
      ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              (f/N^2) * b
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        kernel_1(i,j)=(fCori(i,j,bi,bj)/N2_1(i,j))*buoy_m1(i,j)
        kernel_2(i,j)=(fCori(i,j,bi,bj)/N2_2(i,j))*buoy(i,j)
        kernel_3(i,j)=(fCori(i,j,bi,bj)/N2_3(i,j))*buoy_p1(i,j)
       ENDDO
      ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              d/dz of it
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx

        stretching(i,j)= maskC(i,j,k,bi,bj)
     &                *0.25*recip_drC(k)*rkSign
     &  *(-3*kernel_1(i,j)+4*kernel_2(i,j)-kernel_3(i,j))

        Nsq(i,j) = N2_1(i,j)     

       ENDDO
      ENDDO






        ELSEIF (k.EQ.Nr) THEN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              CALCULATE DENSITIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This is for the bottom layer;  Kp1 is going to be the bottom row,
C and I need 3 rows above that

C--     Start computation of dynamics
        iMin = 1-Olx
        iMax = sNx+Olx
        jMin = 1-Oly
        jMax = sNy+Oly

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k,bi,bj),
     O              rhoKp1,
     I              k, bi, bj, myThid )

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-1,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-1,bi,bj),
     O              rholoc,
     I              k-1, bi, bj, myThid )


          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-2,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-2,bi,bj),
     O              rhoKm1,
     I              k-2, bi, bj, myThid )

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-3,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-3,bi,bj),
     O              rhoKm2,
     I              k-3, bi, bj, myThid )


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              BUOYANCY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       DO j=jMin,jMax
        DO i=iMin,iMax
         buoy(i,j)=-Gravity*recip_rhoConst*rholoc(i,j)
         buoy_m1(i,j)=-Gravity*recip_rhoConst*rhoKm1(i,j)
         buoy_p1(i,j)=-Gravity*recip_rhoConst*rhoKp1(i,j)
        ENDDO
       ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              N^2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C a small number (1. _d -9) is added to N^2 to prevent divsion by zero

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        sigmaR= maskC(i,j,k,bi,bj)
     &                *0.5*recip_drC(k)*rkSign
     & *(3*rhoKp1(i,j)-4*rholoc(i,j)+rhoKm1(i,j))
        N2_1(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

        sigmaR= maskC(i,j,k,bi,bj)
     &                *0.5*recip_drC(k)*rkSign
     &                *(rhoKp1(i,j)-rhoKm1(i,j))
        N2_2(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

        sigmaR= maskC(i,j,k,bi,bj)
     &                *0.5*recip_drC(k)*rkSign
     &                *(rholoc(i,j)-rhoKm2(i,j))
        N2_3(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

       ENDDO
      ENDDO

C Take a weighted nearest neighbors average to smooth it out a bit
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
        N2_1(i,j) = (4*N2_1(i,j) + N2_1(i+1,j) +
     &  N2_1(i-1,j) + N2_1(i,j-1) + N2_1(i,j+1))/8.0

        N2_2(i,j) = (4*N2_2(i,j) + N2_2(i+1,j) +
     &  N2_2(i-1,j) + N2_2(i,j-1) + N2_2(i,j+1))/8.0

        N2_3(i,j) = (4*N2_3(i,j) + N2_3(i+1,j) +
     &  N2_3(i-1,j) + N2_3(i,j-1) + N2_3(i,j+1))/8.0


        ENDDO
      ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              (f/N^2) * b
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        kernel_1(i,j)=(fCori(i,j,bi,bj)/N2_1(i,j))*buoy_p1(i,j)
        kernel_2(i,j)=(fCori(i,j,bi,bj)/N2_2(i,j))*buoy(i,j)
        kernel_3(i,j)=(fCori(i,j,bi,bj)/N2_3(i,j))*buoy_m1(i,j)
       ENDDO
      ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              d/dz of it
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C I reversed the sign of the derivative here.

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx

        stretching(i,j)= maskC(i,j,k,bi,bj)
     &                *0.5*recip_drC(k)*rkSign
     &  *(3*kernel_1(i,j)-4*kernel_2(i,j)+kernel_3(i,j))

        Nsq(i,j) = N2_1(i,j)

       ENDDO
      ENDDO

      ELSE



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              CALCULATE DENSITIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--     Start computation of dynamics
        iMin = 1-Olx
        iMax = sNx+Olx
        jMin = 1-Oly
        jMax = sNy+Oly

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k,bi,bj),
     O              rholoc,
     I              k, bi, bj, myThid )

          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-1,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-1,bi,bj),
     O              rhoKm1,
     I              k-1, bi, bj, myThid )


          CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+1,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+1,bi,bj),
     O              rhoKp1,
     I              k+1, bi, bj, myThid )


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
C              BUOYANCY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       DO j=jMin,jMax
        DO i=iMin,iMax
         buoy(i,j)=-Gravity*recip_rhoConst*rholoc(i,j)
         buoy_m1(i,j)=-Gravity*recip_rhoConst*rhoKm1(i,j)
         buoy_p1(i,j)=-Gravity*recip_rhoConst*rhoKp1(i,j)


C Interpolate buoyancy to cell borders
         buoy_1(i,j)= 0.5 * (buoy(i,j) + buoy_m1(i,j))
         buoy_2(i,j)= 0.5 * (buoy(i,j) + buoy_p1(i,j))

        ENDDO
       ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              N^2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C a small number (1. _d -9) is added to N^2 to prevent divsion by zero

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        sigmaR= maskC(i,j,k,bi,bj)
     &                *recip_drC(k)*rkSign
     &                *(rholoc(i,j)-rhoKm1(i,j))
        N2_1(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

        sigmaR= maskC(i,j,k,bi,bj)
     &                *recip_drC(k)*rkSign
     &                *(rhoKp1(i,j)-rholoc(i,j))
        N2_2(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9

        sigmaR= maskC(i,j,k,bi,bj)
     &                *0.5*recip_drC(k)*rkSign
     &                *(rhoKp1(i,j)-rhoKm1(i,j))
        N2_3(i,j) = -gravity*recip_rhoConst*sigmaR + 1. _d -9


       ENDDO
      ENDDO

C Take a weighted nearest neighbors average to smooth it out a bit
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
        N2_1(i,j) = (4*N2_1(i,j) + N2_1(i+1,j) + 
     &  N2_1(i-1,j) + N2_1(i,j-1) + N2_1(i,j+1))/8.0 

        N2_2(i,j) = (4*N2_2(i,j) + N2_2(i+1,j) + 
     &  N2_2(i-1,j) + N2_2(i,j-1) + N2_2(i,j+1))/8.0

        N2_3(i,j) = (4*N2_3(i,j) + N2_3(i+1,j) +
     &  N2_3(i-1,j) + N2_3(i,j-1) + N2_3(i,j+1))/8.0


       ENDDO
      ENDDO


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              (f/N^2) * b 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        kernel_1(i,j)=(fCori(i,j,bi,bj)/N2_1(i,j))*buoy_1(i,j)
        kernel_2(i,j)=(fCori(i,j,bi,bj)/N2_2(i,j))*buoy_2(i,j)
       ENDDO
      ENDDO
  

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              d/dz of it
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx

        stretching(i,j)= maskC(i,j,k,bi,bj)
     &                *recip_drC(k)*rkSign
     &                *(kernel_2(i,j)-kernel_1(i,j))

        Nsq(i,j) = N2_3(i,j)

       ENDDO
      ENDDO

      ENDIF
CC End of IF statement about k

      RETURN
      END
