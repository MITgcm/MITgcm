#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_VISC_QGL_STRETCH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_VISC_QGL_STRETCH(
     I                                 bi, bj, k,
     O                                 stretching, Nsq,
     I                                 myThid, myTime, myIter )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE MOM_VISC_QGL_STRETCH
C     | Calculates the stratification (Nsq) and vortex stretching
C     | terms for the Quasi-Geostrophic implementation of Leith 
C     | dynamic viscosity.
C     *==========================================================*

C !USES: ===============================================================
      IMPLICIT NONE
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


C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  myThid               :: my Thread Id number
C  myTime               :: current time of simulation ( s )
C  myIter               :: current iteration number of simulation
      INTEGER bi,bj, k
      INTEGER myThid, myTime, myIter

C !OUTPUT PARAMETERS: ==================================================
C  stretching           :: vortex stretching contribution
C  Nsq                  :: vertical stratification
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)


C !LOCAL VARIABLES: ====================================================
      _RL sigmaR

      INTEGER i, j
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER k_offset

      LOGICAL skip_loc

      _RL QGL_epsil

      _RL kernel_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kernel_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kernel_3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL rholoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKm1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKp1(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKm2(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoKp2(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rho_single_cell

      _RL buoy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_m1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_p1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)


      _RL N2_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL N2_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL N2_3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

CEOP

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        kernel_1(i,j)   = 0. _d 0
        kernel_2(i,j)   = 0. _d 0
        kernel_3(i,j)   = 0. _d 0
        rhoKm1(i,j)     = 0. _d 0
        rhoKm2(i,j)     = 0. _d 0
        rhoKp1(i,j)     = 0. _d 0
        rhoKp2(i,j)     = 0. _d 0
        rholoc(i,j)     = 0. _d 0
        buoy(i,j)       = 0. _d 0
        buoy_m1(i,j)    = 0. _d 0
        buoy_p1(i,j)    = 0. _d 0
        buoy_1(i,j)     = 0. _d 0
        buoy_2(i,j)     = 0. _d 0
        N2_1(i,j)       = 0. _d 0
        N2_2(i,j)       = 0. _d 0
        N2_3(i,j)       = 0. _d 0
        stretching(i,j) = 0. _d 0
        Nsq(i,j)        = 0. _d 0
       ENDDO
      ENDDO

      skip_loc = .FALSE.
      QGL_epsil = 1. _d -12
      k_offset = 0
      rho_single_cell = 0. _d 0



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              CALCULATE DENSITIES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

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

C       Bathymetry and iceshelves can modify where the ocean is,
C       but we know that there is no ocean above k=1 or below
C       k=Nr. Only evaluate those levels when they exist.

      IF (k.GT.1) THEN
        CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-1,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-1,bi,bj),
     O              rhoKm1,
     I              k-1, bi, bj, myThid )
      ENDIF


      IF (k.LT.Nr) THEN
        CALL FIND_RHO_2D(
     I              iMin, iMax, jMin, jMax, k,
     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+1,bi,bj),
     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+1,bi,bj),
     O              rhoKp1,
     I              k+1, bi, bj, myThid )
      ENDIF

C      IF (k.GT.2) THEN
C        CALL FIND_RHO_2D(
C     I              iMin, iMax, jMin, jMax, k,
C     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-2,bi,bj),
C     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k-2,bi,bj),
C     O              rhoKm2,
C     I              k-2, bi, bj, myThid )
C      ENDIF

C      IF (k.LT.Nr-1) THEN
C        CALL FIND_RHO_2D(
C     I              iMin, iMax, jMin, jMax, k,
C     I              theta(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+2,bi,bj),
C     I              salt(1-OLx:sNx+Olx,1-OLy:sNy+Oly,k+2,bi,bj),
C     O              rhoKp2,
C     I              k+2, bi, bj, myThid )
C      ENDIF


        DO j=jMin,jMax
          DO i=iMin,iMax
            skip_loc = .FALSE.
            k_offset = 0

            IF (k.LT.kSurfC(i,j,bi,bj)) THEN
C     Above the upper ocean level (probably in an ice shelf). Do nothing.
C     Leave stretching and Nsq as the zeros they were initialised to.
            skip_loc = .TRUE.

            ELSEIF (k.EQ.kSurfC(i,j,bi,bj)) THEN
C     Topmost ocean level. Need an offset to calculate the terms.
              k_offset = 1

              CALL FIND_RHO_2D(
     I              i, i, j, j, k,
     I              theta(i,j,k+2,bi,bj),
     I              salt(i,j,k+2,bi,bj),
     O              rho_single_cell,
     I              k+2, bi, bj, myThid )

              rhoKm1(i,j) = rholoc(i,j)
              rholoc(i,j) = rhoKp1(i,j)
              rhoKp1(i,j) = rho_single_cell

            ELSEIF (k.GT.kSurfC(i,j,bi,bj) .AND.
     &                 k.LT.kLowC(i,j,bi,bj)) THEN
C     In the ocean interior. Standard calculation. No offsets required.

            ELSEIF (k.EQ.kLowC(i,j,bi,bj)) THEN
C     Lowest ocean cell. Need an offset
              k_offset = -1

              CALL FIND_RHO_2D(
     I              i, i, j, j, k,
     I              theta(i,j,k-2,bi,bj),
     I              salt(i,j,k-2,bi,bj),
     O              rho_single_cell,
     I              k-2, bi, bj, myThid )

              rhoKp1(i,j) = rholoc(i,j)
              rholoc(i,j) = rhoKm1(i,j)
              rhoKm1(i,j) = rho_single_cell

            ELSEIF (k.GT.kLowC(i,j,bi,bj)) THEN
C     Below the lowest ocean level. Do nothing.
C     Leave stretching and Nsq as the zeros that they were initialised to.
            skip_loc = .TRUE.

            ELSE
C     Catch unexpected behaviour here.
              STOP
            ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C         Do QG Leith calculations
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            IF (skip_loc) THEN
C     Skip this location - it isn't water

            ELSE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              BUOYANCY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              buoy(i,j)=-Gravity*recip_rhoConst*rholoc(i,j)
              buoy_m1(i,j)=-Gravity*recip_rhoConst*rhoKm1(i,j)
              buoy_p1(i,j)=-Gravity*recip_rhoConst*rhoKp1(i,j)

C Interpolate buoyancy to upper and lower cell faces.
              buoy_1(i,j)= halfRL * (buoy(i,j) + buoy_m1(i,j))
              buoy_2(i,j)= halfRL * (buoy(i,j) + buoy_p1(i,j))


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              N^2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C a small number QGL_epsil (default value of 1. _d -9) is added
C to N^2 to prevent divsion by zero

              sigmaR= maskC(i,j,k,bi,bj)
     &                *recip_drC(k+k_offset)*rkSign
     &                *(rholoc(i,j)-rhoKm1(i,j))
              sigmaR = MIN( zeroRL, sigmaR )
              N2_1(i,j) = -gravity*recip_rhoConst*sigmaR + QGL_epsil

              sigmaR= maskC(i,j,k,bi,bj)
     &                *recip_drC(k+1+k_offset)*rkSign
     &                *(rhoKp1(i,j)-rholoc(i,j))
              sigmaR = MIN( zeroRL, sigmaR )
              N2_2(i,j) = -gravity*recip_rhoConst*sigmaR + QGL_epsil

              sigmaR= maskC(i,j,k,bi,bj)
     &                *rkSign
     &                *(rhoKp1(i,j)-rhoKm1(i,j))
     &                /(drC(k+k_offset) + drC(k+1+k_offset))
              sigmaR = MIN( zeroRL, sigmaR )
              N2_3(i,j) = -gravity*recip_rhoConst*sigmaR + QGL_epsil


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              (f/N^2) * b
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              kernel_1(i,j)=(fCori(i,j,bi,bj)/N2_1(i,j))*buoy_1(i,j)
              kernel_2(i,j)=(fCori(i,j,bi,bj)/N2_2(i,j))*buoy_2(i,j)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              d/dz of it
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              stretching(i,j)= maskC(i,j,k,bi,bj)
     &                *recip_drF(k)*rkSign
     &                *(kernel_2(i,j)-kernel_1(i,j))
              Nsq(i,j) = N2_3(i,j)

            ENDIF
          ENDDO
        ENDDO

C C Take a weighted nearest neighbors average to smooth it out a bit
C         DO j=2-Oly,sNy+Oly-1
C           DO i=2-Olx,sNx+Olx-1
C             N2_1(i,j) = (4. _d 0 *N2_1(i,j) + N2_1(i+1,j) +
C      &          N2_1(i-1,j) + N2_1(i,j-1) + N2_1(i,j+1))/8. _d 0

C             N2_2(i,j) = (4. _d 0 *N2_2(i,j) + N2_2(i+1,j) +
C      &          N2_2(i-1,j) + N2_2(i,j-1) + N2_2(i,j+1))/8. _d 0

C             N2_3(i,j) = (4. _d 0 *N2_3(i,j) + N2_3(i+1,j) +
C      &          N2_3(i-1,j) + N2_3(i,j-1) + N2_3(i,j+1))/8. _d 0
C           ENDDO
C         ENDDO

      RETURN
      END
