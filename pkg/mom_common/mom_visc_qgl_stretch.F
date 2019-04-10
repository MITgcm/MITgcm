#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_VISC_QGL_STRETCH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_VISC_QGL_STRETCH(
     I                                 bi, bj, k,
     O                                 stretching, Nsquare,
     I                                 myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE MOM_VISC_QGL_STRETCH
C     | Calculates the stratification and vortex stretching terms
C     | for the Quasi-Geostrophic implementation of Leith
C     | dynamic viscosity.
C     | At the upper and lower boundaries, the stratification is
C     | assumed constant. This means that 'stretching' is
C     | dictated by the buoyancy difference between the
C     | boundary cell and its vertical neighbour.
C     *==========================================================*

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
c#include "MOM_VISC.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  myTime               :: current time of simulation ( s )
C  myIter               :: current iteration number of simulation
C  myThid               :: my Thread Id number
      INTEGER bi,bj, k
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  stretching           :: vortex stretching contribution, calculated at
C                          cell centre, except near top and bottom, when
C                          it is calculated at cell face, but assumed to
C                          reside at cell centre.
C  Nsquare              :: buoyancy frequency, averaged to cell centre,
C                          except near top and bottom, when constant
C                          stratification is assumed, and it is copied
C                          to cell centre.
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsquare(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_LEITH_QG

C !LOCAL VARIABLES: ====================================================
      INTEGER i, j
      INTEGER iMin, iMax
      INTEGER jMin, jMax

      _RL QGL_epsil

      _RL kernel_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL kernel_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL buoy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_m1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_p1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoy_2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL Nsquarep1

CEOP

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        kernel_1(i,j)   = 0. _d 0
        kernel_2(i,j)   = 0. _d 0
        buoy(i,j)       = 0. _d 0
        buoy_m1(i,j)    = 0. _d 0
        buoy_p1(i,j)    = 0. _d 0
        buoy_1(i,j)     = 0. _d 0
        buoy_2(i,j)     = 0. _d 0
        stretching(i,j) = 0. _d 0
        Nsquare(i,j)    = 0. _d 0
       ENDDO
      ENDDO

      QGL_epsil = 1. _d -12
      Nsquarep1 = 0. _d 0

C use sigmaRfield, rhoInSitu from common block DYNVARS.h
C sigmaRfield is correctly masked, so no need to mask Nsquare

      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

      DO j=jMin,jMax
        DO i=iMin,iMax
          IF (k.GT.kSurfC(i,j,bi,bj) .AND.
     &                 k.LT.kLowC(i,j,bi,bj)) THEN
C           In the ocean interior. Standard calculation.

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              buoyancy
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            buoy(i,j)    = gravity*gravitySign*recip_rhoConst*
     &                       rhoInSitu(i,j,k,bi,bj)
            buoy_m1(i,j) = gravity*gravitySign*recip_rhoConst*
     &                       rhoInSitu(i,j,k-1,bi,bj)
            buoy_p1(i,j) = gravity*gravitySign*recip_rhoConst*
     &                       rhoInSitu(i,j,k+1,bi,bj)

C           Interpolate buoyancy to upper and lower cell faces.
C           (same location as Nsquare is calculated)
            buoy_1(i,j)= halfRL * (buoy(i,j) + buoy_m1(i,j))
            buoy_2(i,j)= halfRL * (buoy(i,j) + buoy_p1(i,j))

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              (f/N^2) * b
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            Nsquare(i,j) = gravity*gravitySign*recip_rhoConst
     &                  * sigmaRfield(i,j,k,bi,bj)
            Nsquarep1    = gravity*gravitySign*recip_rhoConst
     &                  * sigmaRfield(i,j,k+1,bi,bj)

            kernel_1(i,j) = (fCori(i,j,bi,bj)/
     &                     MAX(Nsquare(i,j),QGL_epsil))*
     &                        buoy_1(i,j)
            kernel_2(i,j)=(fCori(i,j,bi,bj)/
     &                     MAX(Nsquarep1,QGL_epsil))*
     &                        buoy_2(i,j)

C           Average Nsquare to cell centre for use in
C           MOM_VISC_QGL_LIMIT
            Nsquare(i,j) = halfRL * ( Nsquare(i,j) + Nsquarep1 )

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C              d/dz of it
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            stretching(i,j)= maskC(i,j,k,bi,bj)
     &                *recip_drF(k)*rkSign
     &                *(kernel_2(i,j)-kernel_1(i,j))

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ELSEIF (k.EQ.kSurfC(i,j,bi,bj) .AND.
     &                 k.EQ.kLowC(i,j,bi,bj)) THEN
C           Ocean only has one level. There is no possibility
C           for vertical stratification. Fail gracefully.
            stretching(i,j) = 0. _d 0

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ELSEIF (k.EQ.kSurfC(i,j,bi,bj) .AND.
     &                 k.LT.kLowC(i,j,bi,bj)) THEN
C           Ocean has at least two levels - currently in the uppermost one.
C           Use stratification from k+1, and assume it is constant.

C           buoyancy
            buoy(i,j)    = gravity*gravitySign*recip_rhoConst*
     &                       rhoInSitu(i,j,k,bi,bj)
            buoy_p1(i,j) = gravity*gravitySign*recip_rhoConst*
     &                       rhoInSitu(i,j,k+1,bi,bj)

C           Assuming constant stratification, so no need to
C           interpolate these.

C           (f/N^2) * b
            Nsquarep1    = gravity*gravitySign*recip_rhoConst
     &                  * sigmaRfield(i,j,k+1,bi,bj)

            kernel_1(i,j) = (fCori(i,j,bi,bj)/
     &                     MAX(Nsquarep1,QGL_epsil))*
     &                        buoy(i,j)
            kernel_2(i,j)=(fCori(i,j,bi,bj)/
     &                     MAX(Nsquarep1,QGL_epsil))*
     &                        buoy_p1(i,j)

C           Average Nsquare to cell centre for use in
C           MOM_VISC_QGL_LIMIT
C           (have assumed constant stratification, so
C           just assign it)
            Nsquare(i,j) = Nsquarep1

C           d/dz of it
            stretching(i,j)= maskC(i,j,k,bi,bj)
     &                *recip_drC(k+1)*rkSign
     &                *(kernel_2(i,j)-kernel_1(i,j))

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ELSEIF (k.GT.kSurfC(i,j,bi,bj) .AND.
     &                 k.EQ.kLowC(i,j,bi,bj)) THEN
C         Ocean has at least two levels - currently in the lowest one.
C         Use stratification from this level, and assume it is constant.

C           buoyancy
            buoy(i,j)    = gravity*gravitySign*recip_rhoConst*
     &                       rhoInSitu(i,j,k,bi,bj)
            buoy_m1(i,j) = gravity*gravitySign*recip_rhoConst*
     &                       rhoInSitu(i,j,k-1,bi,bj)

C           Assuming constant stratification, so no need to
C           interpolate these.

C           (f/N^2) * b
            Nsquare(i,j) = gravity*gravitySign*recip_rhoConst
     &                  * sigmaRfield(i,j,k,bi,bj)

            kernel_1(i,j) = (fCori(i,j,bi,bj)/
     &                     MAX(Nsquare(i,j),QGL_epsil))*
     &                        buoy_m1(i,j)
            kernel_2(i,j)=(fCori(i,j,bi,bj)/
     &                     MAX(Nsquare(i,j),QGL_epsil))*
     &                        buoy(i,j)

C           Average Nsquare to cell centre for use in
C           MOM_VISC_QGL_LIMIT
C           (have assumed constant stratification, so
C           no need to do anything)

C           d/dz of it
            stretching(i,j)= maskC(i,j,k,bi,bj)
     &                *recip_drC(k)*rkSign
     &                *(kernel_2(i,j)-kernel_1(i,j))

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ELSE
C           Not in the ocean (probably in an ice shelf)
C           Do nothing - stretching should be zero
            stretching(i,j) = 0. _d 0

          ENDIF

        ENDDO
      ENDDO

#endif /* ALLOW_LEITH_QG */

      RETURN
      END
