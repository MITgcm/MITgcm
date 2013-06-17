C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_seaice.F,v 1.1 2013/06/17 13:45:14 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"
#ifdef ALLOW_THSICE
# include "THSICE_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CHEAPAML_SEAICE
C     !INTERFACE:
      SUBROUTINE CHEAPAML_SEAICE(
     I                    swDown, lwDown, uWind, vWind, LVapor,
     O                    fsha, flha, evp, xolw, ssqt, q100, cdq,
     O                    Tsurf, iceFrac, sw2oce,
     I                    bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CHEAPAML_SEAICE
C     | o Compute fluxes over seaice by calling seaice routine
C     |   to solve for surface temperature.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_THSICE
#include "THSICE_PARAMS.h"
#include "THSICE_SIZE.h"
#include "THSICE_VARS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif

      INTEGER siLo, siHi, sjLo, sjHi
      PARAMETER ( siLo = 1-OLx , siHi = sNx+OLx )
      PARAMETER ( sjLo = 1-OLy , sjHi = sNy+OLy )

C     !INPUT PARAMETERS:
C     == Routine Arguments ==
C     swDown   :: incoming short-wave radiation (+=dw) [W/m2]
C     lwDown   :: incoming  long-wave radiation (+=dw) [W/m2]
C     uRelWind :: relative wind speed, u-component [m/s]
C     vRelWind :: relative wind speed, v-component [m/s]
C     LVapor   :: latent heat of vaporisation
C     bi, bj   :: tile indices
C     myIter   :: current iteration number
C     myTime   :: current time in simulation
C     myThid   :: my Thread Id number
      _RL  swDown(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  lwDown(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uWind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vWind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL LVapor
      _RL myTime
      INTEGER bi, bj, myIter, myThid

C     !OUTPUT PARAMETERS:
C     fsha     :: sensible heat-flux over seaice (+=up) [W/m2]
C     flha     :: latent heat-flux over seaice   (+=up) [W/m2]
C     evp      :: evaporation over seaice     (+=up) [kg/m2/s]
C     xolw     :: upward long-wave over seaice   (+=up) [W/m2]
C     ssqt     ::
C     q100     ::
C     cdq      ::
C     Tsurf    :: updated seaice/snow surface temperature [deg.C]
C     iceFrac  :: ice fraction [0-1]
C     sw2oce   :: short-wave over seaice into the ocean (+=dw) [W/m2]
      _RL fsha(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flha(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL evp (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL xolw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ssqt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL q100(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cdq (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tsurf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceFrac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS sw2oce (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     uRelWind :: relative wind speed, u-component [m/s], (C-grid)
C     vRelWind :: relative wind speed, v-component [m/s], (C-grid)
C     windSq   :: relative wind speed squared (grid-cell center)
      INTEGER i, j
      INTEGER iceOrNot
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL LatentHeat
      _RL icFrac, opFrac
        _RL netSW (1:sNx,1:sNy)
        _RL sFlx  (1:sNx,1:sNy,0:2)
c       _RL tFrzOce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL dTsurf(1:sNx,1:sNy)

        _RL uRelWind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL vRelWind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL windSq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL cdu, dumArg(4)
        _RL fsha0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL evp_0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL xolw0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c       _RL ssqt0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c       _RL q10_0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c       _RL cdq_0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL dShdTs(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL dEvdTs(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL dLwdTs(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy
      LatentHeat = Lfresh + LVapor

c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)

        CALL THSICE_GET_OCEAN(
     I                         bi, bj, myTime, myIter, myThid )

        DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
            uRelWind(i,j) = uWind(i,j)
            vRelWind(i,j) = vWind(i,j)
          ENDDO
        ENDDO
#ifdef ALLOW_SEAICE
        IF ( useSEAICE ) THEN
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
            uRelWind(i,j) = uRelWind(i,j)-uIce(i,j,bi,bj)
            vRelWind(i,j) = vRelWind(i,j)-vIce(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_SEAICE */
        DO j = jMin,jMax
          DO i = iMin,iMax
            windSq(i,j) = ( uRelWind( i ,j)*uRelWind( i ,j)
     &                    + uRelWind(i+1,j)*uRelWind(i+1,j)
     &                    + vRelWind(i, j )*vRelWind(i, j )
     &                    + vRelWind(i,j+1)*vRelWind(i,j+1)
     &                    )*0.5 _d 0
          ENDDO
        ENDDO

C   1) compute albedo ; compute netSW
       CALL THSICE_ALBEDO(
     I          bi, bj, siLo, siHi, sjLo, sjHi,
     I          iMin,iMax, jMin,jMax,
     I          iceMask(siLo,sjLo,bi,bj), iceHeight(siLo,sjLo,bi,bj),
     I          snowHeight(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     I          snowAge(siLo,sjLo,bi,bj),
     O          siceAlb(siLo,sjLo,bi,bj), icAlbNIR(siLo,sjLo,bi,bj),
     I          myTime, myIter, myThid )

       DO j = jMin, jMax
        DO i = iMin, iMax
         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
C-      surface net SW flux:
          netSW(i,j) = swDown(i,j)
     &               *(1. _d 0 - siceAlb(i,j,bi,bj))
         ELSE
          netSW(i,j) = swDown(i,j)
         ENDIF
        ENDDO
       ENDDO


C   2) compute other flx over seaice, over melting surf
C   3) compute other flx over seaice & derivative vs Tsurf, using previous Tsurf
         DO j = jMin, jMax
          DO i = iMin, iMax

            IF ( snowHeight(i,j,bi,bj).GT.3. _d -1 ) THEN
             iceornot=2
            ELSE
             iceornot=1
            ENDIF
            Tsurf(i,j) = 0.
            CALL CHEAPAML_COARE3_FLUX(
     I                    i, j, bi, bj, iceOrNot,
     I                    Tsurf, windSq,
     O                    fsha0(i,j), flha(i,j), evp_0(i,j), xolw0(i,j),
     O                    ssqt(i,j), q100(i,j), cdq(i,j), cdu,
     O                    dumArg(1), dumArg(2), dumArg(3), dumArg(4),
     I                    myIter, myThid )
            sFlx(i,j,0) = lwDown(i,j)- xolw0(i,j)
     &                  - fsha0(i,j) - evp_0(i,j)*LatentHeat

            Tsurf(i,j) = Tsrf(i,j,bi,bj)
            CALL CHEAPAML_COARE3_FLUX(
     I                    i, j, bi, bj, iceOrNot,
     I                    Tsurf, windSq,
     O                    fsha(i,j), flha(i,j), evp(i,j), xolw(i,j),
     O                    ssqt(i,j), q100(i,j), cdq(i,j), cdu,
     O              dShdTs(i,j), dEvdTs(i,j), dLwdTs(i,j), dumArg(4),
     I                   myIter, myThid )
            sFlx(i,j,1) = lwDown(i,j)- xolw(i,j)
     &                  - fsha(i,j) - evp(i,j)*LatentHeat
            sFlx(i,j,2) = -dLwdTs(i,j)
     &                  - dShdTs(i,j) - dEvdTs(i,j)*LatentHeat
          ENDDO
         ENDDO

C   4) solve for surf & seaice temp
C--    needs to fill in snowPrc, ( & prcAtm ? )
C--    note: this S/R  assumes No overlap
         CALL THSICE_IMPL_TEMP(
     I                netSW, sFlx,
     O                dTsurf,
     I                bi, bj, myTime, myIter, myThid )

C   5) update surf fluxes
        DO j = jMin, jMax
         DO i = iMin, iMax
          iceFrac(i,j) = iceMask(i,j,bi,bj)
          sw2oce (i,j) = icFlxSW(i,j,bi,bj)
          IF ( dTsurf(i,j) .GT. 999. ) THEN
c          dTsurf(J)= tFreeze - Tsurf(J)
           Tsurf(i,j)= 0.
           fsha(i,j) = fsha0(i,j)
           flha(i,j) = evp_0(i,j)*LatentHeat
           evp(i,j)  = evp_0(i,j)
           xolw(i,j) = xolw0(i,j)
          ELSE
           Tsurf(i,j)= Tsurf(i,j)+ dTsurf(i,j)
           fsha(i,j) = fsha(i,j) + dTsurf(i,j)*dShdTs(i,j)
           evp(i,j)  = evp(i,j)  + dTsurf(i,j)*dEvdTs(i,j)
           flha(i,j) = evp(i,j)*LatentHeat
           xolw(i,j) = xolw(i,j) + dTsurf(i,j)*dLwdTs(i,j)
          ENDIF
         ENDDO
        ENDDO

        DO j = jMin, jMax
         DO i = iMin, iMax
c         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
           icFrac  = iceMask(i,j,bi,bj)
           opFrac = 1. _d 0 - icFrac
C--    Update Fluxes :
           icFlxAtm(i,j,bi,bj) = netSW(i,j)
     &                         + lwDown(i,j)- xolw(i,j)
     &                         - fsha(i,j) - evp(i,j)*LVapor
           icFrwAtm(i,j,bi,bj) = evp(i,j)
c         ENDIF
         ENDDO
        ENDDO

c      ENDDO
c     ENDDO

#endif /* ALLOW_THSICE */
      RETURN
      END
