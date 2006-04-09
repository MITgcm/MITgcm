C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_step_fwd.F,v 1.16 2006/04/09 17:35:30 heimbach Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_STEP_FWD
C     !INTERFACE:
      SUBROUTINE THSICE_STEP_FWD(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             prcAtm,
     U             evpAtm, flxSW,
     I             myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_STEP_FWD
C     | o Step Forward Therm-SeaIce model.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#include "THSICE_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi,bj   :: tile indices
C   iMin,iMax :: computation domain: 1rst index range
C   jMin,jMax :: computation domain: 2nd  index range
C- input:
C     prcAtm  :: total precip from the atmosphere [kg/m2/s]
C     evpAtm  :: (Inp) evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
C     flxSW   :: (Inp) short-wave heat flux (+=down): downward comp. only
C                      (part.1), becomes net SW flux into ocean (part.2).
C- output
C     evpAtm  :: (Out) net fresh-water flux (E-P) from the atmosphere [m/s] (+=up)
C     flxSW   :: (Out) net surf. heat flux from the atmosphere [W/m2], (+=down)
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: thread number for this instance of the routine.
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL evpAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxSW (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     snowPr    :: snow precipitation [kg/m2/s]
C     agingTime :: aging time scale (s)
C     ageFac    :: snow aging factor [1]
C     albedo    :: surface albedo [0-1]
C     flxAtm    :: net heat flux from the atmosphere (+=down) [W/m2]
C     frwAtm    :: net fresh-water flux (E-P) to the atmosphere  [kg/m2/s]
C     Fbot      :: the oceanic heat flux already incorporated (ice_therm)
C     flx2oc    :: net heat flux from the ice to the ocean (+=down) [W/m2]
C     frw2oc    :: fresh-water flux from the ice to the ocean
C     fsalt     :: mass salt flux to the ocean
C     frzmltMxL :: ocean mixed-layer freezing/melting potential [W/m2]
C     TFrzOce   :: sea-water freezing temperature [oC] (function of S)
C     isIceFree :: true for ice-free grid-cell that remains ice-free
      INTEGER i,j
      _RL snowPr
      _RL agingTime, ageFac
      _RL albedo
      _RL frwAtm
      _RL flx2oc
      _RL frw2oc
      _RL fsalt
      _RL TFrzOce, cphm, frzmltMxL
      _RL Fbot, esurp
      _RL opFrac, icFrac
      _RL oceV2s, oceTs
      _RL Tsf, Tice(nlyr), qicen(nlyr)
      _RL tmpflx(0:2), tmpdTs
      _RL hSnow
c
      _RL flxAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL compact(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c
      LOGICAL isIceFree(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFac
#endif

      LOGICAL dBug

 1010 FORMAT(A,1P4E14.6)
      dBug = .FALSE.
C-    Initialise flxAtm
       DO j = 1-Oly, sNy+Oly
        DO i = 1-Olx, sNx+Olx
          flxAtm(i,j) = 0.
          isIceFree(i,j) = .FALSE.
#ifdef ALLOW_AUTODIFF_TAMC
          compact(i,j) = 0.
          hIce(i,j) = 0.
#endif
        ENDDO
       ENDDO

      IF ( fluidIsWater ) THEN
       DO j = jMin, jMax
        DO i = iMin, iMax
c        dBug = ( bi.EQ.3 .AND. i.EQ.15 .AND. j.EQ.11 )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.1 : ice-covered fraction ;
C     Solve for surface and ice temperature (implicitly) ; compute surf. fluxes
C-------
         IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
          icFrac  = iceMask(i,j,bi,bj)
          TFrzOce = -mu_Tf*sOceMxL(i,j,bi,bj)
          hIce(i,j)    = iceHeight(i,j,bi,bj)
          hSnow   = snowHeight(i,j,bi,bj)
          Tsf     = Tsrf(i,j,bi,bj)
          qicen(1)= Qice1(i,j,bi,bj)
          qicen(2)= Qice2(i,j,bi,bj)
          IF ( dBug ) THEN
           WRITE(6,'(A,2I4,2I2)') 'ThSI_FWD: i,j=',i,j,bi,bj
           WRITE(6,1010) 'ThSI_FWD:-0- iceMask, hIc, hSn, Tsf  =',
     &                                 icFrac, hIce,hSnow,Tsf
          ENDIF

          CALL THSICE_ALBEDO(
     I               hIce(i,j), hSnow, Tsf, snowAge(i,j,bi,bj),
     O               albedo,
     I               myThid )
          flxSW(i,j) = flxSW(i,j)*(1. _d 0 - albedo)
          siceAlb(i,j,bi,bj) = albedo

          CALL THSICE_SOLVE4TEMP(
     I          useBulkForce, tmpflx, TFrzOce, hIce(i,j), hSnow,
     U          flxSW(i,j), Tsf, qicen,
     O          Tice, sHeating(i,j,bi,bj), flxCndBt(i,j,bi,bj),
     O          tmpdTs, flxAtm(i,j), evpAtm(i,j),
     I          i,j, bi,bj, myThid)

#ifdef SHORTWAVE_HEATING
C--    Update Fluxes :
          opFrac= 1. _d 0-icFrac
          Qsw(i,j,bi,bj)=-icFrac*flxSW(i,j) +opFrac*Qsw(i,j,bi,bj)
#endif
C--    Update Sea-Ice state :
          Tsrf(i,j,bi,bj) =Tsf
          Tice1(i,j,bi,bj)=Tice(1)
          Tice2(i,j,bi,bj)=Tice(2)
          Qice1(i,j,bi,bj)=qicen(1)
          Qice2(i,j,bi,bj)=qicen(2)
          IF ( dBug ) THEN
           WRITE(6,1010) 'ThSI_FWD: Tsf, Tice(1,2), TFrzOce =',
     &                              Tsf, Tice, TFrzOce
           WRITE(6,1010) 'ThSI_FWD: sHeat,fxCndBt, fxAtm,evAtm=',
     &                  sHeating(i,j,bi,bj), flxCndBt(i,j,bi,bj),
     &                  flxAtm(i,j), evpAtm(i,j)
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDIF
      dBug = .FALSE.

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        tmpFac = 1. _d 0
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   snowPrc,   iceMask,tmpFac,1,'SIsnwPrc',
     I                   0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   siceAlb,   iceMask,tmpFac,1,'SIalbedo',
     I                   0,1,1,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      DO j = jMin, jMax
       DO i = iMin, iMax
          siceAlb(i,j,bi,bj) = iceMask(i,j,bi,bj)*siceAlb(i,j,bi,bj)
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.2 : ice-covered fraction ;
C     change in ice/snow thickness and ice-fraction
C     note: can only reduce the ice-fraction but not increase it.
C-------
      agingTime = 50. _d 0 * 86400. _d 0
      ageFac = 1. _d 0 - thSIce_deltaT/agingTime
      DO j = jMin, jMax
       DO i = iMin, iMax
c       dBug = ( bi.EQ.3 .AND. i.EQ.15 .AND. j.EQ.11 )

        TFrzOce = -mu_Tf*sOceMxL(i,j,bi,bj)
        oceTs   = tOceMxL(i,j,bi,bj)
        cphm    = cpwater*rhosw*hOceMxL(i,j,bi,bj)
        frzmltMxL = (TFrzOce-oceTs)*cphm/ocean_deltaT

        Fbot   = 0. _d 0
        saltFlux(i,j,bi,bj) = 0. _d 0
        compact(i,j)= iceMask(i,j,bi,bj)
C-------
        IF (dBug .AND. (frzmltMxL.GT.0. .OR. compact(i,j).GT.0.) ) THEN
          WRITE(6,'(A,2I4,2I2)') 'ThSI_FWD: i,j=',i,j,bi,bj
          WRITE(6,1010) 'ThSI_FWD:-1- iceMask, hIc, hSn, Tsf  =',
     &                  compact(i,j), iceHeight(i,j,bi,bj),
     &                  snowHeight(i,j,bi,bj), Tsrf(i,j,bi,bj)
          WRITE(6,1010) 'ThSI_FWD: ocTs,TFrzOce,frzmltMxL,Qnet=',
     &                     oceTs, TFrzOce, frzmltMxL,Qnet(i,j,bi,bj)
        ENDIF
C-------
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN

          oceV2s  = v2ocMxL(i,j,bi,bj)
          snowPr  = snowPrc(i,j,bi,bj)
          hIce(i,j)    = iceHeight(i,j,bi,bj)
          hSnow   = snowHeight(i,j,bi,bj)
          Tsf     = Tsrf(i,j,bi,bj)
          qicen(1)= Qice1(i,j,bi,bj)
          qicen(2)= Qice2(i,j,bi,bj)
          flx2oc  = flxSW(i,j)

          CALL THSICE_CALC_THICKN(
     I          frzmltMxL, TFrzOce, oceTs, oceV2s, snowPr,
     I          sHeating(i,j,bi,bj), flxCndBt(i,j,bi,bj), evpAtm(i,j),
     U          compact(i,j), hIce(i,j), hSnow, Tsf, qicen, flx2oc,
     O          frw2oc, fsalt, Fbot,
     I          dBug, myThid)

C- note : snowPr was not supposed to be modified in THSICE_THERM ;
C         but to reproduce old results, is reset to zero if Tsf >= 0
          snowPrc(i,j,bi,bj) = snowPr

C--  Snow aging :
          snowAge(i,j,bi,bj) = thSIce_deltaT
     &                       + snowAge(i,j,bi,bj)*ageFac
          IF ( snowPr.GT.0. _d 0 )
     &      snowAge(i,j,bi,bj) = snowAge(i,j,bi,bj)
     &          * EXP( -(thSIce_deltaT*snowPr/rhos)/hNewSnowAge )
C--

C-- Diagnostic of Atmospheric Fluxes over sea-ice :
          frwAtm = evpAtm(i,j) - prcAtm(i,j)
C note: Any flux of mass (here fresh water) that enter or leave the system
C       with a non zero energy HAS TO be counted: add snow precip.
          flxAtm(i,j) = flxAtm(i,j) - Lfresh*snowPrc(i,j,bi,bj)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (dBug) WRITE(6,1010) 'ThSI_FWD: icFrac,flxAtm,evpAtm,flxSnw=',
     &  iceMask(i,j,bi,bj),flxAtm(i,j),evpAtm(i,j),-Lfresh*snowPr
      IF (dBug) WRITE(6,1010) 'ThSI_FWD: compact,flx2oc,fsalt,frw2oc=',
     &   compact(i,j),flx2oc,fsalt,frw2oc
#ifdef CHECK_ENERGY_CONSERV
          icFrac = iceMask(i,j,bi,bj)
          CALL THSICE_CHECK_CONSERV( dBug, i, j, bi, bj, 0,
     I            icFrac, compact(i,j), hIce(i,j), hSnow, qicen,
     I            flx2oc, frw2oc, fsalt, flxAtm(i,j), frwAtm,
     I            myTime, myIter, myThid )
#endif /* CHECK_ENERGY_CONSERV */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--    Update Sea-Ice state :
c         iceMask(i,j,bi,bj)=compact
          iceHeight(i,j,bi,bj) = hIce(i,j)
          snowHeight(i,j,bi,bj)= hSnow
          Tsrf(i,j,bi,bj) =Tsf
          Qice1(i,j,bi,bj)=qicen(1)
          Qice2(i,j,bi,bj)=qicen(2)

C--    Net fluxes :
          frw2oc = frw2oc + (prcAtm(i,j)-snowPrc(i,j,bi,bj))
C-     weighted average net fluxes:
          icFrac = iceMask(i,j,bi,bj)
          opFrac= 1. _d 0-icFrac
          flxAtm(i,j) = icFrac*flxAtm(i,j) - opFrac*Qnet(i,j,bi,bj)
          frwAtm =     icFrac*frwAtm + opFrac*rhofw*EmPmR(i,j,bi,bj)
          Qnet(i,j,bi,bj)=-icFrac*flx2oc +opFrac*Qnet(i,j,bi,bj)
          EmPmR(i,j,bi,bj)=-icFrac*frw2oc/rhofw+opFrac*EmPmR(i,j,bi,bj)
          saltFlux(i,j,bi,bj)=-icFrac*fsalt

          IF (dBug) WRITE(6,1010)
     &          'ThSI_FWD:-3- compact, hIc, hSn, Qnet =',
     &                        compact(i,j),hIce(i,j),hSnow,
     &                        Qnet(i,j,bi,bj)

        ELSEIF (hOceMxL(i,j,bi,bj).gt.0. _d 0) THEN
          flxAtm(i,j) =  -Qnet(i,j,bi,bj)
          frwAtm = rhofw*EmPmR(i,j,bi,bj)
        ELSE
          flxAtm(i,j) = 0. _d 0
          frwAtm      = 0. _d 0
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.3 : freezing of sea-water
C     over ice-free fraction and what is left from ice-covered fraction
C-------
c       compact(i,j) = iceMask(i,j,bi,bj)
        hIce(i,j)   = iceHeight(i,j,bi,bj)
        hSnow  = snowHeight(i,j,bi,bj)

        esurp  = frzmltMxL - Fbot*iceMask(i,j,bi,bj)
        IF (esurp.GT.0. _d 0) THEN
          icFrac = compact(i,j)
          qicen(1)= Qice1(i,j,bi,bj)
          qicen(2)= Qice2(i,j,bi,bj)
          CALL THSICE_EXTEND(
     I               esurp, TFrzOce,
     U               oceTs, compact(i,j), hIce(i,j), hSnow, qicen,
     O               flx2oc, frw2oc, fsalt,
     I               dBug, myThid )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (dBug) WRITE(6,1010) 'ThSI_FWD: compact,flx2oc,fsalt,frw2oc='
     &                 ,compact(i,j),flx2oc,fsalt,frw2oc
#ifdef CHECK_ENERGY_CONSERV
          tmpflx(1) = 0.
          tmpflx(2) = 0.
          CALL THSICE_CHECK_CONSERV( dBug, i, j, bi, bj, 1,
     I            icFrac, compact(i,j), hIce(i,j), hSnow, qicen,
     I            flx2oc, frw2oc, fsalt, tmpflx(1), tmpflx(2),
     I            myTime, myIter, myThid )
#endif /* CHECK_ENERGY_CONSERV */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--    Update Sea-Ice state :
          IF ( compact(i,j).GT.0. _d 0 .AND. icFrac.EQ.0. _d 0) THEN
             Tsrf(i,j,bi,bj)  = TFrzOce
             Tice1(i,j,bi,bj) = TFrzOce
             Tice2(i,j,bi,bj) = TFrzOce
             Qice1(i,j,bi,bj) = qicen(1)
             Qice2(i,j,bi,bj) = qicen(2)
          ENDIF
          iceHeight(i,j,bi,bj) = hIce(i,j)
          snowHeight(i,j,bi,bj)= hSnow
C--    Net fluxes :
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) - flx2oc
          EmPmR(i,j,bi,bj)= EmPmR(i,j,bi,bj)- frw2oc/rhofw
          saltFlux(i,j,bi,bj)=saltFlux(i,j,bi,bj) - fsalt

          IF (dBug) WRITE(6,1010)
     &         'ThSI_FWD:-4- compact, hIc, hSn, Qnet =',
     &         compact(i,j),hIce(i,j),hSnow,
     &         Qnet(i,j,bi,bj)
C--   - if esurp > 0 : end
        ENDIF

        IF ( hOceMxL(i,j,bi,bj).GT.0. _d 0 )
     &    isIceFree(i,j) = iceMask(i,j,bi,bj).LE.0. _d 0
     &                       .AND.   compact(i,j) .LE.0. _d 0
        IF ( compact(i,j) .GT. 0. _d 0 ) THEN
          iceMask(i,j,bi,bj)=compact(i,j)
          IF ( hSnow .EQ. 0. _d 0 ) snowAge(i,j,bi,bj) = 0. _d 0
        ELSE
          iceMask(i,j,bi,bj)  = 0. _d 0
          iceHeight(i,j,bi,bj)= 0. _d 0
          snowHeight(i,j,bi,bj)=0. _d 0
          snowAge(i,j,bi,bj)  = 0. _d 0
          Tsrf(i,j,bi,bj)     = oceTs
          Tice1(i,j,bi,bj)    = 0. _d 0
          Tice2(i,j,bi,bj)    = 0. _d 0
          Qice1(i,j,bi,bj)    = 0. _d 0
          Qice2(i,j,bi,bj)    = 0. _d 0
        ENDIF

C--     Return atmospheric fluxes in evpAtm & flxSW (same sign and units):
        evpAtm(i,j) = frwAtm
        flxSW (i,j) = flxAtm(i,j)

#ifdef ATMOSPHERIC_LOADING
C--     Compute Sea-Ice Loading (= mass of sea-ice + snow / area unit)
        sIceLoad(i,j,bi,bj) = ( snowHeight(i,j,bi,bj)*rhos
     &                         + iceHeight(i,j,bi,bj)*rhoi
     &                        )*iceMask(i,j,bi,bj)
#endif

       ENDDO
      ENDDO

#ifdef ALLOW_BULK_FORCE
      IF ( useBulkForce ) THEN
        CALL BULKF_FLUX_ADJUST(
     I                          bi, bj, iMin, iMax, jMin, jMax,
     I                          isIceFree, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_BULK_FORCE */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_THSICE */

      RETURN
      END
