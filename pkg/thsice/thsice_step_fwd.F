C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_step_fwd.F,v 1.2 2003/12/31 17:44:32 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
 
C     !ROUTINE: THSICE_STEP_FWD
C     !INTERFACE:
      SUBROUTINE THSICE_STEP_FWD( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE  THSICE_STEP_FWD             
C     | o Step Forward Therm-SeaIce model.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#endif
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE.h"
#include "THSICE_DIAGS.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     snowAge       :: snow age (s)
C     albedo        :: surface albedo [0-1]
C     fSWabs        :: net Short-Wave (+ = down) at surface (W m-2)
C     Fbot          :: the oceanic heat flux already incorporated (ice_therm)
C     flxAtm        :: net heat flux from the atmosphere ( >0 downward)
C     evpAtm        :: evaporation to the atmosphere
C     frwAtm        :: net fresh-water flux (E-P-R) to the atmosphere (m/s)
C     qleft         :: net heat flux from the ice to the ocean
C     ffresh        :: fresh-water flux from the ice to the ocean
C     fsalt         :: mass salt flux to the ocean
      INTEGER i,j
      _RL snowAge
      _RL albedo
      _RL fSWabs
      _RL qleft, qNewIce 
      _RL fsalt
      _RL ffresh
      _RL Tf, cphm, frzmlt
      _RL Fbot, esurp
      _RL flxAtm, evpAtm, frwAtm 
      _RL openFrac, iceFrac, qicAv
      _RL oceHs, oceV2s, oceSs, oceTs
      _RL compact, hIce, hSnow, Tsf, Tice(nlyr), qicen(nlyr)

      LOGICAL dBug

      dBug = .FALSE.
 1010 FORMAT(A,1P4E11.3)

      DO j = jMin, jMax
       DO i = iMin, iMax
c       dBug = ( bi.EQ.3 .AND. i.EQ.13 .AND. j.EQ.13 )

        Tf     = -mu_Tf*salt(i,j,1,bi,bj)
        cphm   = cpwater*rhosw*drF(1)*hFacC(i,j,1,bi,bj)
        oceTs  = theta(i,j,1,bi,bj)
        frzmlt = (Tf-oceTs)*cphm/thSIce_deltaT
        compact= iceMask(i,j,bi,bj)
        hIce   = iceHeight(i,j,bi,bj)
        hSnow  = snowHeight(i,j,bi,bj)
        Fbot   = 0. _d 0
        snow(i,j,bi,bj)     = 0. _d 0
        saltFlux(i,j,bi,bj) = 0. _d 0

        IF (dBug .AND. (frzmlt.GT.0. .OR. compact.GT.0.) ) THEN
          WRITE(6,1010) 'ThSI_FWD:-0- iceMask,hIc,hSn,Qnet=',
     &                  compact, hIce, hSnow, Qnet(i,j,bi,bj)
          WRITE(6,1010) 'ThSI_FWD: ocTs,Tf,frzmlt=',
     &                            oceTs,Tf,frzmlt
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.1 : ice-covered fraction ; 
C     can only reduce the ice-fraction but not increase it.
C-------
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN 
          oceHs   = hfacC(i,j,1,bi,bj)*drF(1)
          oceSs   = salt (i,j,1,bi,bj)
          oceV2s  = ( uvel(i,j,1,bi,bj)*uvel(i,j,1,bi,bj)
     &              + uvel(i+1,j,1,bi,bj)*uvel(i+1,j,1,bi,bj)
     &              + vvel(i,j+1,1,bi,bj)*vvel(i,j+1,1,bi,bj)
     &              + vvel(i,j,1,bi,bj)*vvel(i,j,1,bi,bj) )*0.5 _d 0
          snowAge = sage(i,j,bi,bj)
c         snowAge = thSIce_deltaT
          Tsf     = Tsrf(i,j,bi,bj)
          Tice(1) = Tice1(i,j,bi,bj)
          Tice(2) = Tice2(i,j,bi,bj)
          qicen(1)= Qice1(i,j,bi,bj)
          qicen(2)= Qice2(i,j,bi,bj)
          CALL THSICE_ALBEDO(hIce,hSnow,Tsf,snowAge,albedo)
          fSWabs = solar(i,j,bi,bj)*(1. _d 0 - albedo)
          CALL THSICE_THERM(
     I          fSWabs, oceHs, oceV2s, oceSs, oceTs,
     U          compact, hIce, hSnow, Tsf, Tice, qicen,
     O          qleft, ffresh, fsalt, Fbot,
     O          flxAtm, evpAtm,
     I          i,j, bi,bj, myThid)

C-- Diagnostic of Atmospheric Fluxes over sea-ice :
          frwAtm = evpAtm - snow(i,j,bi,bj)*rhos/rhofw
C note: Any flux of mass (here fresh water) that enter or leave the system
C       with a non zero energy HAS TO be counted: add snow precip.
          flxAtm = flxAtm - Lfresh*snow(i,j,bi,bj)*rhos

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (dBug) WRITE(6,1010) 'ThSI_FWD: iceFrac,flxAtm,evpAtm,flxSnw=',
     &  iceMask(i,j,bi,bj),flxAtm,evpAtm,-Lfresh*snow(i,j,bi,bj)*rhos
      IF (dBug) WRITE(6,1010) 'ThSI_FWD: compact,qleft,fsalt,ffresh=',
     &   compact,qleft,fsalt,ffresh
#ifdef CHECK_ENERGY_CONSERV
          iceFrac = iceMask(i,j,bi,bj)
          CALL THSICE_CHECK_CONSERV( dBug, i, j, bi, bj, 0,
     I            iceFrac, compact, hIce, hSnow, qicen,
     I            qleft, ffresh, fsalt, flxAtm, frwAtm,
     I            myTime, myIter, myThid )
#endif /* CHECK_ENERGY_CONSERV */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--    Update Sea-Ice state :
c         iceMask(i,j,bi,bj)=compact
          iceheight(i,j,bi,bj) = hIce
          snowheight(i,j,bi,bj)= hSnow
          Tsrf(i,j,bi,bj) =Tsf
          Tice1(i,j,bi,bj)=Tice(1)
          Tice2(i,j,bi,bj)=Tice(2)
          Qice1(i,j,bi,bj)=qicen(1)
          Qice2(i,j,bi,bj)=qicen(2)

C--    Net fluxes :
          ffresh = ffresh/rhofw
          ffresh = -ffresh-rain(i,j,bi,bj)-runoff(i,j,bi,bj)
          frwAtm =  frwAtm-rain(i,j,bi,bj)-runoff(i,j,bi,bj)
          iceFrac = iceMask(i,j,bi,bj)
          openFrac= 1. _d 0-iceFrac
#ifdef ALLOW_TIMEAVE
          ICE_Qnet_AVE(i,j,bi,bj) = ICE_Qnet_AVE(i,j,bi,bj)
     &          + ( -iceFrac*flxAtm + openFrac*Qnet(i,j,bi,bj)
     &            )*thSIce_deltaT
          ICE_FWfx_AVE(i,j,bi,bj) = ICE_FWfx_AVE(i,j,bi,bj)
     &          + ( iceFrac*frwAtm + openFrac*EmPmR(i,j,bi,bj)
     &            )*thSIce_deltaT
          ICE_albedo_AVE(i,j,bi,bj) = ICE_albedo_AVE(i,j,bi,bj)
     &          + iceFrac*albedo*thSIce_deltaT
#endif /*ALLOW_TIMEAVE*/
          Qnet(i,j,bi,bj)=-iceFrac*qleft + openFrac*Qnet(i,j,bi,bj)
          EmPmR(i,j,bi,bj)=iceFrac*ffresh+openFrac*EmPmR(i,j,bi,bj)
          saltFlux(i,j,bi,bj)=-iceFrac*fsalt

          IF (dBug) WRITE(6,1010)'ThSI_FWD:-1- compact,hIc,hSn,Qnet=',
     &                      compact,hIce,hSnow,Qnet(i,j,bi,bj)

        ELSEIF (hFacC(i,j,1,bi,bj).gt.0. _d 0) THEN

#ifdef ALLOW_TIMEAVE
         ICE_Qnet_AVE(i,j,bi,bj) = ICE_Qnet_AVE(i,j,bi,bj)
     &                   +Qnet(i,j,bi,bj)*thSIce_deltaT
         ICE_FWfx_AVE(i,j,bi,bj) = ICE_FWfx_AVE(i,j,bi,bj)
     &                   +EmPmR(i,j,bi,bj)*thSIce_deltaT
#endif /*ALLOW_TIMEAVE*/

        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.2 : freezing of sea-water
C     over ice-free fraction and what is left from ice-covered fraction
C-------
        esurp = frzmlt - Fbot*iceMask(i,j,bi,bj)
        IF (esurp.GT.0. _d 0) THEN
          iceFrac = compact
          IF ( compact.GT.0. _d 0 ) THEN
            qicen(1)= Qice1(i,j,bi,bj)
            qicen(2)= Qice2(i,j,bi,bj)
          ELSE
            qicen(1)= -cpwater*Tmlt1
     &               + cpice *(Tmlt1-Tf) + Lfresh*(1. _d 0-Tmlt1/Tf)
            qicen(2)= -cpice *Tf + Lfresh 
          ENDIF
          qicAv = rhoi*(qicen(1)+qicen(2))*0.5 _d 0
          CALL THSICE_START( myThid,
     I             esurp, qicAv, Tf,
     O             qNewIce, ffresh, fsalt,
     U             oceTs, compact, hIce, hSnow )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (dBug) WRITE(6,1010) 'ThSI_FWD: compact,qNewIce,fsalt,ffresh='
     &                 ,compact,qNewIce,fsalt,ffresh
#ifdef CHECK_ENERGY_CONSERV 
          flxAtm = 0. 
          frwAtm = 0. 
          CALL THSICE_CHECK_CONSERV( dBug, i, j, bi, bj, 1,
     I            iceFrac, compact, hIce, hSnow, qicen,
     I            qNewIce, ffresh, fsalt, flxAtm, frwAtm,
     I            myTime, myIter, myThid )
#endif /* CHECK_ENERGY_CONSERV */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--    Update Sea-Ice state :
          IF ( compact.GT.0. _d 0 .AND. iceFrac.EQ.0. _d 0) THEN
             Tsrf(i,j,bi,bj)  = Tf
             Tice1(i,j,bi,bj) = Tf
             Tice2(i,j,bi,bj) = Tf
             Qice1(i,j,bi,bj) = qicen(1)
             Qice2(i,j,bi,bj) = qicen(2)
          ENDIF
          iceheight(i,j,bi,bj) = hIce
          snowheight(i,j,bi,bj)= hSnow
C--    Net fluxes :
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) - qNewIce
          EmPmR(i,j,bi,bj)= EmPmR(i,j,bi,bj)- ffresh/rhofw 
          saltFlux(i,j,bi,bj)=saltFlux(i,j,bi,bj) - fsalt

          IF (dBug) WRITE(6,1010)'ThSI_FWD:-2- compact,hIc,hSn,Qnet=',
     &                        compact,hIce,hSnow,Qnet(i,j,bi,bj)
C--   - if esurp > 0 : end
        ENDIF

        IF ( compact .GT. 0. _d 0 ) THEN
          iceMask(i,j,bi,bj)=compact
          IF ( hSnow .EQ. 0. _d 0 ) sage(i,j,bi,bj) = 0. _d 0
        ELSE
          iceMask(i,j,bi,bj)  = 0. _d 0
          iceHeight(i,j,bi,bj)= 0. _d 0
          snowHeight(i,j,bi,bj)=0. _d 0
          sage(i,j,bi,bj)     = 0. _d 0
          Tsrf(i,j,bi,bj)     = oceTs
          Tice1(i,j,bi,bj)    = 0. _d 0
          Tice2(i,j,bi,bj)    = 0. _d 0
          Qice1(i,j,bi,bj)    = 0. _d 0
          Qice2(i,j,bi,bj)    = 0. _d 0
        ENDIF

#ifndef CHECK_ENERGY_CONSERV
#ifdef ALLOW_TIMEAVE
          ICE_qleft_AVE(i,j,bi,bj)=ICE_qleft_AVE(i,j,bi,bj)
     &         + ( Qnet(i,j,bi,bj)
     &            )*thSIce_deltaT
          ICE_fresh_AVE(i,j,bi,bj)=ICE_fresh_AVE(i,j,bi,bj)
     &         + ( EmPmR(i,j,bi,bj)
     &            )*thSIce_deltaT
          ICE_salFx_AVE(i,j,bi,bj)=ICE_salFx_AVE(i,j,bi,bj)
     &            +saltFlux(i,j,bi,bj)*thSIce_deltaT
#endif /* ALLOW_TIMEAVE */
#endif /* CHECK_ENERGY_CONSERV */

       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_THSICE */

      RETURN
      END
