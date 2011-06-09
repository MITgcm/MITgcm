C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_step_fwd.F,v 1.32 2011/06/09 18:50:16 jscott Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_ATM2D
# include "ctrparam.h"
#endif

CBOP
C     !ROUTINE: THSICE_STEP_FWD
C     !INTERFACE:
      SUBROUTINE THSICE_STEP_FWD(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             prcAtm,
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
#ifdef  ALLOW_ATM2D
# include "ATMSIZE.h"
# include "ATM2D_VARS.h"
#endif
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#include "THSICE_TAVE.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

      INTEGER siLo, siHi, sjLo, sjHi
      PARAMETER ( siLo = 1-OLx , siHi = sNx+OLx )
      PARAMETER ( sjLo = 1-OLy , sjHi = sNy+OLy )

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C- input:
C     bi,bj   :: tile indices
C   iMin,iMax :: computation domain: 1rst index range
C   jMin,jMax :: computation domain: 2nd  index range
C     prcAtm  :: total precip from the atmosphere [kg/m2/s]
C     myTime  :: current Time of simulation [s]
C     myIter  :: current Iteration number in simulation
C     myThid  :: my Thread Id number
C-- Use fluxes hold in commom blocks
C- input:
C     icFlxSW :: net short-wave heat flux (+=down) below sea-ice, into ocean
C     icFlxAtm  :: net Atmospheric surf. heat flux over sea-ice [W/m2], (+=down)
C     icFrwAtm  :: evaporation over sea-ice to the atmosphere [kg/m2/s] (+=up)
C- output
C     icFlxAtm  :: net Atmospheric surf. heat flux over ice+ocean [W/m2], (+=down)
C     icFrwAtm  :: net fresh-water flux (E-P) from the atmosphere [m/s] (+=up)
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     iceFrac   :: fraction of grid area covered in ice
C     flx2oc    :: net heat flux from the ice to the ocean (+=down) [W/m2]
C     frw2oc    :: fresh-water flux from the ice to the ocean
C     fsalt     :: mass salt flux to the ocean
C     frzmltMxL :: ocean mixed-layer freezing/melting potential [W/m2]
C     tFrzOce   :: sea-water freezing temperature [oC] (function of S)
C     isIceFree :: true for ice-free grid-cell that remains ice-free
C     ageFac    :: snow aging factor [1]
C     snowFac   :: snowing refreshing-age factor [units of 1/snowPr]
      LOGICAL isIceFree(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     iceFrac  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     flx2oc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     frw2oc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     fsalt    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tFrzOce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     frzmltMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ageFac
      _RL snowFac
      _RL cphm
      _RL opFrac, icFrac
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFac
#endif
      INTEGER i,j
      LOGICAL dBugFlag

C-    define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

 1010 FORMAT(A,1P4E14.6)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ticekey = (act1 + 1) + act2*max1
     &                     + act3*max1*max2
     &                     + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C-    Initialise
      dBugFlag = debugLevel.GE.debLevC
      DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
          isIceFree(i,j) = .FALSE.
#ifdef ALLOW_ATM2D
          sFluxFromIce(i,j) = 0. _d 0
#else
          saltFlux(i,j,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_AUTODIFF_TAMC
          iceFrac(i,j) = 0.
#endif
        ENDDO
      ENDDO

      ageFac = 1. _d 0 - thSIce_deltaT/snowAgTime
      snowFac = thSIce_deltaT/(rhos*hNewSnowAge)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceMask(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE iceheight(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE icfrwatm(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE qice1(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE qice2(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE snowheight(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
C--  Snow aging :
          snowAge(i,j,bi,bj) = thSIce_deltaT
     &                       + snowAge(i,j,bi,bj)*ageFac
          IF ( snowPrc(i,j,bi,bj).GT.0. _d 0 )
     &      snowAge(i,j,bi,bj) = snowAge(i,j,bi,bj)
     &          * EXP( - snowFac*snowPrc(i,j,bi,bj) )
c    &          * EXP( -(thSIce_deltaT*snowPrc(i,j,bi,bj)/rhos)
c    &                  /hNewSnowAge )
C-------
C note: Any flux of mass (here fresh water) that enter or leave the system
C       with a non zero energy HAS TO be counted: add snow precip.
          icFlxAtm(i,j,bi,bj) = icFlxAtm(i,j,bi,bj)
     &                        - Lfresh*snowPrc(i,j,bi,bj)
C--
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        tmpFac = 1. _d 0
        CALL DIAGNOSTICS_FILL(iceMask,'SI_FrcFx',0,1,1,bi,bj,myThid)
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
      DO j = jMin, jMax
       DO i = iMin, iMax

        tFrzOce(i,j) = -mu_Tf*sOceMxL(i,j,bi,bj)
        cphm    = cpwater*rhosw*hOceMxL(i,j,bi,bj)
        frzmltMxL(i,j) = ( tFrzOce(i,j)-tOceMxL(i,j,bi,bj) )
     &                 * cphm/ocean_deltaT
        iceFrac(i,j) = iceMask(i,j,bi,bj)
        flx2oc(i,j)  = icFlxSW(i,j,bi,bj)
C-------
#ifdef ALLOW_DBUG_THSICE
        IF ( dBug(i,j,bi,bj) ) THEN
         IF (frzmltMxL(i,j).GT.0. .OR. iceFrac(i,j).GT.0.) THEN
          WRITE(6,'(A,2I4,2I2)') 'ThSI_FWD: i,j=',i,j,bi,bj
          WRITE(6,1010) 'ThSI_FWD:-1- iceMask, hIc, hSn, Tsf  =',
     &                  iceFrac(i,j), iceHeight(i,j,bi,bj),
     &                  snowHeight(i,j,bi,bj), Tsrf(i,j,bi,bj)
          WRITE(6,1010) 'ThSI_FWD: ocTs,tFrzOce,frzmltMxL,Qnet=',
     &                     tOceMxL(i,j,bi,bj), tFrzOce(i,j),
     &                     frzmltMxL(i,j), Qnet(i,j,bi,bj)
         ENDIF
         IF (iceFrac(i,j).GT.0.)
     &    WRITE(6,1010) 'ThSI_FWD: icFrac,flxAtm,evpAtm,flxSnw=',
     &      iceFrac(i,j), icFlxAtm(i,j,bi,bj),
     &      icFrwAtm(i,j,bi,bj),-Lfresh*snowPrc(i,j,bi,bj)
        ENDIF
#endif
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceMask(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
#endif

      CALL THSICE_CALC_THICKN(
     I          bi, bj,
     I          iMin,iMax, jMin,jMax, dBugFlag,
     I          iceMask(siLo,sjLo,bi,bj), tFrzOce,
     I          tOceMxL(siLo,sjLo,bi,bj), v2ocMxL(siLo,sjLo,bi,bj),
     I          snowPrc(siLo,sjLo,bi,bj), prcAtm,
     I          sHeating(siLo,sjLo,bi,bj), flxCndBt(siLo,sjLo,bi,bj),
     U          iceFrac, iceHeight(siLo,sjLo,bi,bj),
     U          snowHeight(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     U          Qice1(siLo,sjLo,bi,bj), Qice2(siLo,sjLo,bi,bj),
     U          icFrwAtm(siLo,sjLo,bi,bj), frzmltMxL, flx2oc,
     O          frw2oc, fsalt,
     I          myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceMask(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE fsalt(:,:)  = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE flx2oc(:,:) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE frw2oc(:,:) = comlev1_bibj,key=ticekey,byte=isbyte
#endif
C--    Net fluxes :
      DO j = jMin, jMax
       DO i = iMin, iMax
c#ifdef ALLOW_AUTODIFF_TAMC
c         ikey_1 = i
c    &         + sNx*(j-1)
c    &         + sNx*sNy*act1
c    &         + sNx*sNy*max1*act2
c    &         + sNx*sNy*max1*max2*act3
c    &         + sNx*sNy*max1*max2*max3*act4
c#endif /* ALLOW_AUTODIFF_TAMC */
c#ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE  icemask(i,j,bi,bj) = comlev1_thsice_1, key=ikey_1
c#endif
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
C-     weighted average net fluxes:
c#ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE  fsalt(i,j) = comlev1_thsice_1, key=ikey_1
cCADJ STORE  flx2oc(i,j) = comlev1_thsice_1, key=ikey_1
cCADJ STORE  frw2oc(i,j) = comlev1_thsice_1, key=ikey_1
cCADJ STORE  icemask(i,j,bi,bj) = comlev1_thsice_1, key=ikey_1
c#endif
          icFrac = iceMask(i,j,bi,bj)
          opFrac= 1. _d 0-icFrac
#ifdef ALLOW_ATM2D
          pass_qnet(i,j) = pass_qnet(i,j) - icFrac*flx2oc(i,j)
          pass_evap(i,j) = pass_evap(i,j) - icFrac*frw2oc(i,j)/rhofw
          sFluxFromIce(i,j) = -icFrac*fsalt(i,j)
#else
          icFlxAtm(i,j,bi,bj) = icFrac*icFlxAtm(i,j,bi,bj)
     &                        - opFrac*Qnet(i,j,bi,bj)
          icFrwAtm(i,j,bi,bj) = icFrac*icFrwAtm(i,j,bi,bj)
     &                        + opFrac*EmPmR(i,j,bi,bj)
          Qnet(i,j,bi,bj) = -icFrac*flx2oc(i,j) + opFrac*Qnet(i,j,bi,bj)
          EmPmR(i,j,bi,bj)= -icFrac*frw2oc(i,j)
     &                    +  opFrac*EmPmR(i,j,bi,bj)
          saltFlux(i,j,bi,bj) = -icFrac*fsalt(i,j)
#endif

#ifdef ALLOW_DBUG_THSICE
          IF (dBug(i,j,bi,bj)) WRITE(6,1010)
     &          'ThSI_FWD:-3- iceFrac, hIc, hSn, Qnet =',
     &           iceFrac(i,j), iceHeight(i,j,bi,bj),
     &           snowHeight(i,j,bi,bj), Qnet(i,j,bi,bj)
#endif

        ELSEIF (hOceMxL(i,j,bi,bj).GT.0. _d 0) THEN
          icFlxAtm(i,j,bi,bj) = -Qnet(i,j,bi,bj)
          icFrwAtm(i,j,bi,bj) = EmPmR(i,j,bi,bj)
        ELSE
          icFlxAtm(i,j,bi,bj) = 0. _d 0
          icFrwAtm(i,j,bi,bj) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C    part.3 : freezing of sea-water
C     over ice-free fraction and what is left from ice-covered fraction
C-------
      CALL THSICE_EXTEND(
     I          bi, bj,
     I          iMin,iMax, jMin,jMax, dBugFlag,
     I          frzmltMxL, tFrzOce,
     I          tOceMxL(siLo,sjLo,bi,bj),
     U          iceFrac, iceHeight(siLo,sjLo,bi,bj),
     U          snowHeight(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     U          Tice1(siLo,sjLo,bi,bj), Tice2(siLo,sjLo,bi,bj),
     U          Qice1(siLo,sjLo,bi,bj), Qice2(siLo,sjLo,bi,bj),
     O          flx2oc, frw2oc, fsalt,
     I          myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE snowHeight(:,:,bi,bj) =
CADJ &     comlev1_bibj,key=ticekey,byte=isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (frzmltMxL(i,j).GT.0. _d 0) THEN
C--    Net fluxes :
#ifdef ALLOW_ATM2D
          pass_qnet(i,j) = pass_qnet(i,j) - flx2oc(i,j)
          pass_evap(i,j) = pass_evap(i,j) - frw2oc(i,j)/rhofw
          sFluxFromIce(i,j)= sFluxFromIce(i,j) - fsalt(i,j)
#else
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) - flx2oc(i,j)
          EmPmR(i,j,bi,bj)= EmPmR(i,j,bi,bj)- frw2oc(i,j)
          saltFlux(i,j,bi,bj)=saltFlux(i,j,bi,bj) - fsalt(i,j)
#endif

#ifdef ALLOW_DBUG_THSICE
          IF (dBug(i,j,bi,bj)) WRITE(6,1010)
     &         'ThSI_FWD:-4- iceFrac, hIc, hSn, Qnet =',
     &           iceFrac(i,j), iceHeight(i,j,bi,bj),
     &           snowHeight(i,j,bi,bj), Qnet(i,j,bi,bj)
#endif
        ENDIF

        IF ( hOceMxL(i,j,bi,bj).GT.0. _d 0 )
     &    isIceFree(i,j) = iceMask(i,j,bi,bj).LE.0. _d 0
     &                  .AND.   iceFrac(i,j) .LE.0. _d 0
        IF ( iceFrac(i,j) .GT. 0. _d 0 ) THEN
          iceMask(i,j,bi,bj)=iceFrac(i,j)
          IF ( snowHeight(i,j,bi,bj).EQ.0. _d 0 )
     &     snowAge(i,j,bi,bj) = 0. _d 0
        ELSE
          iceMask(i,j,bi,bj)  = 0. _d 0
          iceHeight(i,j,bi,bj)= 0. _d 0
          snowHeight(i,j,bi,bj)=0. _d 0
          snowAge(i,j,bi,bj)  = 0. _d 0
          Tsrf(i,j,bi,bj)     = tOceMxL(i,j,bi,bj)
          Tice1(i,j,bi,bj)    = 0. _d 0
          Tice2(i,j,bi,bj)    = 0. _d 0
          Qice1(i,j,bi,bj)    = Lfresh
          Qice2(i,j,bi,bj)    = Lfresh
        ENDIF
       ENDDO
      ENDDO

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE snowHeight(:,:,bi,bj) =
CADJ &     comlev1_bibj,key=ticekey,byte=isbyte
# endif
      DO j = jMin, jMax
       DO i = iMin, iMax
C--     Compute Sea-Ice Loading (= mass of sea-ice + snow / area unit)
        sIceLoad(i,j,bi,bj) = ( snowHeight(i,j,bi,bj)*rhos
     &                         + iceHeight(i,j,bi,bj)*rhoi
     &                        )*iceMask(i,j,bi,bj)
#ifdef ALLOW_ATM2D
        pass_sIceLoad(i,j)=sIceLoad(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

      IF ( thSIceAdvScheme.GT.0 ) THEN
C--   note: those fluxes should to be added directly to Qnet, EmPmR & saltFlux
       DO j = jMin, jMax
        DO i = iMin, iMax
         IF ( hOceMxL(i,j,bi,bj).GT.0. _d 0 ) THEN
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) - oceQnet(i,j,bi,bj)
          EmPmR(i,j,bi,bj)= EmPmR(i,j,bi,bj)- oceFWfx(i,j,bi,bj)
          saltFlux(i,j,bi,bj)=saltFlux(i,j,bi,bj) - oceSflx(i,j,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

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
