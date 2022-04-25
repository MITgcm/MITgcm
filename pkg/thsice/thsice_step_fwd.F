#include "THSICE_OPTIONS.h"
#ifdef ALLOW_ATM2D
# include "ctrparam.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_STEP_FWD
C     !INTERFACE:
      SUBROUTINE THSICE_STEP_FWD(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             prcAtm, snowPrc, qPrcRnO,
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
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
C     snowPrc :: snow precipitation               [kg/m2/s]
C     qPrcRnO :: Energy content of Precip+RunOff (+=down) [W/m2]
C     myTime  :: current Time of simulation [s]
C     myIter  :: current Iteration number in simulation
C     myThid  :: my Thread Id number
C-- Use fluxes hold in commom blocks
C- input:
C     icFlxSW :: net short-wave heat flux (+=down) below sea-ice, into ocean
C     icFlxAtm  :: net Atmospheric surf. heat flux over sea-ice [W/m2], (+=down)
C     icFrwAtm  :: evaporation over sea-ice to the atmosphere   [kg/m2/s] (+=up)
C- output
C     icFlxAtm  :: net Atmospheric surf. heat flux over ice+ocean [W/m2], (+=down)
C     icFrwAtm  :: net fresh-water flux (E-P) from the atmosphere [kg/m2/s] (+=up)
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL prcAtm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL snowPrc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qPrcRnO(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     iceFrac   :: fraction of grid area covered in ice
C     flx2oc    :: net heat flux from the ice to the ocean (+=down) [W/m2]
C     frw2oc    :: fresh-water flux from the ice to the ocean (+=down)
C     fsalt     :: mass salt flux to the ocean                (+=down)
C     frzSeaWat :: seawater freezing rate (expressed as mass flux) [kg/m^2/s]
C     frzmltMxL :: ocean mixed-layer freezing/melting potential [W/m2]
C     tFrzOce   :: sea-water freezing temperature [oC] (function of S)
C     isIceFree :: true for ice-free grid-cell that remains ice-free
C     ageFac    :: snow aging factor [1]
C     snowFac   :: snowing refreshing-age factor [units of 1/snowPr]
#ifdef ALLOW_BULK_FORCE
      LOGICAL isIceFree(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
      _RL     iceFrac  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     flx2oc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     frw2oc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     fsalt    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     frzSeaWat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tFrzOce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     frzmltMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ageFac
      _RL snowFac
      _RL cphm
      _RL opFrac, icFrac
      INTEGER i,j
      LOGICAL dBugFlag

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey
#endif
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
#ifdef ALLOW_BULK_FORCE
          isIceFree(i,j) = .FALSE.
#endif
#ifdef ALLOW_ATM2D
          sFluxFromIce(i,j) = 0. _d 0
#else
          saltFlux(i,j,bi,bj) = 0. _d 0
#endif
          frzSeaWat(i,j) = 0. _d 0
#ifdef ALLOW_AUTODIFF
          iceFrac(i,j) = 0.
C-   set these arrays everywhere: overlap are not set and not used,
C     but some arrays are stored and storage includes overlap.
          flx2oc(i,j) = 0. _d 0
          frw2oc(i,j) = 0. _d 0
          fsalt (i,j) = 0. _d 0
c         tFrzOce  (i,j) = 0. _d 0
c         frzmltMxL(i,j) = 0. _d 0
#endif
       ENDDO
      ENDDO

      ageFac = 1. _d 0 - thSIce_deltaT/snowAgTime
      snowFac = thSIce_deltaT/(rhos*hNewSnowAge)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceMask(:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
C--  Snow aging :
          snowAge(i,j,bi,bj) = thSIce_deltaT
     &                       + snowAge(i,j,bi,bj)*ageFac
          IF ( snowPrc(i,j).GT.0. _d 0 )
     &      snowAge(i,j,bi,bj) = snowAge(i,j,bi,bj)
     &          * EXP( - snowFac*snowPrc(i,j) )
C-------
C note: Any flux of mass (here fresh water) that enter or leave the system
C       with a non zero energy HAS TO be counted: add snow precip.
          icFlxAtm(i,j,bi,bj) = icFlxAtm(i,j,bi,bj)
     &                        - Lfresh*snowPrc(i,j)
     &                        + qPrcRnO(i,j)
C--
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FRACT_FILL( snowPrc,
     I                       iceMask(1-OLx,1-OLy,bi,bj), oneRL, 1,
     I                              'SIsnwPrc', 0,1,2,bi,bj,myThid )
        CALL DIAGNOSTICS_FRACT_FILL( siceAlb, iceMask, oneRL, 1,
     I                              'SIalbedo', 0,1,1,bi,bj,myThid )
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
        flx2oc(i,j)  = icFlxSW(i,j,bi,bj) + qPrcRnO(i,j)
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
     &      icFrwAtm(i,j,bi,bj),-Lfresh*snowPrc(i,j)
        ENDIF
#endif
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceHeight (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE snowHeight(:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE iceMask   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE icFrwAtm  (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE tFrzOce               = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE frzMltMxl             = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE iceFrac               = comlev1_bibj,key=ticekey,kind=isbyte
#endif
      CALL THSICE_CALC_THICKN(
     I          bi, bj,
     I          iMin,iMax, jMin,jMax, dBugFlag,
     I          iceMask(siLo,sjLo,bi,bj), tFrzOce,
     I          tOceMxL(siLo,sjLo,bi,bj), v2ocMxL(siLo,sjLo,bi,bj),
     I          snowPrc(siLo,sjLo), prcAtm,
     I          sHeating(siLo,sjLo,bi,bj), flxCndBt(siLo,sjLo,bi,bj),
     U          iceFrac, iceHeight(siLo,sjLo,bi,bj),
     U          snowHeight(siLo,sjLo,bi,bj), Tsrf(siLo,sjLo,bi,bj),
     U          Qice1(siLo,sjLo,bi,bj), Qice2(siLo,sjLo,bi,bj),
     U          icFrwAtm(siLo,sjLo,bi,bj), frzmltMxL, flx2oc,
     O          frw2oc, fsalt, frzSeaWat,
     I          myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
C     these stores avoid recomputing s/r thsice_calc_thickn
CADJ STORE icFrwAtm(:,:,bi,bj)
CADJ &               = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE frzSeaWat = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE fsalt     = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE flx2oc    = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE frw2oc    = comlev1_bibj, key = ticekey, kind = isbyte
CADJ STORE frzmltMxL = comlev1_bibj, key = ticekey, kind = isbyte
#endif

C--   Net fluxes :
      DO j = jMin, jMax
       DO i = iMin, iMax
        IF (iceMask(i,j,bi,bj).GT.0. _d 0) THEN
C-     weighted average net fluxes:
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
#if defined(ALLOW_SALT_PLUME) || defined(ALLOW_ATM_COMPON_INTERF)
C-    All seawater freezing (no reduction by surf. melting) from CALC_THICKN
c         frzSeaWat(i,j) = icFrac*frzSeaWat(i,j)
C-    Net seawater freezing (underestimated if there is surf. melting or rain)
          frzSeaWat(i,j) = MAX( -icFrac*frw2oc(i,j), 0. _d 0 )
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
      DO j = 1-OLy, sNy+OLy
       DO i = 1-OLx, sNx+OLx
         flx2oc(i,j) = 0. _d 0
         frw2oc(i,j) = 0. _d 0
         fsalt (i,j) = 0. _d 0
       ENDDO
      ENDDO
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
C     these stores avoid recomputing s/r thsice_extend
CADJ STORE snowHeight(:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE iceHeight (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE iceMask   (:,:,bi,bj) = comlev1_bibj,key=ticekey,kind=isbyte
CADJ STORE iceFrac               = comlev1_bibj,key=ticekey,kind=isbyte
#endif
      DO j = jMin, jMax
       DO i = iMin, iMax
C--    Net fluxes : (only non-zero contribution where frzmltMxL > 0 )
#ifdef ALLOW_ATM2D
          pass_qnet(i,j) = pass_qnet(i,j) - flx2oc(i,j)
          pass_evap(i,j) = pass_evap(i,j) - frw2oc(i,j)/rhofw
          sFluxFromIce(i,j)= sFluxFromIce(i,j) - fsalt(i,j)
#else
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) - flx2oc(i,j)
          EmPmR(i,j,bi,bj)= EmPmR(i,j,bi,bj)- frw2oc(i,j)
          saltFlux(i,j,bi,bj)=saltFlux(i,j,bi,bj) - fsalt(i,j)
#endif
#if defined(ALLOW_SALT_PLUME) || defined(ALLOW_ATM_COMPON_INTERF)
          frzSeaWat(i,j) = frzSeaWat(i,j) + MAX(-frw2oc(i,j), 0. _d 0 )
#endif

#ifdef ALLOW_DBUG_THSICE
        IF (dBug(i,j,bi,bj)) WRITE(6,1010)
     &         'ThSI_FWD:-4- iceFrac, hIc, hSn, Qnet =',
     &           iceFrac(i,j), iceHeight(i,j,bi,bj),
     &           snowHeight(i,j,bi,bj), Qnet(i,j,bi,bj)
#endif

#ifdef ALLOW_BULK_FORCE
        IF ( hOceMxL(i,j,bi,bj).GT.0. _d 0 )
     &    isIceFree(i,j) = iceMask(i,j,bi,bj).LE.0. _d 0
     &                  .AND.   iceFrac(i,j) .LE.0. _d 0
#endif
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

#if defined(ALLOW_SALT_PLUME) || defined(ALLOW_ATM_COMPON_INTERF)
      IF ( useSALT_PLUME .OR. useCoupler ) THEN
        CALL THSICE_SALT_PLUME(
     I          sOceMxL(1-OLx,1-OLy,bi,bj),
     I          frzSeaWat,
     I          iMin,iMax, jMin,jMax, bi, bj,
     I          myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_SALT_PLUME or ALLOW_ATM_COMPON_INTERF */

      IF ( thSIceAdvScheme.LE.0 ) THEN
C- note: 1) regarding sIceLoad in ocean-dynamics, in case thSIceAdvScheme > 0,
C          compute sIceLoad in THSICE_DO_ADVECT after seaice advection is done.
C        2) regarding sIceLoad in seaice-dynamics, probably better not to update
C          sIceLoad here, to keep the balance between sIceLoad and adjusted Eta.
C        3) not sure in the case of no advection (thSIceAdvScheme=0) but using
C          seaice dynamics (unlikely senario anyway).
C--   Compute Sea-Ice Loading (= mass of sea-ice + snow / area unit)
       DO j = jMin, jMax
        DO i = iMin, iMax
          sIceLoad(i,j,bi,bj) = ( snowHeight(i,j,bi,bj)*rhos
     &                           + iceHeight(i,j,bi,bj)*rhoi
     &                          )*iceMask(i,j,bi,bj)
#ifdef ALLOW_ATM2D
          pass_sIceLoad(i,j)=sIceLoad(i,j,bi,bj)
#endif
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
