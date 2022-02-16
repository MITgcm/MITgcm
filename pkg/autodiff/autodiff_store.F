#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

      SUBROUTINE AUTODIFF_STORE( myThid )

c     ==================================================================
c     SUBROUTINE autodiff_store
c     ==================================================================
c
c     packing for checkpoint storage
c
c     started: Matt Mazloff mmazloff@mit.edu 03-May-2007
c
c     ==================================================================
c     SUBROUTINE autodiff_store
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c**************************************
c These includes are needed for
c AD-checkpointing.
c They provide the fields to be stored.

#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"

#ifdef ALLOW_OBCS
# include "OBCS_FIELDS.h"
# include "OBCS_SEAICE.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
# ifdef ALLOW_BULKFORMULAE
#  include "EXF_CONSTANTS.h"
# endif
#endif /* ALLOW_EXF */
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif
#ifdef ALLOW_CTRL
# include "ctrl.h"
# include "CTRL_OBCS.h"
#endif
c     it is important that this file is included after SEAICE_SIZE.h
#include "AUTODIFF_STORE.h"

c     == routine arguments ==
c     note: under the multi-threaded model myIter and
c           myTime are local variables passed around as routine
c           arguments. Although this is fiddly it saves the need to
c           impose additional synchronisation points when they are
c           updated.
c     myThid - thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_AUTODIFF_TAMC
c     == local variables ==

      INTEGER bi,bj
      INTEGER I,J,K

c--   == end of interface ==

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_ENTER('AUTODIFF_STORE',myThid)
#endif

#ifdef AUTODIFF_USE_STORE_RESTORE
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          StoreDynVars2D(I,J,bi,bj,1) = etaN(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,2) = taux0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,3) = taux1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,4) = tauy0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,5) = tauy1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,6) = qnet0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,7) = qnet1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,8)  = empmr0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,9) = empmr1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,10) = sst0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,11) = sst1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,12) = sss0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,13) = sss1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,14) = saltflux0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,15) = saltflux1(I,J,bi,bj)
#ifdef SHORTWAVE_HEATING
          StoreDynVars2D(I,J,bi,bj,16) = qsw0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,17) = qsw1(I,J,bi,bj)
#else
          StoreDynVars2D(I,J,bi,bj,16) = 0.
          StoreDynVars2D(I,J,bi,bj,17) = 0.
#endif
#ifdef ATMOSPHERIC_LOADING
          StoreDynVars2D(I,J,bi,bj,18) = pload0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,19) = pload1(I,J,bi,bj)
#else
          StoreDynVars2D(I,J,bi,bj,18) = 0.
          StoreDynVars2D(I,J,bi,bj,19) = 0.
#endif
#ifdef EXACT_CONSERV
          StoreDynVars2D(I,J,bi,bj,20) = etaH(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,21) = dEtaHdt(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,22) = PmEpR(I,J,bi,bj)
#else
          StoreDynVars2D(I,J,bi,bj,20) = 0.
          StoreDynVars2D(I,J,bi,bj,21) = 0.
          StoreDynVars2D(I,J,bi,bj,22) = 0.
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      3D arrays
        DO K=1,Nr
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
#ifdef ALLOW_ADAMSBASHFORTH_3
           StoreDynVars3D(I,J,K,bi,bj,1)  = gtNm(I,J,K,bi,bj,1)
           StoreDynVars3D(I,J,K,bi,bj,2)  = gsNm(I,J,K,bi,bj,1)
           StoreDynVars3D(I,J,K,bi,bj,3)  = guNm(I,J,K,bi,bj,1)
           StoreDynVars3D(I,J,K,bi,bj,4)  = gvNm(I,J,K,bi,bj,1)
#else
           StoreDynVars3D(I,J,K,bi,bj,1)  = gtNm1(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,2)  = gsNm1(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,3)  = guNm1(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,4)  = gvNm1(I,J,K,bi,bj)
#endif
           StoreDynVars3D(I,J,K,bi,bj,5)  = theta(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,6)  = salt(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,7)  = uVel(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,8)  = vVel(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,9)  = wVel(I,J,K,bi,bj)
           StoreDynVars3D(I,J,K,bi,bj,10) = totPhiHyd(I,J,K,bi,bj)
#ifdef ALLOW_ADAMSBASHFORTH_3
           StoreDynVars3D(I,J,K,bi,bj,11) = gtNm(I,J,K,bi,bj,2)
           StoreDynVars3D(I,J,K,bi,bj,12) = gsNm(I,J,K,bi,bj,2)
           StoreDynVars3D(I,J,K,bi,bj,13) = guNm(I,J,K,bi,bj,2)
           StoreDynVars3D(I,J,K,bi,bj,14) = gvNm(I,J,K,bi,bj,2)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_EXF
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          StoreEXF1(I,J,bi,bj,1)  = hflux0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,2)  = hflux1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,3)  = sflux0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,4)  = sflux1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,5)  = ustress0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,6)  = ustress1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,7)  = vstress0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,8)  = vstress1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,9)  = wspeed0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,10) = wspeed1(I,J,bi,bj)
# ifdef SHORTWAVE_HEATING
          StoreEXF1(I,J,bi,bj,11) = swflux0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,12) = swflux1(I,J,bi,bj)
# else
          StoreEXF1(I,J,bi,bj,11) = 0.0
          StoreEXF1(I,J,bi,bj,12) = 0.0
# endif
# ifdef ALLOW_RUNOFF
          StoreEXF1(I,J,bi,bj,13) = runoff0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,14) = runoff1(I,J,bi,bj)
# else
          StoreEXF1(I,J,bi,bj,13) = 0.0
          StoreEXF1(I,J,bi,bj,14) = 0.0
# endif
# ifdef ATMOSPHERIC_LOADING
          StoreEXF1(I,J,bi,bj,15) = apressure0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,16) = apressure1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,17) = siceload(I,J,bi,bj)
# else
          StoreEXF1(I,J,bi,bj,15) = 0.0
          StoreEXF1(I,J,bi,bj,16) = 0.0
          StoreEXF1(I,J,bi,bj,17) = 0.0
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
          StoreEXF1(I,J,bi,bj,18) = climsss0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,19) = climsss1(I,J,bi,bj)
# else
          StoreEXF1(I,J,bi,bj,18) = 0.0
          StoreEXF1(I,J,bi,bj,19) = 0.0
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
          StoreEXF1(I,J,bi,bj,20) = climsst0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,21) = climsst1(I,J,bi,bj)
# else
          StoreEXF1(I,J,bi,bj,20) = 0.0
          StoreEXF1(I,J,bi,bj,21) = 0.0
# endif
# ifdef ALLOW_SALTFLX
          StoreEXF1(I,J,bi,bj,22) = saltflx0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,23) = saltflx1(I,J,bi,bj)
# else
          StoreEXF1(I,J,bi,bj,22) = 0.0
          StoreEXF1(I,J,bi,bj,23) = 0.0
# endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
# ifdef ALLOW_ATM_TEMP
          StoreEXF2(I,J,bi,bj,1) = aqh0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,2) = aqh1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,3) = atemp0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,4) = atemp1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,5) = precip0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,6) = precip1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,7) = lwflux0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,8) = lwflux1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,9)  = snowprecip0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,10) = snowprecip1(I,J,bi,bj)
#  ifdef ALLOW_READ_TURBFLUXES
          StoreEXF2(I,J,bi,bj,11) = hs0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,12) = hs1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,13) = hl0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,14) = hl1(I,J,bi,bj)
#  else
          StoreEXF2(I,J,bi,bj,11) = 0.0
          StoreEXF2(I,J,bi,bj,12) = 0.0
          StoreEXF2(I,J,bi,bj,13) = 0.0
          StoreEXF2(I,J,bi,bj,14) = 0.0
#  endif
#  ifdef EXF_READ_EVAP
          StoreEXF2(I,J,bi,bj,15) = evap0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,16) = evap1(I,J,bi,bj)
#  else
          StoreEXF2(I,J,bi,bj,15) = evap(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,16) = 0.0
#  endif /* EXF_READ_EVAP */
#  ifdef ALLOW_DOWNWARD_RADIATION
          StoreEXF2(I,J,bi,bj,17) = swdown0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,18) = swdown1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,19) = lwdown0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,20) = lwdown1(I,J,bi,bj)
#  else
          StoreEXF2(I,J,bi,bj,17) = 0.0
          StoreEXF2(I,J,bi,bj,18) = 0.0
          StoreEXF2(I,J,bi,bj,19) = 0.0
          StoreEXF2(I,J,bi,bj,20) = 0.0
#  endif
# endif /* ALLOW_ATM_TEMP */
          StoreEXF2(I,J,bi,bj,21) = uwind0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,22) = uwind1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,23) = vwind0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,24) = vwind1(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ECCO_CTRL_DEPRECATED
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
# ifdef ALLOW_UWIND_CONTROL
          StoreCTRLS1(I,J,bi,bj,1) = xx_uwind0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,2) = xx_uwind1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,1) = 0.0
          StoreCTRLS1(I,J,bi,bj,2) = 0.0
# endif
# ifdef ALLOW_VWIND_CONTROL
          StoreCTRLS1(I,J,bi,bj,3) = xx_vwind0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,4) = xx_vwind1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,3) = 0.0
          StoreCTRLS1(I,J,bi,bj,4) = 0.0
# endif
# ifdef ALLOW_ATEMP_CONTROL
          StoreCTRLS1(I,J,bi,bj,5) = xx_atemp0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,6) = xx_atemp1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,5) = 0.0
          StoreCTRLS1(I,J,bi,bj,6) = 0.0
# endif
# ifdef ALLOW_AQH_CONTROL
          StoreCTRLS1(I,J,bi,bj,7) = xx_aqh0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,8) = xx_aqh1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,7) = 0.0
          StoreCTRLS1(I,J,bi,bj,8) = 0.0
# endif
# ifdef ALLOW_PRECIP_CONTROL
          StoreCTRLS1(I,J,bi,bj,9) = xx_precip0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,10) = xx_precip1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,9) = 0.0
          StoreCTRLS1(I,J,bi,bj,10) = 0.0
# endif
# ifdef ALLOW_SNOWPRECIP_CONTROL
          StoreCTRLS1(I,J,bi,bj,11) = xx_snowprecip0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,12) = xx_snowprecip1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,11) = 0.0
          StoreCTRLS1(I,J,bi,bj,12) = 0.0
# endif
# ifdef ALLOW_SWFLUX_CONTROL
          StoreCTRLS1(I,J,bi,bj,13) = xx_swflux0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,14) = xx_swflux1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,13) = 0.0
          StoreCTRLS1(I,J,bi,bj,14) = 0.0
# endif
# ifdef ALLOW_SWDOWN_CONTROL
          StoreCTRLS1(I,J,bi,bj,15) = xx_swdown0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,16) = xx_swdown1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,15) = 0.0
          StoreCTRLS1(I,J,bi,bj,16) = 0.0
# endif
# ifdef ALLOW_LWDOWN_CONTROL
          StoreCTRLS1(I,J,bi,bj,17) = xx_lwdown0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,18) = xx_lwdown1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,17) = 0.0
          StoreCTRLS1(I,J,bi,bj,18) = 0.0
# endif
# ifdef ALLOW_APRESSURE_CONTROL
          StoreCTRLS1(I,J,bi,bj,19) = xx_apressure0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,20) = xx_apressure1(I,J,bi,bj)
# else
          StoreCTRLS1(I,J,bi,bj,19) = 0.0
          StoreCTRLS1(I,J,bi,bj,20) = 0.0
# endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ECCO_CTRL_DEPRECATED */
#endif /* ALLOW_EXF */

#ifdef ALLOW_SEAICE
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          StoreSEAICE(I,J,bi,bj, 1) = AREA(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj, 2) = HEFF(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj, 3) = HSNOW(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj, 4) = RUNOFF(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj, 5) = UICE(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj, 6) = VICE(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj, 7) = ZETA(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj, 8) = ETA(I,J,bi,bj)
# ifdef SEAICE_CGRID
          StoreSEAICE(I,J,bi,bj, 9) = dwatn(I,J,bi,bj)
#  ifdef SEAICE_ALLOW_BOTTOMDRAG
          StoreSEAICE(I,J,bi,bj,10) = cbotc(I,J,bi,bj)
#  else
          StoreSEAICE(I,J,bi,bj,10) = 0.0
#  endif /* SEAICE_ALLOW_BOTTOMDRAG */
          StoreSEAICE(I,J,bi,bj,11) = stressDivergenceX(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,12) = stressDivergenceY(I,J,bi,bj)
# else
          StoreSEAICE(I,J,bi,bj, 9) = 0.0
          StoreSEAICE(I,J,bi,bj,10) = 0.0
          StoreSEAICE(I,J,bi,bj,11) = 0.0
          StoreSEAICE(I,J,bi,bj,12) = 0.0
# endif /* SEAICE_CGRID */
# ifdef SEAICE_ALLOW_EVP
          StoreSEAICE(I,J,bi,bj,13) = seaice_sigma1(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,14) = seaice_sigma2(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,15) = seaice_sigma12(I,J,bi,bj)
# else
          StoreSEAICE(I,J,bi,bj,13) = 0.0
          StoreSEAICE(I,J,bi,bj,14) = 0.0
          StoreSEAICE(I,J,bi,bj,15) = 0.0
# endif /* SEAICE_ALLOW_EVP */
# ifdef SEAICE_VARIABLE_SALINITY
          StoreSEAICE(I,J,bi,bj,16) = HSALT(I,J,bi,bj)
# else
          StoreSEAICE(I,J,bi,bj,16) = 0.0
# endif
         ENDDO
        ENDDO
C--   this 3D array comes last because it has a variable size nITD
        DO k=1,nITD
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           StoreSEAICE(I,J,bi,bj,16+k) = TICES(I,J,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C
       ENDDO
      ENDDO
#endif /* ALLOW_SEAICE */

#endif /* AUTODIFF_USE_STORE_RESTORE */

#if ( defined ALLOW_OBCS && defined AUTODIFF_USE_STORE_RESTORE_OBCS )
# ifdef ALLOW_OBCS_NORTH
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO I=1-OLx,sNx+OLx
          StoreOBCSN(I,K,bi,bj,1)  = OBNu(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,2)  = OBNv(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,3)  = OBNt(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,4)  = OBNs(I,K,bi,bj)
#  ifdef ALLOW_OBCS_PRESCRIBE
          StoreOBCSN(I,K,bi,bj,5)  = OBNu0(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,6)  = OBNv0(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,7)  = OBNt0(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,8)  = OBNs0(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,9)  = OBNu1(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,10) = OBNv1(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,11) = OBNt1(I,K,bi,bj)
          StoreOBCSN(I,K,bi,bj,12) = OBNs1(I,K,bi,bj)
#  endif
#  ifdef ALLOW_OBCSN_CONTROL
          StoreOBCSN(I,K,bi,bj,13) = xx_obcsn0(I,K,bi,bj,1)
          StoreOBCSN(I,K,bi,bj,14) = xx_obcsn0(I,K,bi,bj,2)
          StoreOBCSN(I,K,bi,bj,15) = xx_obcsn0(I,K,bi,bj,3)
          StoreOBCSN(I,K,bi,bj,16) = xx_obcsn0(I,K,bi,bj,4)
          StoreOBCSN(I,K,bi,bj,17) = xx_obcsn1(I,K,bi,bj,1)
          StoreOBCSN(I,K,bi,bj,18) = xx_obcsn1(I,K,bi,bj,2)
          StoreOBCSN(I,K,bi,bj,19) = xx_obcsn1(I,K,bi,bj,3)
          StoreOBCSN(I,K,bi,bj,20) = xx_obcsn1(I,K,bi,bj,4)
#  else
          StoreOBCSN(I,K,bi,bj,13) = 0.0
          StoreOBCSN(I,K,bi,bj,14) = 0.0
          StoreOBCSN(I,K,bi,bj,15) = 0.0
          StoreOBCSN(I,K,bi,bj,16) = 0.0
          StoreOBCSN(I,K,bi,bj,17) = 0.0
          StoreOBCSN(I,K,bi,bj,18) = 0.0
          StoreOBCSN(I,K,bi,bj,19) = 0.0
          StoreOBCSN(I,K,bi,bj,20) = 0.0
#  endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif /* ALLOW_OBCS_NORTH */

# ifdef ALLOW_OBCS_SOUTH
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO I=1-OLx,sNx+OLx
          StoreOBCSS(I,K,bi,bj,1)  = OBSu(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,2)  = OBSv(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,3)  = OBSt(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,4)  = OBSs(I,K,bi,bj)
#  ifdef ALLOW_OBCS_PRESCRIBE
          StoreOBCSS(I,K,bi,bj,5)  = OBSu0(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,6)  = OBSv0(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,7)  = OBSt0(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,8)  = OBSs0(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,9)  = OBSu1(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,10) = OBSv1(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,11) = OBSt1(I,K,bi,bj)
          StoreOBCSS(I,K,bi,bj,12) = OBSs1(I,K,bi,bj)
#  endif
#  ifdef ALLOW_OBCSS_CONTROL
          StoreOBCSS(I,K,bi,bj,13) = xx_obcss0(I,K,bi,bj,1)
          StoreOBCSS(I,K,bi,bj,14) = xx_obcss0(I,K,bi,bj,2)
          StoreOBCSS(I,K,bi,bj,15) = xx_obcss0(I,K,bi,bj,3)
          StoreOBCSS(I,K,bi,bj,16) = xx_obcss0(I,K,bi,bj,4)
          StoreOBCSS(I,K,bi,bj,17) = xx_obcss1(I,K,bi,bj,1)
          StoreOBCSS(I,K,bi,bj,18) = xx_obcss1(I,K,bi,bj,2)
          StoreOBCSS(I,K,bi,bj,19) = xx_obcss1(I,K,bi,bj,3)
          StoreOBCSS(I,K,bi,bj,20) = xx_obcss1(I,K,bi,bj,4)
#  else
          StoreOBCSS(I,K,bi,bj,13) = 0.0
          StoreOBCSS(I,K,bi,bj,14) = 0.0
          StoreOBCSS(I,K,bi,bj,15) = 0.0
          StoreOBCSS(I,K,bi,bj,16) = 0.0
          StoreOBCSS(I,K,bi,bj,17) = 0.0
          StoreOBCSS(I,K,bi,bj,18) = 0.0
          StoreOBCSS(I,K,bi,bj,19) = 0.0
          StoreOBCSS(I,K,bi,bj,20) = 0.0
#  endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif /* ALLOW_OBCS_SOUTH */

# ifdef ALLOW_OBCS_EAST
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO J=1-OLy,sNy+OLy
          StoreOBCSE(J,K,bi,bj,1)  = OBEu(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,2)  = OBEv(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,3)  = OBEt(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,4)  = OBEs(J,K,bi,bj)
#  ifdef ALLOW_OBCS_PRESCRIBE
          StoreOBCSE(J,K,bi,bj,5)  = OBEu0(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,6)  = OBEv0(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,7)  = OBEt0(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,8)  = OBEs0(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,9)  = OBEu1(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,10) = OBEv1(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,11) = OBEt1(J,K,bi,bj)
          StoreOBCSE(J,K,bi,bj,12) = OBEs1(J,K,bi,bj)
#  endif
#  ifdef ALLOW_OBCSE_CONTROL
          StoreOBCSE(J,K,bi,bj,13) = xx_obcse0(J,K,bi,bj,1)
          StoreOBCSE(J,K,bi,bj,14) = xx_obcse0(J,K,bi,bj,2)
          StoreOBCSE(J,K,bi,bj,15) = xx_obcse0(J,K,bi,bj,3)
          StoreOBCSE(J,K,bi,bj,16) = xx_obcse0(J,K,bi,bj,4)
          StoreOBCSE(J,K,bi,bj,17) = xx_obcse1(J,K,bi,bj,1)
          StoreOBCSE(J,K,bi,bj,18) = xx_obcse1(J,K,bi,bj,2)
          StoreOBCSE(J,K,bi,bj,19) = xx_obcse1(J,K,bi,bj,3)
          StoreOBCSE(J,K,bi,bj,20) = xx_obcse1(J,K,bi,bj,4)
#  else
          StoreOBCSE(J,K,bi,bj,13) = 0.0
          StoreOBCSE(J,K,bi,bj,14) = 0.0
          StoreOBCSE(J,K,bi,bj,15) = 0.0
          StoreOBCSE(J,K,bi,bj,16) = 0.0
          StoreOBCSE(J,K,bi,bj,17) = 0.0
          StoreOBCSE(J,K,bi,bj,18) = 0.0
          StoreOBCSE(J,K,bi,bj,19) = 0.0
          StoreOBCSE(J,K,bi,bj,20) = 0.0
#  endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif /* ALLOW_OBCS_EAST */

# ifdef ALLOW_OBCS_WEST
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO J=1-OLy,sNy+OLy
          StoreOBCSW(J,K,bi,bj,1)  = OBWu(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,2)  = OBWv(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,3)  = OBWt(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,4)  = OBWs(J,K,bi,bj)
#  ifdef ALLOW_OBCS_PRESCRIBE
          StoreOBCSW(J,K,bi,bj,5)  = OBWu0(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,6)  = OBWv0(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,7)  = OBWt0(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,8)  = OBWs0(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,9)  = OBWu1(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,10) = OBWv1(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,11) = OBWt1(J,K,bi,bj)
          StoreOBCSW(J,K,bi,bj,12) = OBWs1(J,K,bi,bj)
#  endif
#  ifdef ALLOW_OBCSW_CONTROL
          StoreOBCSW(J,K,bi,bj,13) = xx_obcsw0(J,K,bi,bj,1)
          StoreOBCSW(J,K,bi,bj,14) = xx_obcsw0(J,K,bi,bj,2)
          StoreOBCSW(J,K,bi,bj,15) = xx_obcsw0(J,K,bi,bj,3)
          StoreOBCSW(J,K,bi,bj,16) = xx_obcsw0(J,K,bi,bj,4)
          StoreOBCSW(J,K,bi,bj,17) = xx_obcsw1(J,K,bi,bj,1)
          StoreOBCSW(J,K,bi,bj,18) = xx_obcsw1(J,K,bi,bj,2)
          StoreOBCSW(J,K,bi,bj,19) = xx_obcsw1(J,K,bi,bj,3)
          StoreOBCSW(J,K,bi,bj,20) = xx_obcsw1(J,K,bi,bj,4)
#  else
          StoreOBCSW(J,K,bi,bj,13) = 0.0
          StoreOBCSW(J,K,bi,bj,14) = 0.0
          StoreOBCSW(J,K,bi,bj,15) = 0.0
          StoreOBCSW(J,K,bi,bj,16) = 0.0
          StoreOBCSW(J,K,bi,bj,17) = 0.0
          StoreOBCSW(J,K,bi,bj,18) = 0.0
          StoreOBCSW(J,K,bi,bj,19) = 0.0
          StoreOBCSW(J,K,bi,bj,20) = 0.0
#  endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif /* ALLOW_OBCS_WEST */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('AUTODIFF_STORE',myThid)
#endif

#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

      RETURN
      END
