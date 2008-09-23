C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_store.F,v 1.6 2008/09/23 06:13:59 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

      subroutine autodiff_store( mythid )

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

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c**************************************
#ifdef ALLOW_AUTODIFF_TAMC

c These includes are needed for 
c AD-checkpointing. 
c They provide the fields to be stored.

# include "GRID.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "SURFACE.h"
# include "AUTODIFF.h"

# ifdef ALLOW_OBCS
#  include "OBCS.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_FIELDS.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
# endif /* ALLOW_EXF */
# ifdef ALLOW_SEAICE
#  include "SEAICE.h"
# endif

# include "ctrl.h"

c     == routine arguments ==
c     note: under the multi-threaded model myiter and 
c           mytime are local variables passed around as routine 
c           arguments. Although this is fiddly it saves the need to 
c           impose additional synchronisation points when they are 
c           updated.
c     mythid - thread number for this instance of the routine.
      integer mythid 

c     == local variables ==

      integer bi,bj
      integer I,J,K
       
c--   == end of interface ==

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_ENTER('AUTODIFF_STORE',myThid)
#endif
          
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifndef AUTODIFF_USE_OLDSTORE_2D
C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          StoreDynVars2D(I,J,bi,bj,1) = etan(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,2) = surfaceforcingTice(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,3) = taux0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,4) = taux1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,5) = tauy0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,6) = tauy1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,7) = qnet0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,8) = qnet1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,9)  = empmr0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,10) = empmr1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,11) = sst0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,12) = sst1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,13) = sss0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,14) = sss1(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,15) = saltflux0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,16) = saltflux1(I,J,bi,bj)
#ifdef SHORTWAVE_HEATING
          StoreDynVars2D(I,J,bi,bj,17) = qsw0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,18) = qsw1(I,J,bi,bj)
#else
          StoreDynVars2D(I,J,bi,bj,17) = 0.
          StoreDynVars2D(I,J,bi,bj,18) = 0.
#endif
#ifdef ATMOSPHERIC_LOADING
          StoreDynVars2D(I,J,bi,bj,19) = pload0(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,20) = pload1(I,J,bi,bj)
#else
          StoreDynVars2D(I,J,bi,bj,19) = 0.
          StoreDynVars2D(I,J,bi,bj,20) = 0.
#endif
#ifdef EXACT_CONSERV
          StoreDynVars2D(I,J,bi,bj,21) = etaH(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,22) = dEtaHdt(I,J,bi,bj)
          StoreDynVars2D(I,J,bi,bj,23) = PmEpR(I,J,bi,bj)
#else
          StoreDynVars2D(I,J,bi,bj,21) = 0.
          StoreDynVars2D(I,J,bi,bj,22) = 0.
          StoreDynVars2D(I,J,bi,bj,23) = 0.
#endif
         ENDDO
        ENDDO
#endif /* AUTODIFF_USE_OLDSTORE_2D */

#ifndef AUTODIFF_USE_OLDSTORE_3D
C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
       StoreDynVars3D(I,J,K,bi,bj,1) = gs(I,J,K,bi,bj)
       StoreDynVars3D(I,J,K,bi,bj,2) = gt(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,3) = gtnm1(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,4) = gsnm1(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,5) = gunm1(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,6) = gvnm1(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,7) = theta(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,8) = salt(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,9) = uvel(I,J,K,bi,bj)             
       StoreDynVars3D(I,J,K,bi,bj,10) = vvel(I,J,K,bi,bj)     
       StoreDynVars3D(I,J,K,bi,bj,11) = wvel(I,J,K,bi,bj)
       StoreDynVars3D(I,J,K,bi,bj,12) = totphihyd(I,J,K,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#endif /* AUTODIFF_USE_OLDSTORE_3D */

       ENDDO
      ENDDO
   
#ifdef ALLOW_EXF

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          StoreEXF1(I,J,bi,bj,1) = hflux0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,2) = hflux1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,3) = sflux0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,4) = sflux1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,5) = ustress0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,6) = ustress1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,7) = vstress0(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,8) = vstress1(I,J,bi,bj)
          StoreEXF1(I,J,bi,bj,9) = wspeed0(I,J,bi,bj)
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
         enddo
        enddo
       enddo
      enddo
       
# if (defined (ALLOW_ATM_TEMP) || defined (ALLOW_ATM_WIND))
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
#  ifdef ALLOW_ATM_TEMP
          StoreEXF2(I,J,bi,bj,1) = aqh0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,2) = aqh1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,3) = atemp0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,4) = atemp1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,5) = precip0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,6) = precip1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,7) = lwflux0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,8) = lwflux1(I,J,bi,bj)
#   ifdef EXF_READ_EVAP
          StoreEXF2(I,J,bi,bj,9) = evap0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,10) = evap1(I,J,bi,bj)
#   else
          StoreEXF2(I,J,bi,bj,9) = evap(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,10) = 0.0
#   endif /* EXF_READ_EVAP */
#   ifdef ALLOW_DOWNWARD_RADIATION
          StoreEXF2(I,J,bi,bj,11) = swdown0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,12) = swdown1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,13) = lwdown0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,14) = lwdown1(I,J,bi,bj)
#   else
          StoreEXF2(I,J,bi,bj,11) = 0.0
          StoreEXF2(I,J,bi,bj,12) = 0.0
          StoreEXF2(I,J,bi,bj,13) = 0.0
          StoreEXF2(I,J,bi,bj,14) = 0.0
#   endif
#  endif /* ALLOW_ATM_TEMP */
#  ifdef ALLOW_ATM_WIND
          StoreEXF2(I,J,bi,bj,15) = uwind0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,16) = uwind1(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,17) = vwind0(I,J,bi,bj)
          StoreEXF2(I,J,bi,bj,18) = vwind1(I,J,bi,bj)
#  else /* ALLOW_ATM_WIND undef */
          StoreEXF2(I,J,bi,bj,15) = 0.0
          StoreEXF2(I,J,bi,bj,16) = 0.0
          StoreEXF2(I,J,bi,bj,17) = 0.0
          StoreEXF2(I,J,bi,bj,18) = 0.0
#  endif  /* ALLOW_ATM_WIND */
         enddo
        enddo
       enddo
      enddo
# endif /* ALLOW_ATM_TEMP */

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
# ifdef ALLOW_UWIND_CONTROL
          StoreCTRLS1(I,J,bi,bj,1) = xx_uwind0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,2) = xx_uwind1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,1) = 0.0
          StoreCTRLS1(I,J,bi,bj,2) = 0.0
#  endif
# ifdef ALLOW_VWIND_CONTROL
          StoreCTRLS1(I,J,bi,bj,3) = xx_vwind0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,4) = xx_vwind1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,3) = 0.0
          StoreCTRLS1(I,J,bi,bj,4) = 0.0
#  endif
# ifdef ALLOW_ATEMP_CONTROL
          StoreCTRLS1(I,J,bi,bj,5) = xx_atemp0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,6) = xx_atemp1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,5) = 0.0
          StoreCTRLS1(I,J,bi,bj,6) = 0.0
#  endif
# ifdef ALLOW_AQH_CONTROL
          StoreCTRLS1(I,J,bi,bj,7) = xx_aqh0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,8) = xx_aqh1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,7) = 0.0
          StoreCTRLS1(I,J,bi,bj,8) = 0.0
#  endif
# ifdef ALLOW_PRECIP_CONTROL
          StoreCTRLS1(I,J,bi,bj,9) = xx_precip0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,10) = xx_precip1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,9) = 0.0
          StoreCTRLS1(I,J,bi,bj,10) = 0.0
#  endif
# ifdef ALLOW_SWFLUX_CONTROL
          StoreCTRLS1(I,J,bi,bj,11) = xx_swflux0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,12) = xx_swflux1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,11) = 0.0
          StoreCTRLS1(I,J,bi,bj,12) = 0.0
#  endif
#  ifdef ALLOW_SWDOWN_CONTROL
          StoreCTRLS1(I,J,bi,bj,13) = xx_swdown0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,14) = xx_swdown1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,13) = 0.0
          StoreCTRLS1(I,J,bi,bj,14) = 0.0
#  endif
#  ifdef ALLOW_LWDOWN_CONTROL
          StoreCTRLS1(I,J,bi,bj,15) = xx_lwdown0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,16) = xx_lwdown1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,15) = 0.0
          StoreCTRLS1(I,J,bi,bj,16) = 0.0
#  endif
#  ifdef ALLOW_APRESSURE_CONTROL
          StoreCTRLS1(I,J,bi,bj,17) = xx_apressure0(I,J,bi,bj)
          StoreCTRLS1(I,J,bi,bj,18) = xx_apressure1(I,J,bi,bj)
#  else            
          StoreCTRLS1(I,J,bi,bj,17) = 0.0
          StoreCTRLS1(I,J,bi,bj,18) = 0.0
#  endif
         enddo
        enddo
       enddo
      enddo
#endif /* ALLOW_EXF */

#ifdef ALLOW_OBCS
# ifdef ALLOW_OBCS_NORTH
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO I=1-Olx,sNx+Olx
          StoreOBCSN(I,Nr,bi,bj,1) = OBNt(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,2) = OBNs(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,3) = OBNu0(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,4) = OBNv0(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,5) = OBNt0(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,6) = OBNs0(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,7) = OBNu1(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,8) = OBNv1(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,9) = OBNt1(I,Nr,bi,bj)
          StoreOBCSN(I,Nr,bi,bj,10) = OBNs1(I,Nr,bi,bj)
#  ifdef ALLOW_OBCSN_CONTROL
          StoreOBCSN(I,Nr,bi,bj,11) = xx_obcsn0(I,Nr,bi,bj,1)
          StoreOBCSN(I,Nr,bi,bj,12) = xx_obcsn0(I,Nr,bi,bj,2)
          StoreOBCSN(I,Nr,bi,bj,13) = xx_obcsn0(I,Nr,bi,bj,3)
          StoreOBCSN(I,Nr,bi,bj,14) = xx_obcsn0(I,Nr,bi,bj,4)
          StoreOBCSN(I,Nr,bi,bj,15) = xx_obcsn1(I,Nr,bi,bj,1)
          StoreOBCSN(I,Nr,bi,bj,16) = xx_obcsn1(I,Nr,bi,bj,2)
          StoreOBCSN(I,Nr,bi,bj,17) = xx_obcsn1(I,Nr,bi,bj,3)
          StoreOBCSN(I,Nr,bi,bj,18) = xx_obcsn1(I,Nr,bi,bj,4)
# else
          StoreOBCSN(I,Nr,bi,bj,11) = 0.0
          StoreOBCSN(I,Nr,bi,bj,12) = 0.0
          StoreOBCSN(I,Nr,bi,bj,13) = 0.0
          StoreOBCSN(I,Nr,bi,bj,14) = 0.0
          StoreOBCSN(I,Nr,bi,bj,15) = 0.0
          StoreOBCSN(I,Nr,bi,bj,16) = 0.0
          StoreOBCSN(I,Nr,bi,bj,17) = 0.0
          StoreOBCSN(I,Nr,bi,bj,18) = 0.0
#  endif
         enddo
        enddo
       enddo
      enddo
# endif /* ALLOW_OBCS_NORTH */       

# ifdef ALLOW_OBCS_SOUTH
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO I=1-Olx,sNx+Olx
          StoreOBCSS(I,Nr,bi,bj,1) = OBSt(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,2) = OBSs(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,3) = OBSu0(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,4) = OBSv0(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,5) = OBSt0(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,6) = OBSs0(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,7) = OBSu1(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,8) = OBSv1(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,9) = OBSt1(I,Nr,bi,bj)
          StoreOBCSS(I,Nr,bi,bj,10)= OBSs1(I,Nr,bi,bj)
#  ifdef ALLOW_OBCSS_CONTROL
          StoreOBCSS(I,Nr,bi,bj,11) = xx_obcss0(I,Nr,bi,bj,1)
          StoreOBCSS(I,Nr,bi,bj,12) = xx_obcss0(I,Nr,bi,bj,2)
          StoreOBCSS(I,Nr,bi,bj,13) = xx_obcss0(I,Nr,bi,bj,3)
          StoreOBCSS(I,Nr,bi,bj,14) = xx_obcss0(I,Nr,bi,bj,4)
          StoreOBCSS(I,Nr,bi,bj,15) = xx_obcss1(I,Nr,bi,bj,1)
          StoreOBCSS(I,Nr,bi,bj,16) = xx_obcss1(I,Nr,bi,bj,2)
          StoreOBCSS(I,Nr,bi,bj,17) = xx_obcss1(I,Nr,bi,bj,3)
          StoreOBCSS(I,Nr,bi,bj,18) = xx_obcss1(I,Nr,bi,bj,4)
# else
          StoreOBCSS(I,Nr,bi,bj,11) = 0.0
          StoreOBCSS(I,Nr,bi,bj,12) = 0.0
          StoreOBCSS(I,Nr,bi,bj,13) = 0.0
          StoreOBCSS(I,Nr,bi,bj,14) = 0.0
          StoreOBCSS(I,Nr,bi,bj,15) = 0.0
          StoreOBCSS(I,Nr,bi,bj,16) = 0.0
          StoreOBCSS(I,Nr,bi,bj,17) = 0.0
          StoreOBCSS(I,Nr,bi,bj,18) = 0.0
#  endif
         enddo
        enddo
       enddo
      enddo
# endif /* ALLOW_OBCS_SOUTH */       

# ifdef ALLOW_OBCS_EAST
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          StoreOBCSE(J,Nr,bi,bj,1) = OBEt(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,2) = OBEs(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,3) = OBEu0(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,4) = OBEv0(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,5) = OBEt0(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,6) = OBEs0(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,7) = OBEu1(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,8) = OBEv1(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,9) = OBEt1(J,Nr,bi,bj)
          StoreOBCSE(J,Nr,bi,bj,10)= OBEs1(J,Nr,bi,bj)
#  ifdef ALLOW_OBCSE_CONTROL
          StoreOBCSE(J,Nr,bi,bj,11) = xx_obcse0(J,Nr,bi,bj,1)
          StoreOBCSE(J,Nr,bi,bj,12) = xx_obcse0(J,Nr,bi,bj,2)
          StoreOBCSE(J,Nr,bi,bj,13) = xx_obcse0(J,Nr,bi,bj,3)
          StoreOBCSE(J,Nr,bi,bj,14) = xx_obcse0(J,Nr,bi,bj,4)
          StoreOBCSE(J,Nr,bi,bj,15) = xx_obcse1(J,Nr,bi,bj,1)
          StoreOBCSE(J,Nr,bi,bj,16) = xx_obcse1(J,Nr,bi,bj,2)
          StoreOBCSE(J,Nr,bi,bj,17) = xx_obcse1(J,Nr,bi,bj,3)
          StoreOBCSE(J,Nr,bi,bj,18) = xx_obcse1(J,Nr,bi,bj,4)
# else
          StoreOBCSE(J,Nr,bi,bj,11) = 0.0
          StoreOBCSE(J,Nr,bi,bj,12) = 0.0
          StoreOBCSE(J,Nr,bi,bj,13) = 0.0
          StoreOBCSE(J,Nr,bi,bj,14) = 0.0
          StoreOBCSE(J,Nr,bi,bj,15) = 0.0
          StoreOBCSE(J,Nr,bi,bj,16) = 0.0
          StoreOBCSE(J,Nr,bi,bj,17) = 0.0
          StoreOBCSE(J,Nr,bi,bj,18) = 0.0
#  endif
         enddo
        enddo
       enddo
      enddo
# endif /* ALLOW_OBCS_EAST */     

# ifdef ALLOW_OBCS_WEST
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          StoreOBCSW(J,Nr,bi,bj,1) = OBWt(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,2) = OBWs(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,3) = OBWu0(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,4) = OBWv0(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,5) = OBWt0(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,6) = OBWs0(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,7) = OBWu1(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,8) = OBWv1(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,9) = OBWt1(J,Nr,bi,bj)
          StoreOBCSW(J,Nr,bi,bj,10)= OBWs1(J,Nr,bi,bj)
#  ifdef ALLOW_OBCSW_CONTROL
          StoreOBCSW(J,Nr,bi,bj,11) = xx_obcsw0(J,Nr,bi,bj,1)
          StoreOBCSW(J,Nr,bi,bj,12) = xx_obcsw0(J,Nr,bi,bj,2)
          StoreOBCSW(J,Nr,bi,bj,13) = xx_obcsw0(J,Nr,bi,bj,3)
          StoreOBCSW(J,Nr,bi,bj,14) = xx_obcsw0(J,Nr,bi,bj,4)
          StoreOBCSW(J,Nr,bi,bj,15) = xx_obcsw1(J,Nr,bi,bj,1)
          StoreOBCSW(J,Nr,bi,bj,16) = xx_obcsw1(J,Nr,bi,bj,2)
          StoreOBCSW(J,Nr,bi,bj,17) = xx_obcsw1(J,Nr,bi,bj,3)
          StoreOBCSW(J,Nr,bi,bj,18) = xx_obcsw1(J,Nr,bi,bj,4)
# else
          StoreOBCSW(J,Nr,bi,bj,11) = 0.0
          StoreOBCSW(J,Nr,bi,bj,12) = 0.0
          StoreOBCSW(J,Nr,bi,bj,13) = 0.0
          StoreOBCSW(J,Nr,bi,bj,14) = 0.0
          StoreOBCSW(J,Nr,bi,bj,15) = 0.0
          StoreOBCSW(J,Nr,bi,bj,16) = 0.0
          StoreOBCSW(J,Nr,bi,bj,17) = 0.0
          StoreOBCSW(J,Nr,bi,bj,18) = 0.0
#  endif
         enddo
        enddo
       enddo
      enddo
# endif /* ALLOW_OBCS_WEST */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          StoreSEAICE(I,J,bi,bj,1) = AREA(I,J,1,bi,bj)
          StoreSEAICE(I,J,bi,bj,2) = AREA(I,J,2,bi,bj)
          StoreSEAICE(I,J,bi,bj,3) = AREA(I,J,3,bi,bj)
          StoreSEAICE(I,J,bi,bj,4) = HEFF(I,J,1,bi,bj)
          StoreSEAICE(I,J,bi,bj,5) = HEFF(I,J,2,bi,bj)
          StoreSEAICE(I,J,bi,bj,6) = HEFF(I,J,3,bi,bj)           
          StoreSEAICE(I,J,bi,bj,7) = HSNOW(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,8) = TICE(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,9) = RUNOFF(I,J,bi,bj)
# ifdef SEAICE_MULTICATEGORY
          StoreSEAICE(I,J,bi,bj,10) = TICES(I,J,bi,bj)
# else
          StoreSEAICE(I,J,bi,bj,10) = 0.0
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
          StoreSEAICE(I,J,bi,bj,11) = UICE(I,J,1,bi,bj)
          StoreSEAICE(I,J,bi,bj,12) = UICE(I,J,2,bi,bj)
          StoreSEAICE(I,J,bi,bj,13) = UICE(I,J,3,bi,bj)
          StoreSEAICE(I,J,bi,bj,14) = VICE(I,J,1,bi,bj)
          StoreSEAICE(I,J,bi,bj,15) = VICE(I,J,2,bi,bj)
          StoreSEAICE(I,J,bi,bj,16) = VICE(I,J,3,bi,bj)
          StoreSEAICE(I,J,bi,bj,17) = ZETA(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,18) = ETA(I,J,bi,bj)
#  ifdef SEAICE_CGRID
          StoreSEAICE(I,J,bi,bj,19) = dwatn(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,20) = seaicemasku(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,21) = seaicemaskv(I,J,bi,bj)
#  else
          StoreSEAICE(I,J,bi,bj,19) = 0.0      
          StoreSEAICE(I,J,bi,bj,20) = 0.0           
          StoreSEAICE(I,J,bi,bj,21) = 0.0
#  endif /* SEAICE_CGRID */
#  ifdef SEAICE_ALLOW_EVP
          StoreSEAICE(I,J,bi,bj,22) = seaice_sigma1(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,23) = seaice_sigma2(I,J,bi,bj)
          StoreSEAICE(I,J,bi,bj,24) = seaice_sigma12(I,J,bi,bj)
#  else
          StoreSEAICE(I,J,bi,bj,22) = 0.0      
          StoreSEAICE(I,J,bi,bj,23) = 0.0           
          StoreSEAICE(I,J,bi,bj,24) = 0.0
#  endif /* SEAICE_ALLOW_EVP */
# else
          StoreSEAICE(I,J,bi,bj,11) = 0.0      
          StoreSEAICE(I,J,bi,bj,12) = 0.0           
          StoreSEAICE(I,J,bi,bj,13) = 0.0
          StoreSEAICE(I,J,bi,bj,14) = 0.0      
          StoreSEAICE(I,J,bi,bj,15) = 0.0           
          StoreSEAICE(I,J,bi,bj,16) = 0.0
          StoreSEAICE(I,J,bi,bj,17) = 0.0      
          StoreSEAICE(I,J,bi,bj,18) = 0.0           
          StoreSEAICE(I,J,bi,bj,19) = 0.0      
          StoreSEAICE(I,J,bi,bj,20) = 0.0           
          StoreSEAICE(I,J,bi,bj,21) = 0.0
          StoreSEAICE(I,J,bi,bj,22) = 0.0      
          StoreSEAICE(I,J,bi,bj,23) = 0.0           
          StoreSEAICE(I,J,bi,bj,24) = 0.0
# endif /* SEAICE_ALLOW_DYNAMICS */
# ifdef SEAICE_SALINITY
          StoreSEAICE(I,J,bi,bj,25) = HSALT(I,J,bi,bj)
# else
          StoreSEAICE(I,J,bi,bj,25) = 0.0
# endif
         enddo
        enddo
       enddo
      enddo
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_LEAVE('AUTODIFF_STORE',myThid)
#endif

#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

      return
      end

