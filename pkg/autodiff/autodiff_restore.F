C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_restore.F,v 1.2 2007/06/27 22:14:28 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

      subroutine autodiff_restore( mythid )

c     ==================================================================
c     SUBROUTINE autodiff_restore
c     ==================================================================
c
c     packing for checkpoint storage
c
c     started: Matt Mazloff mmazloff@mit.edu 03-May-2007
c
c     ==================================================================
c     SUBROUTINE autodiff_restore
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

#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_ENTER('AUTODIFF_RESTORE',myThid)
#endif
          
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etan(I,J,bi,bj)               = StoreDynVars2D(I,J,bi,bj,1)
          surfaceforcingTice(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,2)
          taux0(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,3)
          taux1(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,4)
          tauy0(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,5)
          tauy1(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,6)
          qnet0(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,7)
          qnet1(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,8)
          empmr0(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,9)
          empmr1(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,10)
          sst0(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,11)
          sst1(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,12)
          sss0(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,13)
          sss1(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,14)
          saltflux0(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,15)
          saltflux1(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,16)
#ifdef SHORTWAVE_HEATING
          qsw0(I,J,bi,bj)    = StoreDynVars2D(I,J,bi,bj,17)
          qsw1(I,J,bi,bj)    = StoreDynVars2D(I,J,bi,bj,18)
#endif
#ifdef ATMOSPHERIC_LOADING
          pload0(I,J,bi,bj)  = StoreDynVars2D(I,J,bi,bj,19)
          pload1(I,J,bi,bj)  = StoreDynVars2D(I,J,bi,bj,20)
#endif
#ifdef EXACT_CONSERV
          etaH(I,J,bi,bj)    = StoreDynVars2D(I,J,bi,bj,21)
          dEtaHdt(I,J,bi,bj) = StoreDynVars2D(I,J,bi,bj,22)
          PmEpR(I,J,bi,bj)   = StoreDynVars2D(I,J,bi,bj,23)
#endif
         ENDDO
        ENDDO
C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
       gs(I,J,K,bi,bj)       = StoreDynVars3D(I,J,K,bi,bj,1)
       gt(I,J,K,bi,bj)       = StoreDynVars3D(I,J,K,bi,bj,2)
       gtnm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,3)
       gsnm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,4)
       gunm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,5)
       gvnm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,6)
       theta(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,7)        
       salt(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,8)       
       uvel(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,9)    
       vvel(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,10)     
       wvel(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,11)
       totphihyd(I,J,K,bi,bj)= StoreDynVars3D(I,J,K,bi,bj,12)
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
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
          hflux0(I,J,bi,bj)            = StoreEXF1(I,J,bi,bj,1)
          hflux1(I,J,bi,bj)            = StoreEXF1(I,J,bi,bj,2)
          sflux0(I,J,bi,bj)            = StoreEXF1(I,J,bi,bj,3)
          sflux1(I,J,bi,bj)            = StoreEXF1(I,J,bi,bj,4)
          ustress0(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,5)
          ustress1(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,6)
          vstress0(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,7)
          vstress1(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,8)
          wspeed0(I,J,bi,bj)           = StoreEXF1(I,J,bi,bj,9)
          wspeed1(I,J,bi,bj)           = StoreEXF1(I,J,bi,bj,10)
# ifdef SHORTWAVE_HEATING
          swflux0(I,J,bi,bj)           = StoreEXF1(I,J,bi,bj,11)
          swflux1(I,J,bi,bj)           = StoreEXF1(I,J,bi,bj,12)
# endif
# ifdef ALLOW_RUNOFF
          runoff0(I,J,bi,bj)           = StoreEXF1(I,J,bi,bj,13)
          runoff1(I,J,bi,bj)           = StoreEXF1(I,J,bi,bj,14)
# endif
# ifdef ATMOSPHERIC_LOADING
          apressure0(I,J,bi,bj)        = StoreEXF1(I,J,bi,bj,15)
          apressure1(I,J,bi,bj)        = StoreEXF1(I,J,bi,bj,16)
          siceload(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,17)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
          climsss0(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,18)
          climsss1(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,19)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
          climsst0(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,20)
          climsst1(I,J,bi,bj)          = StoreEXF1(I,J,bi,bj,21)
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
          aqh0(I,J,bi,bj)            = StoreEXF2(I,J,bi,bj,1)
          aqh1(I,J,bi,bj)            = StoreEXF2(I,J,bi,bj,2)
          atemp0(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,3)
          atemp1(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,4)
          precip0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,5)
          precip1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,6)
          lwflux0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,7) 
          lwflux1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,8)
#   ifdef EXF_READ_EVAP
          evap0(I,J,bi,bj)           = StoreEXF2(I,J,bi,bj,9)
          evap1(I,J,bi,bj)           = StoreEXF2(I,J,bi,bj,10)
#   else
          evap(I,J,bi,bj)            = StoreEXF2(I,J,bi,bj,9)
#   endif /* EXF_READ_EVAP */
#   ifdef ALLOW_DOWNWARD_RADIATION
          swdown0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,11)
          swdown1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,12)
          lwdown0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,13)
          lwdown1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,14)
#   endif
#  endif /* ALLOW_ATM_TEMP */
#  ifdef ALLOW_ATM_WIND
          uwind0(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,15)
          uwind1(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,16)
          vwind0(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,17)
          vwind1(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,18)
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
          xx_uwind0(I,J,bi,bj)     = StoreCTRLS1(I,J,bi,bj,1)
          xx_uwind1(I,J,bi,bj)     = StoreCTRLS1(I,J,bi,bj,2)
#  endif
# ifdef ALLOW_VWIND_CONTROL
          xx_vwind0(I,J,bi,bj)     = StoreCTRLS1(I,J,bi,bj,3)
          xx_vwind1(I,J,bi,bj)     = StoreCTRLS1(I,J,bi,bj,4)
#  endif
# ifdef ALLOW_ATEMP_CONTROL
          xx_atemp0(I,J,bi,bj)     = StoreCTRLS1(I,J,bi,bj,5)
          xx_atemp1(I,J,bi,bj)     = StoreCTRLS1(I,J,bi,bj,6)
#  endif
# ifdef ALLOW_AQH_CONTROL
          xx_aqh0(I,J,bi,bj)       = StoreCTRLS1(I,J,bi,bj,7)
          xx_aqh1(I,J,bi,bj)       = StoreCTRLS1(I,J,bi,bj,8)
#  endif
# ifdef ALLOW_PRECIP_CONTROL
          xx_precip0(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,9)
          xx_precip1(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,10)
#  endif
# ifdef ALLOW_SWFLUX_CONTROL
          xx_swflux0(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,11)
          xx_swflux1(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,12)
#  endif
# ifdef ALLOW_SWDOWN_CONTROL
          xx_swdown0(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,13)
          xx_swdown1(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,14)
#  endif
#  ifdef ALLOW_LWDOWN_CONTROL
          xx_lwdown0(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,15)
          xx_lwdown1(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,16)
#  endif
#  ifdef ALLOW_APRESSURE_CONTROL
          xx_apressure0(I,J,bi,bj) = StoreCTRLS1(I,J,bi,bj,17)
          xx_apressure1(I,J,bi,bj) = StoreCTRLS1(I,J,bi,bj,18)
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
          OBNt(I,Nr,bi,bj)    = StoreOBCSN(I,Nr,bi,bj,1)
          OBNs(I,Nr,bi,bj)    = StoreOBCSN(I,Nr,bi,bj,2)
          OBNu0(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,3)
          OBNv0(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,4)
          OBNt0(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,5)
          OBNs0(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,6)
          OBNu1(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,7)
          OBNv1(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,8)
          OBNt1(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,9)
          OBNs1(I,Nr,bi,bj)   = StoreOBCSN(I,Nr,bi,bj,10)
#  ifdef ALLOW_OBCSN_CONTROL
          xx_obcsn0(I,Nr,i,j,1)   = StoreOBCSN(I,Nr,bi,bj,11)
          xx_obcsn0(I,Nr,i,j,2)   = StoreOBCSN(I,Nr,bi,bj,12)
          xx_obcsn0(I,Nr,i,j,3)   = StoreOBCSN(I,Nr,bi,bj,13)
          xx_obcsn0(I,Nr,i,j,4)   = StoreOBCSN(I,Nr,bi,bj,14)
          xx_obcsn1(I,Nr,i,j,1)   = StoreOBCSN(I,Nr,bi,bj,15)
          xx_obcsn1(I,Nr,i,j,2)   = StoreOBCSN(I,Nr,bi,bj,16)
          xx_obcsn1(I,Nr,i,j,3)   = StoreOBCSN(I,Nr,bi,bj,17)
          xx_obcsn1(I,Nr,i,j,4)   = StoreOBCSN(I,Nr,bi,bj,18)
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
          OBSt(I,Nr,bi,bj)       = StoreOBCSS(I,Nr,bi,bj,1)
          OBSs(I,Nr,bi,bj)       = StoreOBCSS(I,Nr,bi,bj,2)
          OBSu0(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,3)
          OBSv0(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,4)
          OBSt0(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,5)
          OBSs0(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,6)
          OBSu1(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,7)
          OBSv1(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,8)
          OBSt1(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,9)
          OBSs1(I,Nr,bi,bj)      = StoreOBCSS(I,Nr,bi,bj,10)
#  ifdef ALLOW_OBCSS_CONTROL
          xx_obcss0(I,Nr,i,j,1)   = StoreOBCSS(I,Nr,bi,bj,11)
          xx_obcss0(I,Nr,i,j,2)   = StoreOBCSS(I,Nr,bi,bj,12)
          xx_obcss0(I,Nr,i,j,3)   = StoreOBCSS(I,Nr,bi,bj,13)
          xx_obcss0(I,Nr,i,j,4)   = StoreOBCSS(I,Nr,bi,bj,14)
          xx_obcss1(I,Nr,i,j,1)   = StoreOBCSS(I,Nr,bi,bj,15)
          xx_obcss1(I,Nr,i,j,2)   = StoreOBCSS(I,Nr,bi,bj,16)
          xx_obcss1(I,Nr,i,j,3)   = StoreOBCSS(I,Nr,bi,bj,17)
          xx_obcss1(I,Nr,i,j,4)   = StoreOBCSS(I,Nr,bi,bj,18)
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
          OBEt(J,Nr,bi,bj)      = StoreOBCSE(J,Nr,bi,bj,1)
          OBEs(J,Nr,bi,bj)      = StoreOBCSE(J,Nr,bi,bj,2)
          OBEu0(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,3)
          OBEv0(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,4)
          OBEt0(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,5)
          OBEs0(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,6)
          OBEu1(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,7)
          OBEv1(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,8)
          OBEt1(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,9)
          OBEs1(J,Nr,bi,bj)     = StoreOBCSE(J,Nr,bi,bj,10)
#  ifdef ALLOW_OBCSE_CONTROL
          xx_obcse0(J,Nr,bi,bj,1)     = StoreOBCSE(J,Nr,bi,bj,11)
          xx_obcse0(J,Nr,bi,bj,2)     = StoreOBCSE(J,Nr,bi,bj,12)
          xx_obcse0(J,Nr,bi,bj,3)     = StoreOBCSE(J,Nr,bi,bj,13)
          xx_obcse0(J,Nr,bi,bj,4)     = StoreOBCSE(J,Nr,bi,bj,14)
          xx_obcse1(J,Nr,bi,bj,1)     = StoreOBCSE(J,Nr,bi,bj,15)
          xx_obcse1(J,Nr,bi,bj,2)     = StoreOBCSE(J,Nr,bi,bj,16)
          xx_obcse1(J,Nr,bi,bj,3)     = StoreOBCSE(J,Nr,bi,bj,17)
          xx_obcse1(J,Nr,bi,bj,4)     = StoreOBCSE(J,Nr,bi,bj,18)
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
          OBWt(J,Nr,bi,bj)      = StoreOBCSW(J,Nr,bi,bj,1)
          OBWs(J,Nr,bi,bj)      = StoreOBCSW(J,Nr,bi,bj,1)
          OBWu0(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
          OBWv0(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
          OBWt0(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
          OBWs0(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
          OBWu1(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
          OBWv1(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
          OBWt1(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
          OBWs1(J,Nr,bi,bj)     = StoreOBCSW(J,Nr,bi,bj,1)
#  ifdef ALLOW_OBCSW_CONTROL
          xx_obcsw0(J,Nr,i,j,1) = StoreOBCSW(J,Nr,bi,bj,11)
          xx_obcsw0(J,Nr,i,j,2) = StoreOBCSW(J,Nr,bi,bj,12)
          xx_obcsw0(J,Nr,i,j,3) = StoreOBCSW(J,Nr,bi,bj,13)
          xx_obcsw0(J,Nr,i,j,4) = StoreOBCSW(J,Nr,bi,bj,14)
          xx_obcsw1(J,Nr,i,j,1) = StoreOBCSW(J,Nr,bi,bj,15)
          xx_obcsw1(J,Nr,i,j,2) = StoreOBCSW(J,Nr,bi,bj,16)
          xx_obcsw1(J,Nr,i,j,3) = StoreOBCSW(J,Nr,bi,bj,17)
          xx_obcsw1(J,Nr,i,j,4) = StoreOBCSW(J,Nr,bi,bj,18)
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
          AREA(I,J,1,bi,bj)   = StoreSEAICE(I,J,bi,bj,1)
          AREA(I,J,2,bi,bj)   = StoreSEAICE(I,J,bi,bj,2)
          AREA(I,J,3,bi,bj)   = StoreSEAICE(I,J,bi,bj,3)
          HEFF(I,J,1,bi,bj)   = StoreSEAICE(I,J,bi,bj,4)
          HEFF(I,J,2,bi,bj)   = StoreSEAICE(I,J,bi,bj,5)
          HEFF(I,J,3,bi,bj)   = StoreSEAICE(I,J,bi,bj,6)           
          HSNOW(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,7)      
          TICE(I,J,bi,bj)     = StoreSEAICE(I,J,bi,bj,8)
          RUNOFF(I,J,bi,bj)   = StoreSEAICE(I,J,bi,bj,9)
# ifdef SEAICE_MULTICATEGORY
          TICES(I,J,bi,bj)     = StoreSEAICE(I,J,bi,bj,10)
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
          UICE(I,J,1,bi,bj)    = StoreSEAICE(I,J,bi,bj,11)
          UICE(I,J,2,bi,bj)    = StoreSEAICE(I,J,bi,bj,12)
          UICE(I,J,3,bi,bj)    = StoreSEAICE(I,J,bi,bj,13)
          VICE(I,J,1,bi,bj)    = StoreSEAICE(I,J,bi,bj,14)
          VICE(I,J,2,bi,bj)    = StoreSEAICE(I,J,bi,bj,15)
          VICE(I,J,3,bi,bj)    = StoreSEAICE(I,J,bi,bj,16)
          ZETA(I,J,bi,bj)      = StoreSEAICE(I,J,bi,bj,17)
          ETA(I,J,bi,bj)       = StoreSEAICE(I,J,bi,bj,18)
#  ifdef SEAICE_CGRID
          dwatn(I,J,bi,bj)          = StoreSEAICE(I,J,bi,bj,19)
          seaicemasku(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,20)
          seaicemaskv(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,21)
#  endif /* SEAICE_CGRID */
#  ifdef SEAICE_ALLOW_EVP
          seaice_sigma1(I,J,bi,bj)  = StoreSEAICE(I,J,bi,bj,22)
          seaice_sigma2(I,J,bi,bj)  = StoreSEAICE(I,J,bi,bj,23)
          seaice_sigma12(I,J,bi,bj) = StoreSEAICE(I,J,bi,bj,24)
#  endif /* SEAICE_ALLOW_EVP */
# endif /* SEAICE_ALLOW_DYNAMICS */
         enddo
        enddo
       enddo
      enddo
#endif /* ALLOW_SEAICE */

#ifndef DISABLE_DEBUGMODE
      IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_LEAVE('AUTODIFF_RESTORE',myThid)
#endif

#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

      return
      end

