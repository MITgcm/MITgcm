C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_restore.F,v 1.29 2014/10/09 00:47:12 gforget Exp $
C $Name:  $

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

      subroutine autodiff_restore( myThid )

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
c These includes are needed for
c AD-checkpointing.
c They provide the fields to be stored.

# include "GRID.h"
# include "DYNVARS.h"
# include "FFIELDS.h"
# include "SURFACE.h"
# include "AUTODIFF.h"

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

c     == routine arguments ==
c     note: under the multi-threaded model myIter and
c           myTime are local variables passed around as routine
c           arguments. Although this is fiddly it saves the need to
c           impose additional synchronisation points when they are
c           updated.
c     myThid - thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_AUTODIFF_TAMC
c     == local variables ==

      integer bi,bj
      integer I,J,K

c--   == end of interface ==

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_ENTER('AUTODIFF_RESTORE',myThid)
#endif

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifndef AUTODIFF_USE_OLDSTORE_2D
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          etaN(I,J,bi,bj)               = StoreDynVars2D(I,J,bi,bj,1)
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
#endif /* AUTODIFF_USE_OLDSTORE_2D */

#ifndef AUTODIFF_USE_OLDSTORE_3D
C-      3D arrays
        DO K=1,Nr
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
#ifdef ALLOW_ADAMSBASHFORTH_3
           gtNm(I,J,K,bi,bj,1)   = StoreDynVars3D(I,J,K,bi,bj,1)
           gsNm(I,J,K,bi,bj,1)   = StoreDynVars3D(I,J,K,bi,bj,2)
           guNm(I,J,K,bi,bj,1)   = StoreDynVars3D(I,J,K,bi,bj,3)
           gvNm(I,J,K,bi,bj,1)   = StoreDynVars3D(I,J,K,bi,bj,4)
#else
           gtNm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,1)
           gsNm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,2)
           guNm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,3)
           gvNm1(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,4)
#endif
           theta(I,J,K,bi,bj)    = StoreDynVars3D(I,J,K,bi,bj,5)
           salt(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,6)
           uVel(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,7)
           vVel(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,8)
           wVel(I,J,K,bi,bj)     = StoreDynVars3D(I,J,K,bi,bj,9)
           totPhiHyd(I,J,K,bi,bj)= StoreDynVars3D(I,J,K,bi,bj,10)
#ifdef ALLOW_ADAMSBASHFORTH_3
           gtNm(I,J,K,bi,bj,2)   = StoreDynVars3D(I,J,K,bi,bj,11)
           gsNm(I,J,K,bi,bj,2)   = StoreDynVars3D(I,J,K,bi,bj,12)
           guNm(I,J,K,bi,bj,2)   = StoreDynVars3D(I,J,K,bi,bj,13)
           gvNm(I,J,K,bi,bj,2)   = StoreDynVars3D(I,J,K,bi,bj,14)
#endif
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
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
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
         ENDDO
        ENDDO
       ENDDO
      ENDDO

# ifdef ALLOW_BULKFORMULAE
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
#  ifdef ALLOW_ATM_TEMP
          aqh0(I,J,bi,bj)            = StoreEXF2(I,J,bi,bj,1)
          aqh1(I,J,bi,bj)            = StoreEXF2(I,J,bi,bj,2)
          atemp0(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,3)
          atemp1(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,4)
          precip0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,5)
          precip1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,6)
          lwflux0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,7)
          lwflux1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,8)
          snowprecip0(I,J,bi,bj)     = StoreEXF2(I,J,bi,bj,9)
          snowprecip1(I,J,bi,bj)     = StoreEXF2(I,J,bi,bj,10)
#   ifdef EXF_READ_EVAP
          evap0(I,J,bi,bj)           = StoreEXF2(I,J,bi,bj,11)
          evap1(I,J,bi,bj)           = StoreEXF2(I,J,bi,bj,12)
#   else
          evap(I,J,bi,bj)            = StoreEXF2(I,J,bi,bj,11)
#   endif /* EXF_READ_EVAP */
#   ifdef ALLOW_DOWNWARD_RADIATION
          swdown0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,13)
          swdown1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,14)
          lwdown0(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,15)
          lwdown1(I,J,bi,bj)         = StoreEXF2(I,J,bi,bj,16)
#   endif
#  endif /* ALLOW_ATM_TEMP */
          uwind0(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,17)
          uwind1(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,18)
          vwind0(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,19)
          vwind1(I,J,bi,bj)          = StoreEXF2(I,J,bi,bj,20)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif /* ALLOW_ATM_TEMP */

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
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
# ifdef ALLOW_SNOWPRECIP_CONTROL
          xx_snowprecip0(I,J,bi,bj)= StoreCTRLS1(I,J,bi,bj,11)
          xx_snowprecip1(I,J,bi,bj)= StoreCTRLS1(I,J,bi,bj,12)
#  endif
# ifdef ALLOW_SWFLUX_CONTROL
          xx_swflux0(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,13)
          xx_swflux1(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,14)
#  endif
# ifdef ALLOW_SWDOWN_CONTROL
          xx_swdown0(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,15)
          xx_swdown1(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,16)
#  endif
#  ifdef ALLOW_LWDOWN_CONTROL
          xx_lwdown0(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,17)
          xx_lwdown1(I,J,bi,bj)    = StoreCTRLS1(I,J,bi,bj,18)
#  endif
#  ifdef ALLOW_APRESSURE_CONTROL
          xx_apressure0(I,J,bi,bj) = StoreCTRLS1(I,J,bi,bj,19)
          xx_apressure1(I,J,bi,bj) = StoreCTRLS1(I,J,bi,bj,20)
#  endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_EXF */

#ifdef ALLOW_OBCS
# ifdef ALLOW_OBCS_NORTH
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO K=1,Nr
         DO I=1-OLx,sNx+OLx
          OBNu(I,K,bi,bj)    = StoreOBCSN(I,K,bi,bj,1)
          OBNv(I,K,bi,bj)    = StoreOBCSN(I,K,bi,bj,2)
          OBNt(I,K,bi,bj)    = StoreOBCSN(I,K,bi,bj,3)
          OBNs(I,K,bi,bj)    = StoreOBCSN(I,K,bi,bj,4)
          OBNu0(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,5)
          OBNv0(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,6)
          OBNt0(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,7)
          OBNs0(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,8)
          OBNu1(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,9)
          OBNv1(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,10)
          OBNt1(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,11)
          OBNs1(I,K,bi,bj)   = StoreOBCSN(I,K,bi,bj,12)
#  ifdef ALLOW_OBCSN_CONTROL
          xx_obcsn0(I,K,bi,bj,1)   = StoreOBCSN(I,K,bi,bj,13)
          xx_obcsn0(I,K,bi,bj,2)   = StoreOBCSN(I,K,bi,bj,14)
          xx_obcsn0(I,K,bi,bj,3)   = StoreOBCSN(I,K,bi,bj,15)
          xx_obcsn0(I,K,bi,bj,4)   = StoreOBCSN(I,K,bi,bj,16)
          xx_obcsn1(I,K,bi,bj,1)   = StoreOBCSN(I,K,bi,bj,17)
          xx_obcsn1(I,K,bi,bj,2)   = StoreOBCSN(I,K,bi,bj,18)
          xx_obcsn1(I,K,bi,bj,3)   = StoreOBCSN(I,K,bi,bj,19)
          xx_obcsn1(I,K,bi,bj,4)   = StoreOBCSN(I,K,bi,bj,20)
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
          OBSu(I,K,bi,bj)       = StoreOBCSS(I,K,bi,bj,1)
          OBSv(I,K,bi,bj)       = StoreOBCSS(I,K,bi,bj,2)
          OBSt(I,K,bi,bj)       = StoreOBCSS(I,K,bi,bj,3)
          OBSs(I,K,bi,bj)       = StoreOBCSS(I,K,bi,bj,4)
          OBSu0(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,5)
          OBSv0(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,6)
          OBSt0(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,7)
          OBSs0(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,8)
          OBSu1(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,9)
          OBSv1(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,10)
          OBSt1(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,11)
          OBSs1(I,K,bi,bj)      = StoreOBCSS(I,K,bi,bj,12)
#  ifdef ALLOW_OBCSS_CONTROL
          xx_obcss0(I,K,bi,bj,1)   = StoreOBCSS(I,K,bi,bj,13)
          xx_obcss0(I,K,bi,bj,2)   = StoreOBCSS(I,K,bi,bj,14)
          xx_obcss0(I,K,bi,bj,3)   = StoreOBCSS(I,K,bi,bj,15)
          xx_obcss0(I,K,bi,bj,4)   = StoreOBCSS(I,K,bi,bj,16)
          xx_obcss1(I,K,bi,bj,1)   = StoreOBCSS(I,K,bi,bj,17)
          xx_obcss1(I,K,bi,bj,2)   = StoreOBCSS(I,K,bi,bj,18)
          xx_obcss1(I,K,bi,bj,3)   = StoreOBCSS(I,K,bi,bj,19)
          xx_obcss1(I,K,bi,bj,4)   = StoreOBCSS(I,K,bi,bj,20)
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
          OBEu(J,K,bi,bj)      = StoreOBCSE(J,K,bi,bj,1)
          OBEv(J,K,bi,bj)      = StoreOBCSE(J,K,bi,bj,2)
          OBEt(J,K,bi,bj)      = StoreOBCSE(J,K,bi,bj,3)
          OBEs(J,K,bi,bj)      = StoreOBCSE(J,K,bi,bj,4)
          OBEu0(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,5)
          OBEv0(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,6)
          OBEt0(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,7)
          OBEs0(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,8)
          OBEu1(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,9)
          OBEv1(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,10)
          OBEt1(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,11)
          OBEs1(J,K,bi,bj)     = StoreOBCSE(J,K,bi,bj,12)
#  ifdef ALLOW_OBCSE_CONTROL
          xx_obcse0(J,K,bi,bj,1)     = StoreOBCSE(J,K,bi,bj,13)
          xx_obcse0(J,K,bi,bj,2)     = StoreOBCSE(J,K,bi,bj,14)
          xx_obcse0(J,K,bi,bj,3)     = StoreOBCSE(J,K,bi,bj,15)
          xx_obcse0(J,K,bi,bj,4)     = StoreOBCSE(J,K,bi,bj,16)
          xx_obcse1(J,K,bi,bj,1)     = StoreOBCSE(J,K,bi,bj,17)
          xx_obcse1(J,K,bi,bj,2)     = StoreOBCSE(J,K,bi,bj,18)
          xx_obcse1(J,K,bi,bj,3)     = StoreOBCSE(J,K,bi,bj,19)
          xx_obcse1(J,K,bi,bj,4)     = StoreOBCSE(J,K,bi,bj,20)
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
          OBWu(J,K,bi,bj)      = StoreOBCSW(J,K,bi,bj,1)
          OBWv(J,K,bi,bj)      = StoreOBCSW(J,K,bi,bj,2)
          OBWt(J,K,bi,bj)      = StoreOBCSW(J,K,bi,bj,3)
          OBWs(J,K,bi,bj)      = StoreOBCSW(J,K,bi,bj,4)
          OBWu0(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,5)
          OBWv0(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,6)
          OBWt0(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,7)
          OBWs0(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,8)
          OBWu1(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,9)
          OBWv1(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,10)
          OBWt1(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,11)
          OBWs1(J,K,bi,bj)     = StoreOBCSW(J,K,bi,bj,12)
#  ifdef ALLOW_OBCSW_CONTROL
          xx_obcsw0(J,K,bi,bj,1) = StoreOBCSW(J,K,bi,bj,13)
          xx_obcsw0(J,K,bi,bj,2) = StoreOBCSW(J,K,bi,bj,14)
          xx_obcsw0(J,K,bi,bj,3) = StoreOBCSW(J,K,bi,bj,15)
          xx_obcsw0(J,K,bi,bj,4) = StoreOBCSW(J,K,bi,bj,16)
          xx_obcsw1(J,K,bi,bj,1) = StoreOBCSW(J,K,bi,bj,17)
          xx_obcsw1(J,K,bi,bj,2) = StoreOBCSW(J,K,bi,bj,18)
          xx_obcsw1(J,K,bi,bj,3) = StoreOBCSW(J,K,bi,bj,19)
          xx_obcsw1(J,K,bi,bj,4) = StoreOBCSW(J,K,bi,bj,20)
#  endif
          ENDDO
        ENDDO
       ENDDO
      ENDDO
# endif /* ALLOW_OBCS_WEST */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          AREA(I,J,bi,bj)   = StoreSEAICE(I,J,bi,bj,1)
          HEFF(I,J,bi,bj)   = StoreSEAICE(I,J,bi,bj,2)
          HSNOW(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,3)
CML          TICE(I,J,bi,bj)     = StoreSEAICE(I,J,bi,bj,4)
          RUNOFF(I,J,bi,bj)   = StoreSEAICE(I,J,bi,bj,5)
# ifdef SEAICE_CGRID
          stressDivergenceX(I,J,bi,bj) = StoreSEAICE(I,J,bi,bj,14)
          stressDivergenceY(I,J,bi,bj) = StoreSEAICE(I,J,bi,bj,15)
# endif
# ifdef SEAICE_MULTICATEGORY
cph moved to seaice_ad_check_lev because of extra dim. MULTDIM
cph          TICES(I,J,bi,bj)     = StoreSEAICE(I,J,bi,bj,6)
# endif
          UICE(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,7)
          VICE(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,8)
          ZETA(I,J,bi,bj)      = StoreSEAICE(I,J,bi,bj,9)
          ETA(I,J,bi,bj)       = StoreSEAICE(I,J,bi,bj,10)
#  ifdef SEAICE_CGRID
          dwatn(I,J,bi,bj)          = StoreSEAICE(I,J,bi,bj,11)
CML          seaicemasku(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,12)
CML          seaicemaskv(I,J,bi,bj)    = StoreSEAICE(I,J,bi,bj,13)
#  endif /* SEAICE_CGRID */
#  ifdef SEAICE_ALLOW_EVP
          seaice_sigma1(I,J,bi,bj)  = StoreSEAICE(I,J,bi,bj,16)
          seaice_sigma2(I,J,bi,bj)  = StoreSEAICE(I,J,bi,bj,17)
          seaice_sigma12(I,J,bi,bj) = StoreSEAICE(I,J,bi,bj,18)
#  endif /* SEAICE_ALLOW_EVP */
# ifdef SEAICE_VARIABLE_SALINITY
          HSALT(I,J,bi,bj)          = StoreSEAICE(I,J,bi,bj,19)
# endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_DEBUG
      IF ( debugMode ) CALL DEBUG_LEAVE('AUTODIFF_RESTORE',myThid)
#endif

#endif /* ALLOW_AUTODIFF_TAMC */
c**************************************

      return
      end
