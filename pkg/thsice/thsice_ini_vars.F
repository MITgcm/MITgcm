C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_ini_vars.F,v 1.13 2008/05/22 15:54:37 dimitri Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AIM
# include "AIM_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_INI_VARS
C     !INTERFACE:
      SUBROUTINE THSICE_INI_VARS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_INI_VARS
C     | o initialize THermo_SeaICE variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_AIM
# include "AIM_FFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     == Local variables ==
C     bi,bj  :: Loop counters
C     i,j    :: Loop counters
      INTEGER bi, bj
      INTEGER i, j
      INTEGER prec
c     CHARACTER*(MAX_LEN_FNAM) fn
      _RL v2Loc


c     set up ice arrays to zero if starting ice
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
C-        state variables :
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            iceMask(i,j,bi,bj)  = 0. _d 0
            iceHeight(i,j,bi,bj)= 0. _d 0
            snowHeight(i,j,bi,bj)=0. _d 0
            Tsrf(i,j,bi,bj)     = 0. _d 0
            Tice1(i,j,bi,bj)    = 0. _d 0
            Tice2(i,j,bi,bj)    = 0. _d 0
            Qice1(i,j,bi,bj)    = 0. _d 0
            Qice2(i,j,bi,bj)    = 0. _d 0
            snowAge(i,j,bi,bj)  = 0. _d 0
           ENDDO
          ENDDO
C-        fluxes :
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            sHeating(i,j,bi,bj) = 0. _d 0
            flxCndBt(i,j,bi,bj) = 0. _d 0
            snowPrc(i,j,bi,bj)  = 0. _d 0
            siceAlb(i,j,bi,bj)  = 0. _d 0
            icFlxSW (i,j,bi,bj) = 0. _d 0
            icFlxAtm(i,j,bi,bj) = 0. _d 0
            icFrwAtm(i,j,bi,bj) = 0. _d 0
C-        needed when using advection/diffusion:
            oceFWfx(i,j,bi,bj)  = 0. _d 0
            oceSflx(i,j,bi,bj)  = 0. _d 0
            oceQnet(i,j,bi,bj)  = 0. _d 0
           ENDDO
          ENDDO
C-        oceanic mixed layer state :
          v2Loc = vMxL_default*vMxL_default
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             hOceMxL(i,j,bi,bj) = hMxL_default
             tOceMxL(i,j,bi,bj) = 0. _d 0
             sOceMxL(i,j,bi,bj) = sMxL_default
             v2ocMxL(i,j,bi,bj) = v2Loc
           ENDDO
          ENDDO
#ifdef ALLOW_AIM
          IF ( useAIM ) THEN
C-        Mask mixed layer depth : depth is used in thsice slab_ocean
C         and this mask is used in thsice_advdiff and if coupled
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( aim_landFr(i,j,bi,bj).EQ.1. _d 0 )
     &       hOceMxL(i,j,bi,bj) = 0.
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_AIM */
        ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( startIceModel.LE.0 .AND. nIter0.NE.0 ) THEN
C--     Read ice pickup fields
       _BARRIER
        prec=precFloat64
        CALL THSICE_READ_PICKUP( prec, nIter0, myThid )

      ELSE
C--     Read initial conditions:
        _BEGIN_MASTER( myThid )
        IF ( thSIceThick_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceThick_InitFile,iceHeight,1,0,myThid)
        ENDIF
        IF ( thSIceFract_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceFract_InitFile,iceMask,1,0,myThid)
        ELSE
C     default to 100% ice concentration when thickness is non-zero
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF (iceHeight(i,j,bi,bj) .NE. 0. _d 0)
     &            iceMask(i,j,bi,bj) = 1. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        IF ( thSIceSnowH_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceSnowH_InitFile,snowHeight,1,0,myThid)
        ELSE
C     default to <= 20 cm snow thickness where there is ice
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             snowHeight(i,j,bi,bj) = 0.2 _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        IF ( thSIceSnowA_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceSnowA_InitFile,snowAge,1,0,myThid)
        ELSE
C     default to 3 days for snow age
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF (snowHeight(i,j,bi,bj) .NE. 0. _d 0)
     &            snowAge(i,j,bi,bj) = 259200. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        IF ( thSIceEnthp_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceEnthp_InitFile,Qice1,1,0,myThid)
         CALL READ_REC_XY_RL(thSIceEnthp_InitFile,Qice2,2,0,myThid)
        ELSE
C     default to 3.4e5 J/kg for enthalpy where there is sea ice
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF (iceMask(i,j,bi,bj) .NE. 0. _d 0) THEN
              Qice1(i,j,bi,bj) = 3.4 _d 5
              Qice2(i,j,bi,bj) = 3.4 _d 5
             ENDIF
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        IF ( thSIceTsurf_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RS(thSIceTsurf_InitFile,Tsrf,1,0,myThid)
        ENDIF
        _END_MASTER(myThid)
      ENDIF

        _EXCH_XY_R8(iceMask,myThid)
        _EXCH_XY_R8(iceHeight, myThid)
        _EXCH_XY_R8(snowHeight,myThid)
        _EXCH_XY_R8(Tsrf,   myThid)
        _EXCH_XY_R8(Tice1,  myThid)
        _EXCH_XY_R8(Tice2,  myThid)
        _EXCH_XY_R8(Qice1,  myThid)
        _EXCH_XY_R8(Qice2,  myThid)
        _EXCH_XY_R8(snowAge,myThid)

C--     Initialise Sea-Ice Loading for SeaIce-Dynamics :
        IF ( useSEAICE ) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               sIceLoad(i,j,bi,bj) = ( snowHeight(i,j,bi,bj)*rhos
     &                               + iceHeight(i,j,bi,bj)*rhoi
     &                               )*iceMask(i,j,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
        ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
