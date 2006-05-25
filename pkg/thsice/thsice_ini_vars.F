C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_ini_vars.F,v 1.6 2006/05/25 18:03:24 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

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
#include "GRID.h"
c #include "DYNVARS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_RBCS
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     == Local variables ==
C     bi,bj  :: Loop counters
C     i,j    :: Loop counters
      INTEGER bi, bj
      INTEGER I, J
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
#ifdef COUPLE_MODEL
            dFdT(i,j,bi,bj)     = 0. _d 0
#endif
            icFlxSW (i,j,bi,bj) = 0. _d 0
            icFlxAtm(i,j,bi,bj) = 0. _d 0
            icFrwAtm(i,j,bi,bj) = 0. _d 0
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
        IF ( thSIceFract_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceFract_InitFile,iceMask,1,0,myThid)
        ENDIF
        IF ( thSIceThick_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceThick_InitFile,iceHeight,1,0,myThid)
        ENDIF
        IF ( thSIceSnowH_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceSnowH_InitFile,snowHeight,1,0,myThid)
        ENDIF
        IF ( thSIceSnowA_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceSnowA_InitFile,snowAge,1,0,myThid)
        ENDIF
        IF ( thSIceEnthp_InitFile .NE. ' ' ) THEN
         CALL READ_REC_XY_RL(thSIceEnthp_InitFile,Qice1,1,0,myThid)
         CALL READ_REC_XY_RL(thSIceEnthp_InitFile,Qice2,2,0,myThid)
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
#ifdef COUPLE_MODEL
        _EXCH_XY_R8(dFdT, myThid)
#endif

#endif /* ALLOW_THSICE */

      RETURN
      END
