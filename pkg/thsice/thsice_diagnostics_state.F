C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_diagnostics_state.F,v 1.2 2007/08/27 13:23:13 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_DIAGNOSTICS_STATE
C     !INTERFACE:
      SUBROUTINE THSICE_DIAGNOSTICS_STATE(
     I                      myTime, myIter, bi, bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_DIAGNOSTICS_STATE
C     | o fill-in diagnostics array for THSICE state variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     bi,bj   :: tile indices
C     myThid  :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     == Local variables ==
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpFac
      INTEGER i,j
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

      IF ( useDiagnostics ) THEN

        CALL DIAGNOSTICS_FILL(iceMask,'SI_Fract',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(snowAge,'SIsnwAge',0,1,1,bi,bj,myThid)

C-     Ice-fraction weighted quantities:
        tmpFac = 1. _d 0
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   iceHeight, iceMask,tmpFac,1,'SI_Thick',
     I                   0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   snowHeight,iceMask,tmpFac,1,'SI_SnowH',
     I                   0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   Tsrf,      iceMask,tmpFac,1,'SI_Tsrf ',
     I                   0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   Tice1,     iceMask,tmpFac,1,'SI_Tice1',
     I                   0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   Tice2,     iceMask,tmpFac,1,'SI_Tice2',
     I                   0,1,1,bi,bj,myThid)

C-     Ice-Volume weighted quantities:
       IF ( DIAGNOSTICS_IS_ON('SI_Qice1',myThid) .OR.
     &      DIAGNOSTICS_IS_ON('SI_Qice2',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*iceHeight(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   Qice1(1-OLx,1-OLy,bi,bj),
     I                   tmpFld,tmpFac,1,'SI_Qice1',
     I                   0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   Qice2(1-OLx,1-OLy,bi,bj),
     I                   tmpFld,tmpFac,1,'SI_Qice2',
     I                   0,1,2,bi,bj,myThid)
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Ocean Mixed-Layer temp. & salinity
       IF ( stepFwd_oceMxL ) THEN
        CALL DIAGNOSTICS_FILL(tOceMxL,'SItOcMxL',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(sOceMxL,'SIsOcMxL',0,1,1,bi,bj,myThid)
       ENDIF

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
