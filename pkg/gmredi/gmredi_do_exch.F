C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_do_exch.F,v 1.3 2014/12/05 17:39:39 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_DO_EXCH
C     !INTERFACE:
      SUBROUTINE GMREDI_DO_EXCH(
     I                           myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_DO_EXCH
C     | o Apply Exchanges to GM-Redi variables when necessary
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

#ifdef GM_BOLUS_ADVEC
#ifdef ALLOW_EDDYPSI
      IF ( GM_InMomAsStress ) THEN
#else
C     For multi-dim advection, need valid bolus velocity in halo regions
C     near CS-corners: Because of sigmaR averaging @ U & V points, that
C     would require calling FILL_CS_CORNER_TR before each averaging (similar
C     to what is done in grad_sigma.F), near CS-corner values are not right.
C     This exchange fixes this Pb. Fix also a similar Pb with VisbeckK or
C     with GM-bolus 2-D mapping factor (GM_bolFac2d).
      IF ( useCubedSphereExchange
     &     .AND. GM_AdvForm
     &     .AND. .NOT.GM_AdvSeparate
     &     .AND. useMultiDimAdvec ) THEN
#endif
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('EXCH_UV(GM_PsiX,Y)',myThid)
#endif
        CALL EXCH_UV_XYZ_RL( GM_PsiX, GM_PsiY, .TRUE., myThid )
      ENDIF
#endif /* GM_BOLUS_ADVEC */

      RETURN
      END
