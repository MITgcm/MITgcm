#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE LONGSTEP_INIT_VARIA( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_INIT_VARIA
C     | o Initialize LONGSTEP data structures
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "LONGSTEP.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid     :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_LONGSTEP
C     !LOCAL VARIABLES:
C     == Local variables ==
CEOP

C     Initialise LS_doTimeStep
      _BARRIER
      _BEGIN_MASTER(myThid)
      LS_doTimeStep = .FALSE.
      _END_MASTER(myThid)
      _BARRIER

C     initialize longstep averages to zero

      CALL LONGSTEP_RESET_3D(LS_uVelCount, LS_uVel, Nr, myThid)
      CALL LONGSTEP_RESET_3D(LS_vVelCount, LS_vVel, Nr, myThid)
      CALL LONGSTEP_RESET_3D(LS_wVelCount, LS_wVel, Nr, myThid)
      CALL LONGSTEP_RESET_3D(LS_thetaCount,LS_theta,Nr, myThid)
      CALL LONGSTEP_RESET_3D(LS_saltCount, LS_salt, Nr, myThid)
      IF ( ivdc_kappa .NE. 0. _d 0 )
     & CALL LONGSTEP_RESET_3D(LS_IVDConvCountCount,
     &                       LS_IVDConvCount, Nr, myThid)
#ifdef SHORTWAVE_HEATING
      CALL LONGSTEP_RESET_3D(LS_QswCount, LS_Qsw, 1, myThid)
#endif
      CALL LONGSTEP_RESET_3D(LS_fwFluxCount,LS_fwFlux,1,myThid)
#ifdef ALLOW_GMREDI
      IF ( useGMRedi ) THEN
       CALL LONGSTEP_RESET_3D(LS_KwxCount, LS_Kwx, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_KwyCount, LS_Kwy, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_KwzCount, LS_Kwz, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_KuxCount, LS_Kux, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_KvyCount, LS_Kvy, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_KuzCount, LS_Kuz, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_KvzCount, LS_Kvz, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_PsiXCount, LS_PsiX, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_PsiYCount, LS_PsiY, Nr, myThid)
      ENDIF
#endif
#ifdef ALLOW_KPP
      IF ( useKPP ) THEN
       CALL LONGSTEP_RESET_3D(LS_KPPdiffKzSCount,
     &                        LS_KPPdiffKzS, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_KPPghatCount,
     &                        LS_KPPghat, Nr, myThid)
      ENDIF
#endif

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
