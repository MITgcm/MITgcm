C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_initialise.F,v 1.16 2014/01/06 14:53:34 mlosch Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_INITIALISE
C     !INTERFACE:
      SUBROUTINE AIM_INITIALISE( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================*
C     | S/R AIM_INITIALISE
C     *==================================================================*
C     | Initialisation of AIM atmospheric physics package :
C     | 1) call iniphys (=> set parameters to default value)
C     | 2) read AIM parameters
C     *==================================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "AIM_PARAMS.h"
#include "AIM_FFIELDS.h"
c #include "AIM_GRID.h"
c #include "AIM_DIAGS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id. number
      INTEGER myThid

#ifdef ALLOW_AIM
C     !LOCAL VARIABLES:
C     == Local variables ==
C     HSG     :: Cell face in vertical
C     pGround :: Lower boundary pressure
C     bi,bj   :: Tile indices
C     i, j, k :: Loop counters
      _RL HSG(0:Nr)
      _RL pGround, tmpPgrnd, tmpVar
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER Katm
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid )

C--  Set default value for atmos. physics parameters:
      pGround = atm_Po
      DO k=1,Nr
       Katm = _KD2KA( k )
       HSG(Katm) = rF(k)/pGround
      ENDDO
       k=Nr+1
       Katm = _KD2KA( k )
       HSG(Katm) = rF(k)/pGround

      _END_MASTER( myThid )

C--   set default value for all atmos. physics parameter:
      CALL INPHYS( HSG, myThid )

C--   Read AIM parameters (from file data.aimphys):
      CALL AIM_READPARMS( myThid )

C--   set energy fractions in LW bands as a function of temperature:
C     initialize common block RADFIX (originally called from FORDATE in SPEEDY)
      _BEGIN_MASTER(myThid )
       CALL RADSET( myThid )
      _END_MASTER( myThid )

C--   Set truncSurfP : used to correct for truncation (because of hFacMin)
C      of surface reference pressure Ro_surf that affects Surf.Temp.
      CALL INI_P_GROUND(1, topoZ, truncSurfP, myThid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          tmpPgrnd = truncSurfP(i,j,bi,bj)
          tmpVar   = Ro_surf(i,j,bi,bj)
          tmpPgrnd = MIN( tmpPgrnd, atm_Po )
          truncSurfP(i,j,bi,bj) = ( tmpVar/tmpPgrnd )**atm_kappa
         ENDDO
        ENDDO
        IF (aim_useMMsurfFc .AND. aim_surfPotTemp) THEN
         DO j=1,sNy
          DO i=1,sNx
           tmpVar   = Ro_surf(i,j,bi,bj)
           truncSurfP(i,j,bi,bj) = ( tmpVar/atm_Po )**atm_kappa
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

C--   Initialise Land Fraction (in AIM_FFIELDS.h):
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          aim_landFr   (i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( aim_LandFile .NE. ' '  ) THEN
         _BARRIER
         CALL READ_REC_XY_RS(aim_LandFile,aim_landFr,1,nIter0,myThid)
C-    better to fill land fraction overlap (likely to be needed for sea-ice)
         CALL EXCH_XY_RS( aim_landFr, myThid )
      ENDIF

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        CALL AIM_MNC_INIT( myThid )
      ENDIF
#endif /*  ALLOW_MNC  */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL AIM_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

C--   Everyone else must wait for the parameters to be set
      _BARRIER

#endif /* ALLOW_AIM */

      RETURN
      END
