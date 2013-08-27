C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_init_varia.F,v 1.3 2013/08/27 21:34:21 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

CBOP
C     !ROUTINE: BBL_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE BBL_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize BBL variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "BBL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid      :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_BBL

C     !LOCAL VARIABLES:
C     i,j,bi,bj :: loop indices
C     kBot      :: k index of bottommost wet grid
      INTEGER i,j,bi,bj,kBot

C     Initialize arrays in common blocks.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          kBot = max(1,kLowC(i,j,bi,bj))
          bbl_theta(i,j,bi,bj)     = theta(i,j,kBot,bi,bj)
          bbl_salt (i,j,bi,bj)     = salt (i,j,kBot,bi,bj)
          bbl_eta(i,j,bi,bj)       = 0. _d 0
          bbl_TendTheta(i,j,bi,bj) = 0. _d 0
          bbl_TendSalt (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0
     &                           .AND. pickupSuff.EQ.' ' ) THEN

C     Read initialization files, if available.
       IF ( bbl_thetaFile .NE. ' ' ) THEN
         CALL READ_FLD_XY_RL( bbl_thetaFile, ' ',
     &                        bbl_theta, 0, myThid )
         CALL EXCH_XY_RL( bbl_theta, myThid )
       ENDIF
       IF ( bbl_saltFile .NE. ' ' ) THEN
         CALL READ_FLD_XY_RL( bbl_saltFile, ' ',
     &                        bbl_salt, 0, myThid )
         CALL EXCH_XY_RL( bbl_salt, myThid )
       ENDIF
       IF ( bbl_etaFile .NE. ' ' ) THEN
         CALL READ_FLD_XY_RL( bbl_etaFile, ' ',
     &                        bbl_eta, 0, myThid )
         CALL EXCH_XY_RL( bbl_eta, myThid )
       ENDIF

      ELSE

C     Restart from a pickup.
         CALL BBL_READ_PICKUP( nIter0, myThid )

C     End start-from-iter-zero if/else block.
      ENDIF

#endif /* ALLOW_BBL */

      RETURN
      END
