C $Header: /u/gcmpack/MITgcm/model/src/reset_nlfs_vars.F,v 1.2 2014/05/02 16:56:53 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: RESET_NLFS_VARS
C     !INTERFACE:
      SUBROUTINE RESET_NLFS_VARS( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE RESET_NLFS_VARS
C     | o Re-set some Non-Linear Free-Surface variables
C     |   in order to facilitate the AD tool task of solving
C     |   dependency rules.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C     Local variables
C     i,j,k,bi,bj :: loop counter
      INTEGER i,j,bi,bj
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

#ifndef DISABLE_RSTAR_CODE
        IF ( fluidIsAir .AND. select_rStar.GE.1 ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           pStarFacK(i,j,bi,bj) = rStarFacC(i,j,bi,bj)**atm_kappa
          ENDDO
         ENDDO
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           pStarFacK(i,j,bi,bj) = 1. _d 0
          ENDDO
         ENDDO
        ENDIF
#endif /* DISABLE_RSTAR_CODE */

C- end bi,bj loop
       ENDDO
      ENDDO

#endif /* NONLIN_FRSURF */

      RETURN
      END
