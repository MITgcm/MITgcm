#include "ECCO_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_COST_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE ECCO_COST_INIT_VARIA( myThid )

C     !DESCRIPTION:
c     o Initialise the variable cost function part.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_COST
# include "cost.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi, bj
      INTEGER i, j, k
C     LOGICAL exst
CEOP

C--   Initialise adjoint of monthly mean files calculated
C--   in cost_averagesfields (and their ad...).
      CALL COST_AVERAGESINIT( myThid )
      _BARRIER

#ifndef ALLOW_TANGENTLINEAR_RUN
cph(
cph   The following init. shoud not be applied if in the middle
cph   of a divided adjoint run
cph)
c      INQUIRE( FILE='costfinal', EXIST=exst )
c      IF ( .NOT. exst) THEN
c        CALL ECCO_COST_INIT_BARFILES( myThid )
c      ENDIF
#endif

C--   Initialize the tiled cost function contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_GENCOST_CONTRIBUTION
          DO k=1,NGENCOST
            objf_gencost(bi,bj,k) = 0. _d 0
            num_gencost(bi,bj,k)  = 0. _d 0
          ENDDO
#endif

#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
          objf_obcsn(bi,bj) = 0. _d 0
          objf_obcss(bi,bj) = 0. _d 0
          objf_obcsw(bi,bj) = 0. _d 0
          objf_obcse(bi,bj) = 0. _d 0
          objf_ageos(bi,bj) = 0. _d 0
          num_obcsn(bi,bj)  = 0. _d 0
          num_obcss(bi,bj)  = 0. _d 0
          num_obcsw(bi,bj)  = 0. _d 0
          num_obcse(bi,bj)  = 0. _d 0
          num_ageos(bi,bj)  = 0. _d 0
#endif

        ENDDO
      ENDDO

      _BEGIN_MASTER( myThid )
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          frame(i,j) = 1. _d 0
        ENDDO
      ENDDO
      _END_MASTER( myThid )

C--   Initialise the "global" parts of the cost function.
#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
      _BEGIN_MASTER( myThid )
        objf_obcsvol = 0. _d 0
        num_obcsvol = 0. _d 0
      _END_MASTER( myThid )
#endif

      _BARRIER

      RETURN
      END
