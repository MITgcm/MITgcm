C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/Attic/calc_gtr1.F,v 1.1 2001/07/13 19:06:50 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE CALC_GTR1( 
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           KappaRT,
     U           fVerT,
     I           myCurrentTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_GTR1                                     |
C     | o Calculate the passive tracer tendency terms.           |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"
#include "TR1.h"

C     == Routine arguments ==
C     fVerT   - Flux of temperature (T) in the vertical 
C               direction at the upper(U) and lower(D) faces of a cell.
C     maskUp  - Land mask used to denote base of the domain.
C     xA      - Tracer cell face area normal to X
C     yA      - Tracer cell face area normal to X
C     uTrans  - Zonal volume transport through cell face
C     vTrans  - Meridional volume transport through cell face
C     rTrans  - Vertical volume transport through cell face
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C                                      results will be set.
C     myThid - Instance number for this innvocation of CALC_GT
      _RL fVerT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER k,kUp,kDown,kM1
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
      _RL     myCurrentTime

C     == Local variables ==

#ifdef ALLOW_AUTODIFF_TAMC
C--   only the kUp part of fverT is set in this subroutine
C--   the kDown is still required
      fVerT(1,1,kDown) = fVerT(1,1,kDown)
#endif

      CALL GAD_CALC_RHS(
     I           bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I           xA,yA,uTrans,vTrans,rTrans,maskUp,
     I           diffKhT, diffK4T, KappaRT, tr1,
     I           GAD_TR1,
     U           fVerT, gTr1,
     I           myThid )

#ifdef INCLUDE_TR_FORCING_CODE
C--   External thermal forcing term(s)
      CALL EXTERNAL_FORCING_TR(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myCurrentTime,myThid)
#endif /*  INCLUDE_TR_FORCING_CODE */

      RETURN
      END
