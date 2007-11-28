C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_frac.F,v 1.2 2007/11/28 17:55:36 dimitri Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_FRAC
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_FRAC(
     I                  imax, fact,SPDepth,
     U                  plumek,
     I                  myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SALT_PLUME_FRAC
C     | o Compute saltplume penetration.
C     *==========================================================*
C     | Compute fraction of saltplume (flux) penetrating to
C     | specified depth, plumek, due to rejected salt
C     | during freezing.
C     | For example, if surface value is Saltplume0,
C     | and each level gets equal fraction 1/5 down to SPDepth=5,
C     | SALT_PLUME_FRAC will report plumek = 4/5 on output if the input
C     | plumek = 1. Else, output plumek = 0.                
C     | Reference : Duffy et al, (GRL 1999)
C     |
C     | =====
C     | Written by   : ATN (based on SWFRAC)
C     | Date         : Sep 13, 2007
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     input arguments
C     imax    :: number of vertical grid points
C     fact    :: scale  factor to apply to depth array
C     SPDpeth :: corresponding SaltPlumeDepth(i,j) at this grid point
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id. number
      INTEGER imax
      _RL     fact
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     input/output arguments
C     plumek :: on input: vertical depth for desired plume fraction
C               (fact*plumek) is negative distance (m) from surface
C     plumek :: on output: saltplume contribution fraction
      _RL     plumek(imax), SPDepth(imax)
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
      _RL facz
      INTEGER i
      _RL        one          , tinee
      parameter( one = 1. _d 0, tinee = 1.e-1)

      DO i = 1,imax
         facz = abs(fact*plumek(i))
         IF (SPDepth(i).GT.facz .AND. SPDepth(i).GT.tinee) THEN
           plumek(i) = one - min(one,facz/abs(SPDepth(i)))
         ELSE
           plumek(i) = 0.
         ENDIF
      ENDDO

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
