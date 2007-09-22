C $Header: /u/gcmpack/MITgcm/model/src/Attic/plumefrac.F,v 1.1 2007/09/22 03:11:03 dimitri Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PLUMEFRAC
C     !INTERFACE:
      SUBROUTINE PLUMEFRAC(
     I                  imax, fact,SPDepth,
     U                  plumek,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PLUMEFRAC
C     | o Compute saltplume penetration.
C     *==========================================================*
C     | Compute fraction of saltplume (flux) penetrating to
C     | specified depth, plumek, due to rejected salt
C     | during freezing.
C     | For example, if surface value is Saltplume0,
C     | and each level gets equal fraction 1/5 down to SPDepth=5,
C     | PLUMEFRAC will report plumek = 4/5 on output if the input
C     | plumek = 1. Else, output plumek = 0.                
C     | Reference : Duffy et al, (GRL 1999)
C     |
C     |
C     | =====
C     | Written by   : ATN (based on SWFRAC)
C     | Date         : Sep 13, 2007
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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

C     !LOCAL VARIABLES:
C     === Local variables ===
      _RL facz
      INTEGER i
      _RL     tinee
      parameter( tinee = 1.e-1)
#ifdef ALLOW_CAL
      LOGICAL first, changed
      INTEGER count0, count1
#endif /* ALLOW_CAL */
CEOP

#ifdef ALLOW_CAL
ceh3 this should have an IF ( useCALENDAR ) THEN
CML(
C     myIter = 0 makes cal_getMonthsRec always return  count0=12
C     so that jerl(count0) = 2.
C     The following lines are meant to be an example of how to
C     include time dependent water types. However, it would probably
C     make more sense to first think about a regionally varying
C     water type before implementing a time dependence.
CML      CALL  cal_GetMonthsRec(
CML     O     fac, first, changed, count0, count1,
CML     I     myTime, myIter, myThid )
CML      jwtype=jerl(count0)
CML)
#else /* ALLOW_CAL undef */
#endif /* ALLOW_CAL */

      DO i = 1,imax
         facz = abs(fact*plumek(i))
         IF (SPDepth(i).GT.facz .AND. SPDepth(i).GT.tinee) THEN
           !plumek(i) = min(plumedk(i),SPDepth(i)+facz)/SPDepth(i)
           plumek(i) = 1.0 - min(1.0,facz/abs(SPDepth(i)))
         ELSE
           plumek(i) = 0.
         ENDIF
      ENDDO

      RETURN
      END
