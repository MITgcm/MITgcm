C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_frac.F,v 1.4 2007/12/21 22:49:09 atn Exp $
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
#include "SIZE.h"
#include "SALT_PLUME.h"

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
      _RL facz, dd, dd20
      INTEGER i, kk
      _RL     one, two, three, tempN, tempN20
      parameter( one = 1. _d 0, two = 2. _d 0, three = 3. _d 0 )

      DO i = 1,imax
         facz = abs(fact*plumek(i))
         IF (SPDepth(i).GT.facz) THEN

C     Default: uniform distribution, PlumeMethod=1, Npower=0
            IF (PlumeMethod .EQ. 1) THEN
               dd20 = (abs(SPDepth(i)))
               tempN   = one                  !input depth temp
               tempN20 = one
               DO kk=1,Npower+1
                  tempN   = facz*tempN        !raise to the Npower+1
                  tempN20 = dd20*tempN20
               ENDDO
               plumek(i) = one - min(one,tempN/tempN20)

            ELSEIF (PlumeMethod .EQ. 2) THEN  !exponential distribution
               dd = abs(SPDepth(i))
               plumek(i) = one -
     &              (exp(facz/dd)-one)/
     &              (exp(one    )-one)

C     PlumeMethod = 3, distribute salt LINEARLY between SPDepth and SPDepth/SPovershoot
C     (1-SPovershoot)% has already been taken into account in SPDepth calculation,
C     i.e., SPDepth = SPovershoot*SPDepth.
            ELSEIF (PlumeMethod .EQ. 3) THEN  !overshoot 20%
               dd20 = (abs(SPDepth(i)))
               dd   = dd20/SPovershoot
               IF( (facz.GE.dd).AND.(facz.LT.dd20) ) THEN
                  plumek(i) = one - min(one,(facz-dd)/(dd20-dd))
               ELSE
                  plumek(i) = one
               ENDIF
            
C     PlumeMethod = 5, dumping all salt at the top layer
            ELSEIF (PlumeMethod .EQ. 5) THEN
               dd   = 0.
               dd20 = one
               IF( (facz.GE.dd).AND.(facz.LT.dd20) ) THEN
                  plumek(i) = one - min(one,(facz-dd)/(dd20-dd))
               ELSE
                  plumek(i) = 0.
               ENDIF
            ELSE
C     PLumeMethod = 6, currently only works for Npower = 1 and 2.
               dd20 = (abs(SPDepth(i)))
               tempN   = one                  !input depth temp
               tempN20 = one
               DO kk=1,Npower+1
                  tempN   = facz*tempN        !raise to the Npower+1
                  tempN20 = dd20*tempN20
               ENDDO
               IF(Npower.EQ.1) THEN           !Npower=1
                  plumek(i) = one - min(one,two/dd20*facz-tempN/tempN20)
               ELSE                           !Npower=2
                  plumek(i) = one - min(one,
     &                 three/dd20*facz - three/(dd20*dd20)*facz*facz
     &                 + tempN/tempN20)
               ENDIF
               
            ENDIF
         ELSE
            plumek(i) = 0.
         ENDIF
      ENDDO
      
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
