C $Header: /u/gcmpack/MITgcm/model/src/swfrac.F,v 1.7 2000/11/14 03:43:37 heimbach Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE SWFRAC(
     I     imax, fact,
     I     mytime, mythid,
     U     swdk )
C     /==========================================================\
C     | SUBROUTINE SWFRAC                                        |
C     | Compute fraction of solar short-wave flux penetrating to |
C     | specified depth, swdk, due to exponential decay in       |
C     | Jerlov water type jwtype.                                |
C     | Reference : Two band solar absorption model of Paulson   |
C     |             and Simpson (1977, JPO, 7, 952-956)          |
C     | Notes                                                    |
C     | =====                                                    |
C     | Parameter jwtype is hardcoded to 3 for time being.       |
C     | Below 200m the solar penetration gets set to zero,       |
C     | otherwise the limit for the exponent (+/- 5678) needs to | 
C     | be taken care of.                                        |
C     | Written by   : Jan Morzel                                |
C     | Date         : July 12, 1995                             |
C     \==========================================================/

      IMPLICIT NONE

C     === Routine arguments ===

C     input arguments
C     imax    number of vertical grid points
C     fact    scale  factor to apply to depth array
C     myTime  current time in simulation
C     myThid  thread number for this instance of the routine.

      INTEGER imax
      _RL     fact
      _RL     mytime
      integer mythid

C     input/output arguments
C     swdk    on input: vertical depth for desired sw fraction
C             (fact*swdk) is negative distance (m) from surface
C     swdk    on output: short wave (radiation) fractional decay
      _RL     swdk(imax)

C     === Local variables ===

C     max number of different water types 
      integer   nwtype  , jwtype
      PARAMETER(nwtype=5)

      _RL facz
      _RL rfac(nwtype),a1(nwtype),a2(nwtype)
      INTEGER i

#ifdef ALLOW_CALENDAR
      _RL     fac
      logical first, changed
      integer count0, count1
      integer myiter
      integer jerl(12)
      data jerl / 2 , 2 , 2 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , 3 , 2 /
#endif /* ALLOW_CALENDAR */

C
C     Jerlov water type :  I       IA      IB      II      III
C                jwtype    1       2       3       4       5
C
      DATA rfac         /  0.58 ,  0.62 ,  0.67 ,  0.77 ,  0.78 /
      DATA a1           /  0.35 ,  0.6  ,  1.0  ,  1.5  ,  1.4  /
      DATA a2           / 23.0  , 20.0  , 17.0  , 14.0  ,  7.9  /
C
#ifdef ALLOW_CALENDAR
      myiter=0
      call  cal_GetMonthsRec(
     O     fac, first, changed, count0, count1,
     I     mytime, myiter, mythid )
      jwtype=jerl(count0)
#else /* ALLOW_CALENDAR undef */
      jwtype=2
#endif /* ALLOW_CALENDAR */

      DO i = 1,imax
         facz = fact*swdk(i)
         IF (facz .LT. (-200.)) THEN
            swdk(i) = 0.
         ELSE
            swdk(i) =       rfac(jwtype)  * exp(facz/a1(jwtype))
     $                + (1.-rfac(jwtype)) * exp(facz/a2(jwtype))
         ENDIF
      ENDDO

      RETURN
      END
