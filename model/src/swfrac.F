C $Header: /u/gcmpack/MITgcm/model/src/swfrac.F,v 1.5 2000/06/21 19:00:47 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE SWFRAC(
     I     imax, fact, z,
     O     swdk )
C     /==========================================================\
C     | SUBROUTINE SWFRAC                                        |
C     | Compute fraction of solar short-wave flux penetrating to |
C     | specified depth, z, due to exponential decay in          |
C     | Jerlov water type jwtype.                                |
C     | Reference : Two band solar absorption model of Paulson   |
C     |             and Simpson (1977, JPO, 7, 952-956)          |
C     | Notes                                                    |
C     | =====                                                    |
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
C     z       vertical depth for desired sw fraction
C             (fact*z) is negative distance (m) from surface
C     jwtype  index for jerlov water type

      INTEGER imax
      _RS fact
      _RS z(imax)

C     output arguments
C     swdk    short wave (radiation) fractional decay
      _RS     swdk(imax)

C     === Local variables ===

C     max number of different water types 
      integer   nwtype
      PARAMETER(nwtype = 5)

      _RS facz
      _RL rfac(nwtype),a1(nwtype),a2(nwtype)
      INTEGER i
C
C     Jerlov water type :  I       IA      IB      II      III
C                jwtype    1       2       3       4       5
C
      DATA rfac         /  0.58 ,  0.62 ,  0.67 ,  0.77 ,  0.78 /
      DATA a1           /  0.35 ,  0.6  ,  1.0  ,  1.5  ,  1.4  /
      DATA a2           / 23.0  , 20.0  , 17.0  , 14.0  ,  7.9  /
      INTEGER jwtype
C
      jwtype=1
      DO i = 1,imax
         facz = fact*z(i)
         IF (facz .LT. (-200.)) THEN
            swdk(i) = 0.
         ELSE
            swdk(i) =       rfac(jwtype)  * exp(facz/a1(jwtype))
     $                + (1.-rfac(jwtype)) * exp(facz/a2(jwtype))
         ENDIF
      ENDDO

      RETURN
      END
