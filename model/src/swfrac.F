C $Header: /u/gcmpack/models/MITgcmUV/model/src/ini_forcing.F,v 1.15 1998/12/15 00:20:34 adcroft Exp

#include "CPP_OPTIONS.h"

C I.Fukumori 8/24/98 
CStartOfInterface
      SUBROUTINE SWFRAC(
     I     imax, fact, z, jwtype,
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

      integer nwtype
      PARAMETER(nwtype=5)  ! max number of different water types 

C     === Routine arguments ===

C     input arguments
      INTEGER imax              ! number of vertical grid points
      _RS fact                  ! scale  factor to apply to depth array
      _RS z(imax)               ! vertical depth for desired sw fraction
                                ! (fact*z) is negative distance (m) from surface
      INTEGER jwtype            ! index for jerlov water type

C     output arguments
      _RS swdk(imax)            ! short wave (radiation) fractional decay

#ifdef ALLOW_KPP

C     === Local variables ===
      _RS facz
      _RS rfac(nwtype),a1(nwtype),a2(nwtype)
      INTEGER i
C
C     Jerlov water type :  I       IA      IB      II      III
C                jwtype    1       2       3       4       5
C
      DATA rfac         /  0.58 ,  0.62 ,  0.67 ,  0.77 ,  0.78 /
      DATA a1           /  0.35 ,  0.6  ,  1.0  ,  1.5  ,  1.4  /
      DATA a2           / 23.0  , 20.0  , 17.0  , 14.0  ,  7.9  /
C
      DO i = 1,imax
         facz = fact*z(i)
         IF (facz .LT. (-200.)) THEN
            swdk(i) = 0.
         ELSE
            swdk(i) =       rfac(jwtype)  * exp(facz/a1(jwtype))
     $                + (1.-rfac(jwtype)) * exp(facz/a2(jwtype))
         ENDIF
      ENDDO

#endif

      RETURN
      END
