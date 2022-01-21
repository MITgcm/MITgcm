#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SWFRAC
C     !INTERFACE:
      SUBROUTINE SWFRAC(
     I                  imax, fact,
     U                  swdk,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SWFRAC
C     | o Compute solar short-wave flux penetration.
C     *==========================================================*
C     | Compute fraction of solar short-wave flux penetrating to
C     | specified depth, swdk, due to exponential decay in
C     | Jerlov water type jwtype.
C     | Reference : Two band solar absorption model of Paulson
C     |             and Simpson (1977, JPO, 7, 952-956)
C     | Notes
C     | =====
C     | Parameter jwtype is hardcoded to 2 for time being.
C     | Below 200m the solar penetration gets set to zero,
C     | otherwise the limit for the exponent (+/- 5678) needs to
C     | be taken care of.
C     | Written by   : Jan Morzel
C     | Date         : July 12, 1995
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     input arguments
C     imax    :: number of vertical grid points
C     fact    :: scale  factor to apply to depth array
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id. number
      INTEGER imax
      _RL     fact
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     input/output arguments
C     swdk    :: on input: vertical depth for desired sw fraction
C               (fact*swdk) is negative distance (m) from surface
C     swdk    :: on output: short wave (radiation) fractional decay
      _RL     swdk(imax)

C     !LOCAL VARIABLES:
C     === Local variables ===
C     max number of different water types
      INTEGER   nwtype  , jwtype
      PARAMETER(nwtype=5)
      _RL facz
      _RL rfac(nwtype),a1(nwtype),a2(nwtype)
      INTEGER i
#ifdef ALLOW_CAL
c     _RL     fac
c     LOGICAL first, changed
c     INTEGER count0, count1, year0, year1
c     INTEGER jerl(12)
c     DATA jerl / 2 , 2 , 2 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , 3 , 2 /
#endif /* ALLOW_CAL */
CEOP

C     Jerlov water type :
C                  I          IA         IB         II         III
C     jwtype :     1          2          3          4          5
      DATA rfac / 0.58 _d 0, 0.62 _d 0, 0.67 _d 0, 0.77 _d 0, 0.78 _d 0/
      DATA a1   / 0.35 _d 0, 0.6  _d 0, 1.0  _d 0, 1.5  _d 0, 1.4  _d 0/
      DATA a2   / 23.0 _d 0, 20.0 _d 0, 17.0 _d 0, 14.0 _d 0, 7.9  _d 0/

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
CML     O     fac, first, changed, count0, count1, year0, year1,
CML     I     myTime, myIter, myThid )
CML      jwtype=jerl(count0)
CML)
      jwtype=2
#else /* ALLOW_CAL undef */
      jwtype=2
#endif /* ALLOW_CAL */

      DO i = 1,imax
        facz = fact*swdk(i)
        IF ( facz .LT. -200. _d 0 ) THEN
          swdk(i) = 0. _d 0
        ELSE
          swdk(i) =       rfac(jwtype)  * exp( facz/a1(jwtype) )
     &       + (1. _d 0 - rfac(jwtype)) * exp( facz/a2(jwtype) )
        ENDIF
      ENDDO

      RETURN
      END
