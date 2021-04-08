#include "CPP_OPTIONS.h"

C--  File seawater.F: routines that compute quantities related to seawater.
C--   Contents
C--   o SW_PTMP: routine to compute potential temperature (used by SW_TEMP)
C--   o SW_TEMP: routine to compute in-situ temperature from pot. temp.
C--   o SW_ADTG: routine to compute adiabatic temperature gradient
C--              (used by SW_PTMP)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_PTMP
C     !INTERFACE:
      SUBROUTINE SW_PTMP  (S,T,P,PR, rv)

C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_PTMP
C     | o compute potential temperature as per UNESCO 1983 report.
C     *=============================================================*
C     \ev
C     started:
C              Armin Koehl akoehl@ucsd.edu
C
C     ==================================================================
C     SUBROUTINE SW_PTMP
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     === Global variables ===

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     S  :: salinity    [         (PSS-78) ]
C     T  :: temperature [degree C (IPTS-68)]
C     P  :: pressure    [db]
C     PR :: Reference pressure  [db]
C     rv :: return value (potential temeparture in degree C)
      _RL S,T,P,PR
      _RL rv

C     !LOCAL VARIABLES
C     === local variables ===
      _RL del_P ,del_th, th, q
      _RL onehalf, two, three
      PARAMETER ( onehalf = 0.5 _d 0, two = 2. _d 0, three = 3. _d 0 )
      _RL adtg_val
CEOP

C theta1
      del_P  = PR - P
      call sw_adtg(S,T,P, adtg_val)
      del_th = del_P*adtg_val
      th     = T + onehalf*del_th
      q      = del_th
C theta2
      call sw_adtg(S,th,P+onehalf*del_P, adtg_val)
      del_th = del_P*adtg_val

      th     = th + (1 - 1/sqrt(two))*(del_th - q)
      q      = (two-sqrt(two))*del_th + (-two+three/sqrt(two))*q

C theta3
      call sw_adtg(S,th,P+onehalf*del_P, adtg_val)
      del_th = del_P*adtg_val
      th     = th + (1 + 1/sqrt(two))*(del_th - q)
      q      = (two + sqrt(two))*del_th + (-two-three/sqrt(two))*q

C theta4
      call sw_adtg(S,th,P+del_P, adtg_val)
      del_th = del_P*adtg_val
      rv     = th + (del_th - two*q)/(two*three)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_TEMP
C     !INTERFACE:
      SUBROUTINE SW_TEMP( S, T, P, PR, rv)
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_TEMP
C     | o compute in-situ temperature from potential temperature
C     *=============================================================*
C
C     REFERENCES:
C     Fofonoff, P. and Millard, R.C. Jr
C     Unesco 1983. Algorithms for computation of fundamental properties of
C     seawater, 1983. _Unesco Tech. Pap. in Mar. Sci._, No. 44, 53 pp.
C     Eqn.(31) p.39
C
C     Bryden, H. 1973.
C     New Polynomials for thermal expansion, adiabatic temperature gradient
C     and potential temperature of sea water.
C     DEEP-SEA RES., 1973, Vol20,401-408.
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     S      :: salinity
C     T      :: potential temperature
C     P      :: pressure
C     PR     :: reference pressure
C     rv :: return value (in-situ temeparture in degree C)
      _RL  S, T, P, PR
      _RL rv

C     !LOCAL VARIABLES:
C     === local variables ===
CEOP

      CALL SW_PTMP  (S,T,PR,P,rv)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_ADTG
C     !INTERFACE:
      SUBROUTINE SW_ADTG  (S,T,P, rv)

C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_ADTG
C     | o compute adiabatic temperature gradient as per UNESCO 1983 routines.
C     *=============================================================*
C     \ev
C
C     started:
C              Armin Koehl akoehl@ucsd.edu

C     !USES:
      IMPLICIT NONE
C     === Global variables ===

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      _RL S,T,P
      _RL rv

C     !LOCAL VARIABLES:
C     === local variables ===
      _RL a0,a1,a2,a3,b0,b1,c0,c1,c2,c3,d0,d1,e0,e1,e2
      _RL sref
CEOP

      sref = 35. _d 0
      a0 =  3.5803 _d -5
      a1 = +8.5258 _d -6
      a2 = -6.836 _d -8
      a3 =  6.6228 _d -10

      b0 = +1.8932 _d -6
      b1 = -4.2393 _d -8

      c0 = +1.8741 _d -8
      c1 = -6.7795 _d -10
      c2 = +8.733 _d -12
      c3 = -5.4481 _d -14

      d0 = -1.1351 _d -10
      d1 =  2.7759 _d -12

      e0 = -4.6206 _d -13
      e1 = +1.8676 _d -14
      e2 = -2.1687 _d -16

      rv =      a0 + (a1 + (a2 + a3*T)*T)*T
     &     + (b0 + b1*T)*(S-sref)
     &     + ( (c0 + (c1 + (c2 + c3*T)*T)*T) + (d0 + d1*T)*(S-sref) )*P
     &     + (  e0 + (e1 + e2*T)*T )*P*P

      RETURN
      END
