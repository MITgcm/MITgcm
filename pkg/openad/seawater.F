#include "CPP_OPTIONS.h"

C--  File seawater.F: routines that compute quantities related to seawater.
C--   Contents
C--   o SW_PTMP: routine to compute potential temperature (used by SW_TEMP)
C--   o SW_TEMP: routine to compute in-situ temperature from pot. temp.
C--   o SW_ADTG: routine to compute adiabatic temperature gradient
C--              (used by SW_PTMP)
C     TEOS10 routines (renamed and modified from MOM6 implementation)
C--   o CONVERT_CT2PT: S/R to convert conservative to potential temperature
C--   o CONVERT_PT2CT: S/R to convert potential to conservative temperature
C--                    (used by CONVERT_CT2PT)

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
C     S  :: salinity    [         (PSS-78) ]
C     T  :: temperature [degree C (IPTS-68)]
C     P  :: pressure    [dbar]
C     PR :: Reference pressure  [dbar]
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
      _RL S,T,P,PR
      _RL rv

C     !LOCAL VARIABLES
      _RL del_P ,del_th, th, q
      _RL onehalf, two, three
      PARAMETER ( onehalf = 0.5 _d 0, two = 2. _d 0, three = 3. _d 0 )
      _RL adtg_val
CEOP

C theta1
      del_P  = PR - P
      CALL SW_ADTG(S,T,P, adtg_val)
      del_th = del_P*adtg_val
      th     = T + onehalf*del_th
      q      = del_th
C theta2
      CALL SW_ADTG(S,th,P+onehalf*del_P, adtg_val)
      del_th = del_P*adtg_val

      th     = th + (1 - 1/sqrt(two))*(del_th - q)
      q      = (two-sqrt(two))*del_th + (-two+three/sqrt(two))*q

C theta3
      CALL SW_ADTG(S,th,P+onehalf*del_P, adtg_val)
      del_th = del_P*adtg_val
      th     = th + (1 + 1/sqrt(two))*(del_th - q)
      q      = (two + sqrt(two))*del_th + (-two-three/sqrt(two))*q

C theta4
      CALL SW_ADTG(S,th,P+del_P, adtg_val)
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
C
C     started:
C              Armin Koehl akoehl@ucsd.edu
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
      _RL S,T,P
      _RL rv

C     !LOCAL VARIABLES:
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: CONVERT_PT2CT

C !INTERFACE:
      SUBROUTINE CONVERT_PT2CT(
     I     Tp, Sa,
     O     Tc,
     I     myTime, myIter, myThid )
C     !DESCRIPTION:
C     Convert input potential temperature (degC) and absolute salinity
C     (g kg-1) to returned conservative temperature (degC) using the
C     polynomial expressions from TEOS-10.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     Tc        :: Conservative temperature (degC)
C     Sa        :: Absolute salinity        (g kg-1)
C     T         :: potential temperature (degC)
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL Tc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Sa(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j
C     Absolute salinity normalized by a plausible salinity range and its
C     square root (nondim)
      _RL x2, x
C
      _RL     zeRL
      PARAMETER ( zeRL = 0.0 _d 0 )

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        x2 = MAX(I_S0 * Sa(i,j), zeRL)
#ifdef ALLOW_AUTODIFF
        x = 0. _d 0
        IF ( x2 .GT. 0. _d 0 ) x = SQRT(x2)
#else
        x = SQRT(x2)
#endif
        Tc(i,j) = H00 + (Tp(i,j)*(H01 + Tp(i,j)*(H02 +  Tp(i,j)*(H03
     &       +  Tp(i,j)*(H04  + Tp(i,j)*(H05
     &       + Tp(i,j)*(H06 + Tp(i,j)* H07))))))
     &       + x2*(H20 + (Tp(i,j)*(H21 +  Tp(i,j)*(H22  + Tp(i,j)*(H23
     &       + Tp(i,j)*(H24 + Tp(i,j)*(H25 + Tp(i,j)*H26)))))
     &       +  x*(H30 + (Tp(i,j)*(H31  + Tp(i,j)*(H32
     &       + Tp(i,j)*(H33 + Tp(i,j)* H34)))
     &       + x*(H40 + (Tp(i,j)*(H41 + Tp(i,j)*(H42 + Tp(i,j)*(H43
     &       + Tp(i,j)*(H44 + Tp(i,j)*H45))))
     &       + x*(H50 + x*(H60 + x* H70)) )) )) )) )
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: CONVERT_CT2PT

C !INTERFACE:
      SUBROUTINE CONVERT_CT2PT(
     I         Tc, Sa,
     O         Tp,
     I         myTime, myIter, myThid )

C     !DESCRIPTION:
C     Convert input conservative (degC) and absolute salinity
C     (g kg-1) to returned potential temperature (degC) by inverting
C     the polynomial expressions from TEOS-10.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     Tc        :: Conservative temperature (degC)
C     Sa        :: Absolute salinity        (g kg-1)
C     Tp        :: potential temperature (degC)
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL Tc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Sa(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j
C     The numerator of a simple expression for potential temperature (degC)
      _RL Tp_num
C     The inverse of the denominator of a simple expression for
C     potential temperature (nondim)
      _RL I_Tp_den
C     The difference between an estimate of conservative temperature and
C     its target (degC)
      _RL Tc_diff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     A previous estimate of the potential tempearture (degC)
      _RL Tp_old
C     The partial derivative of potential temperature with conservative
C     temperature and inverse (nondim)
      _RL dTp_dTc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dTc_dTp
C     Absolute salinity normalized by a plausible salinity range and its
C     square root (nondim)
      _RL x2, x
C     intermediate temperature (degC)
      _RL Tmp
C
      _RL     zeRL
      PARAMETER ( zeRL = 0.0 _d 0 )

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Tp_num = TPN00 + (Sa(i,j)*(TPN10 + TPN20*Sa(i,j))
     &       + Tc(i,j)*(TPN01 + (TPN11*Sa(i,j) + TPN02*Tc(i,j))))
        I_Tp_den = 1.0 _d 0 / (1.0 _d 0
     &       + (TPD10*Sa(i,j) + Tc(i,j)*(TPD01 + TPD02*Tc(i,j))))
        Tp(i,j) = Tp_num*I_Tp_den
        dTp_dTc(i,j) = ((TPN01 + (TPN11*Sa(i,j) + 2.*TPN02*Tc(i,j)))
     &       - (TPD01 + 2.*TPD02*Tc(i,j))*Tp(i,j))*I_Tp_den
       ENDDO
      ENDDO

C--   Start the 1.5 iterations through the modified Newton-Raphson
C     iterative method, which is also known as the Newton-McDougall
C     method.  In this case 1.5 iterations converge to 64-bit machine
C     precision for oceanographically relevant temperatures and
C     salinities.
      CALL CONVERT_PT2CT (
     I     Tp, Sa,
     O     Tc_diff,
     I     myTime, myIter, myThid )
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Tc_diff(i,j) = Tc_diff(i,j) - Tc(i,j)
        Tp_old = Tp(i,j)
        Tp(i,j) = Tp_old - Tc_diff(i,j)*dTp_dTc(i,j)

C--   Estimate the potential temperature and its derivative from an C
C     approximate rational function fit.
        x2 = MAX(I_S0 * Sa(i,j), zeRL)
#ifdef ALLOW_AUTODIFF
        x = 0. _d 0
        IF ( x2 .GT. 0. _d 0 ) x = SQRT(x2)
#else
        x = SQRT(x2)
#endif
        Tmp = 0.5 _d 0 *(Tp(i,j) + Tp_old)
        dTc_dTp = (     H01 + Tmp*(2.*H02 + Tmp*(3.*H03 + Tmp*(4.*H04
     &       + Tmp*(5.*H05 + Tmp*(6.*H06 + Tmp*(7.*H07)))))) )
     &       + x2*(     (H21 + Tmp*(2.*H22 + Tmp*(3.*H23 + Tmp*(4.*H24
     &       + Tmp*(5.*H25 + Tmp*(6.*H26))))))
     &       +  x*(  (H31 + Tmp*(2.*H32 + Tmp*(3.*H33 + Tmp*(4.*H34))))
     &       + x*(H41 + Tmp*(2.*H42 + Tmp*(3.*H43
     &       + Tmp*(4.*H44 + Tmp*(5.*H45))))) ) )
        dTp_dTc(i,j) = 1. _d 0 / dTc_dTp

        Tp(i,j) = Tp_old - Tc_diff(i,j)*dTp_dTc(i,j)
       ENDDO
      ENDDO
      CALL CONVERT_PT2CT (
     I     Tp, Sa,
     O     Tc_diff,
     I     myTime, myIter, myThid )
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Tc_diff(i,j) = Tc_diff(i,j) - Tc(i,j)
        Tp_old = Tp(i,j)
        Tp(i,j) = Tp_old - Tc_diff(i,j)*dTp_dTc(i,j)
       ENDDO
      ENDDO

      RETURN
      END
