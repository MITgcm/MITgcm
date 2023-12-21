#include "CPP_OPTIONS.h"

C--  File seawater.F: routines that compute quantities related to seawater.
C--   Contents
C     Seawater (SW) librabry routines
C--   o SW_PTMP: function to compute potential temperature
C--   o SW_TEMP: function to compute in-situ temperature from pot. temp.
C--   o SW_ADTG: function to compute adiabatic temperature gradient
C--              (used by both SW_PTMP & SW_TEMP)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_PTMP
C     !INTERFACE:
      _RL FUNCTION SW_PTMP  (S,T,P,PR)

C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SW_PTMP
C     | o compute potential temperature as per UNESCO 1983 report.
C     *=============================================================*
C
C     started:
C              Armin Koehl akoehl@ucsd.edu
C
C     ==================================================================
C     SUBROUTINE SW_PTMP
C     ==================================================================
C     S  :: salinity    [         (PSS-78) ]
C     T  :: temperature [degree C (IPTS-68)]
C     P  :: pressure    [db]
C     PR :: Reference pressure  [db]
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
      _RL S,T,P,PR

C     !FUNCTIONS:
      _RL sw_adtg
      EXTERNAL sw_adtg

C     !LOCAL VARIABLES
      _RL del_P ,del_th, th, q
      _RL onehalf, two, three
      PARAMETER ( onehalf = 0.5 _d 0, two = 2. _d 0, three = 3. _d 0 )
CEOP

C theta1
      del_P  = PR - P
      del_th = del_P*sw_adtg(S,T,P)
      th     = T + onehalf*del_th
      q      = del_th
C theta2
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)

      th     = th + (1 - 1/sqrt(two))*(del_th - q)
      q      = (two-sqrt(two))*del_th + (-two+three/sqrt(two))*q

C theta3
      del_th = del_P*sw_adtg(S,th,P+onehalf*del_P)
      th     = th + (1 + 1/sqrt(two))*(del_th - q)
      q      = (two + sqrt(two))*del_th + (-two-three/sqrt(two))*q

C theta4
      del_th = del_P*sw_adtg(S,th,P+del_P)
      SW_PTMP     = th + (del_th - two*q)/(two*three)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_TEMP
C     !INTERFACE:
      _RL FUNCTION SW_TEMP( S, T, P, PR )
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
      _RL  S, T, P, PR
CEOP

C     !FUNCTIONS:
      _RL sw_ptmp
      EXTERNAL sw_ptmp

      SW_temp = SW_PTMP (S,T,PR,P)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: SW_ADTG
C     !INTERFACE:
      _RL FUNCTION SW_ADTG  (S,T,P)

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

      SW_ADTG =      a0 + (a1 + (a2 + a3*T)*T)*T
     &     + (b0 + b1*T)*(S-sref)
     &     + ( (c0 + (c1 + (c2 + c3*T)*T)*T) + (d0 + d1*T)*(S-sref) )*P
     &     + (  e0 + (e1 + e2*T)*T )*P*P

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     > Convert input potential temperature [degC] and absolute salinity
C     [g kg-1] to returned conservative temperature [degC] using the
C     polynomial expressions from TEOS-10.
      _RL FUNCTION poTemp_to_consTemp(T, Sa)

      IMPLICIT NONE
#include "SIZE.h"
#include "EOS.h"
      _RL T     !< Potential temperature [degC]
      _RL Sa    !< Absolute salinity [g kg-1]

C Local variables
C     Absolute salinity normalized by a plausible salinity range [nondim]
      _RL x2
C     Square root of normalized absolute salinity [nondim]
      _RL x

      x2 = MAX(I_S0 * Sa, 0.0)
#ifdef ALLOW_AUTODIFF
      x = 0. _ d 0
      IF ( x2 .GT. 0. _d 0 ) x = SQRT(x2)
#else
      x = SQRT(x2)
#endif

      poTemp_to_consTemp
     &     = H00 + (T*(H01 + T*(H02 +  T*(H03 +  T*(H04  + T*(H05
     &                                     + T*(H06 + T* H07))))))
     &                    + x2*(H20 + (T*(H21 +  T*(H22  + T*(H23
     &                              + T*(H24 + T*(H25 + T*H26)))))
     &                              +  x*(H30 + (T*(H31  + T*(H32
     &                                        + T*(H33 + T* H34)))
     &     + x*(H40 + (T*(H41 + T*(H42 + T*(H43 + T*(H44 + T*H45))))
     &     + x*(H50 + x*(H60 + x* H70)) )) )) )) )

      RETURN
      END

C     > Return the partial derivative of conservative temperature with
C     potential temperature [nondim] based on the polynomial
C     expressions from TEOS-10.
      _RL function dTc_dTp(T, Sa)
      implicit none
#include "SIZE.h"
#include "EOS.h"
      _RL T     !< Potential temperature [degC]
      _RL Sa    !< Absolute salinity [g kg-1]

C Local variables
C     Absolute salinity normalized by a plausible salinity range [nondim]
      _RL x2
C     Square root of normalized absolute salinity [nondim]
      _RL x

      x2 = MAX(I_S0 * Sa, 0.0)
#ifdef ALLOW_AUTODIFF
      x = 0. _ d 0
      IF ( x2 .GT. 0. _d 0 ) x = SQRT(x2)
#else
      x = SQRT(x2)
#endif

      dTc_dTp = (     H01 + T*(2.*H02 + T*(3.*H03 + T*(4.*H04
     &     + T*(5.*H05 + T*(6.*H06 + T*(7.*H07)))))) )
     &     + x2*(     (H21 + T*(2.*H22 + T*(3.*H23 + T*(4.*H24
     &     + T*(5.*H25 + T*(6.*H26))))))
     &     +  x*(  (H31 + T*(2.*H32 + T*(3.*H33 + T*(4.*H34))))
     &     + x*(H41 + T*(2.*H42 + T*(3.*H43
     &     + T*(4.*H44 + T*(5.*H45))))) ) )

      RETURN
      END

C     > Convert input potential temperature [degC] and absolute salinity
C     [g kg-1] to returned conservative temperature [degC] by
C     inverting the polynomial expressions from TEOS-10.
      _RL FUNCTION consTemp_to_poTemp(Tc, Sa)

      IMPLICIT NONE
#include "SIZE.h"
#include "EOS.h"
      _RL Tc    !< Conservative temperature [degC]
      _RL Sa    !< Absolute salinity [g kg-1]

      _RL poTemp_to_consTemp, dTc_dTp
      EXTERNAL poTemp_to_consTemp, dTc_dTp

      _RL Tp
C     The numerator  of a simple expression for potential temperature [degC]
      _RL Tp_num
C     The inverse of the denominator of a simple expression for
C     potential temperature [nondim]
      _RL I_Tp_den
C     The difference between an estimate of conservative temperature and
C     its target [degC]
      _RL Tc_diff
C     A previous estimate of the potential tempearture [degC]
      _RL Tp_old
C     The partial derivative of potential temperature with conservative
C     temperature [nondim]
      _RL dTp_dTc

C     The following are coefficients in the nominator (TPNxx) or
C     denominator (TPDxx) of a simple rational expression that
C     approximately converts conservative temperature to potential
C     temperature.
      _RL TPN00 ! Simple fit numerator constant [degC]
      _RL TPN10 ! Simple fit numerator Sa coef. [degC ppt-1]
      _RL TPN20 ! Simple fit numerator Sa**2 coef. [degC ppt-2]
      _RL TPN01 ! Simple fit numerator Tc coef. [nondim]
      _RL TPN11 ! Simple fit numerator Sa * Tc coef. [ppt-1]
      _RL TPN02 ! Simple fit numerator Tc**2 coef. [degC-1]
      _RL TPD10 ! Simple fit denominator Sa coef. [ppt-1]
      _RL TPD01 ! Simple fit denominator Tc coef. [degC-1]
      _RL TPD02 ! Simple fit denominator Tc**2 coef. [degC-2]

      PARAMETER (
     &     TPN00 = -1.446013646344788e-2,
     &     TPN10 = -3.305308995852924e-3*Sprac_Sref,
     &     TPN20 =  1.062415929128982e-4*Sprac_Sref**2,
     &     TPN01 =  9.477566673794488e-1,
     &     TPN11 =  2.166591947736613e-3*Sprac_Sref,
     &     TPN02 =  3.828842955039902e-3,
     &     TPD10 =  6.506097115635800e-4*Sprac_Sref,
     &     TPD01 =  3.830289486850898e-3,
     &     TPD02 =  1.247811760368034e-6
     &     )

C--   Estimate the potential temperature and its derivative from an C
C     approximate rational function fit.
      Tp_num = TPN00 + (Sa*(TPN10 + TPN20*Sa)
     &     + Tc*(TPN01 + (TPN11*Sa + TPN02*Tc)))
      I_Tp_den = 1.0 / (1.0 + (TPD10*Sa + Tc*(TPD01 + TPD02*Tc)))
      Tp = Tp_num*I_Tp_den
      dTp_dTc = ((TPN01 + (TPN11*Sa + 2.*TPN02*Tc))
     &     - (TPD01 + 2.*TPD02*Tc)*Tp)*I_Tp_den

C--   Start the 1.5 iterations through the modified Newton-Raphson
C     iterative method, which is also known as the Newton-McDougall
C     method.  In this case 1.5 iterations converge to 64-bit machine
C     precision for oceanographically relevant temperatures and
C     salinities.

      Tc_diff = poTemp_to_consTemp(Tp, Sa) - Tc
      Tp_old = Tp
      Tp = Tp_old - Tc_diff*dTp_dTc

      dTp_dTc = 1.0 / dTc_dTp(0.5*(Tp + Tp_old), Sa)

      Tp = Tp_old - Tc_diff*dTp_dTc
      Tc_diff = poTemp_to_consTemp(Tp, Sa) - Tc
      Tp_old = Tp

      consTemp_to_poTemp = Tp_old - Tc_diff*dTp_dTc

      RETURN
      END
