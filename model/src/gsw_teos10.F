#include "CPP_OPTIONS.h"

C--  File gsw_teos10.F: routines that compute quantities related to seawater.
C--   Contents
C--   TEOS-10 routines (Gibbs seawater, GSW)
C--   o GSW_PT_FROM_CT: function to compute potential temperature 
C--              from conservative temperature and absolute salinity
C--   o GSW_CT_FROM_PT: function to compute conservative temperature with
C--              from potential temperature and absolute salinity
C--   o GSW_GIBBS_PT0_PT0: function to compute specific Gibbs free energy
C--              from potential temperature and absolute salinity

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GSW_PT_FROM_CT
C     !INTERFACE:
      _RL FUNCTION GSW_PT_FROM_CT(SA,CT)
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  GSW_PT_FROM_CT
C     | o compute potential temperature at reference level 0 dbar
C     |   from conservative temperature (CT) and absolute
C     |    salinity (SA)
C     | o this is a more or less shameless copy of the teos-10 code
C     |   available at http://www.teos-10.org
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     SA :: Absolute Salinity                               (g/kg)
C     CT :: Conservative Temperature                        (deg C)
      _RL SA,CT

C     !FUNCTIONS:
C     == Functions ==
CML      _RL gsw_gibbs
      _RL gsw_gibbs_pt0_pt0
      _RL gsw_ct_from_pt
CML      EXTERNAL gsw_gibbs, gsw_gibbs_pt0_pt0, gsw_ct_from_pt
      EXTERNAL gsw_gibbs_pt0_pt0, gsw_ct_from_pt

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER n0, n2
      _RL s1, p0, cp0 
      _RL a0, a1, a2, a3, a4, a5, b0, b1, b2, b3
      _RL a5ct, b3ct, ct_factor, pt_num, pt_den, ct_diff
      _RL pt, pt_old, ptm, dct_dpt
CEOP

      cp0 = 3991.86795711963 _d 0    

      n0 = 0
      n2 = 2

      s1 = SA * 35. _d 0 / 35.16504 _d 0
      p0 = 0. _d 0

      a0 = -1.446013646344788 _d -2
      a1 = -3.305308995852924 _d -3
      a2 =  1.062415929128982 _d -4
      a3 =  9.477566673794488 _d -1
      a4 =  2.166591947736613 _d -3
      a5 =  3.828842955039902 _d -3
      
      b0 =  1.000000000000000 _d +0
      b1 =  6.506097115635800 _d -4
      b2 =  3.830289486850898 _d -3
      b3 =  1.247811760368034 _d -6

      a5ct = a5*CT
      b3ct = b3*CT

      ct_factor = (a3 + a4*s1 + a5ct)
      pt_num    = a0 + s1*(a1 + a2*s1) + CT*ct_factor
      pt_den    = b0 + b1*s1 + CT*(b2 + b3ct)
      pt        = (pt_num)/(pt_den)

      dct_dpt   = (pt_den)/(ct_factor + a5ct - (b2 + b3ct + b3ct)*pt)

C     start the 1.5 iterations through the modified Newton-Rapshon 
C     iterative method.  

      ct_diff = gsw_ct_from_pt(sa,pt) - CT
      pt_old  = pt
      pt      = pt_old - (ct_diff)/dct_dpt
      ptm     = 0.5 _d 0*(pt + pt_old)

      dct_dpt = -(ptm + 273.15 _d 0)*gsw_gibbs_pt0_pt0(sa,ptm)/cp0

      pt             = pt_old - (ct_diff)/dct_dpt
      ct_diff        = gsw_ct_from_pt(sa,pt) - CT
      pt_old         = pt
      GSW_PT_FROM_CT = pt_old - (ct_diff)/dct_dpt

      RETURN 
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GSW_CT_FROM_PT
C     !INTERFACE:
      _RL FUNCTION GSW_CT_FROM_PT(SA,PT)
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  GSW_CT_FROM_PT
C     | o compute conservative temperature from potential 
C     |   temperature (PT)  at reference level 0 dbar and absolute
C     |   salinity (SA)
C     | o this is a more or less shameless copy of the teos-10 code
C     |   available at http://www.teos-10.org
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     SA :: Absolute Salinity                               (g/kg)
C     PT :: Potential Temperature                          (deg C)
      _RL sa, pt

C     !FUNCTIONS:
C     == Functions ==

C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL pot_enthalpy, sfac
      _RL x2, x, y, cp0
CEOP

      sfac = 0.0248826675584615 _d 0 

      x2   = sfac*sa
      x    = 0. _d 0
      if (x2.gt.0. _d 0) x = sqrt(x2)
C     normalize for F03 and F08
      y    = pt*0.025 _d 0

      pot_enthalpy =  61.01362420681071 _d 0 +
     &     y*(168776.46138048015 _d 0 +
     &     y*(-2735.2785605119625 _d 0 + y*(2574.2164453821433 _d 0 + 
     &     y*(-1536.6644434977543 _d 0 + y*(545.7340497931629 _d 0 + 
     &     (-50.91091728474331 _d 0 - 18.30489878927802 _d 0*y)*y))))) + 
     &     x2*(268.5520265845071 _d 0 + y*(-12019.028203559312 _d 0 + 
     &     y *(3734.858026725145 _d 0 + y*(-2046.7671145057618 _d 0 + 
     &     y*(465.28655623826234 _d 0 + (-0.6370820302376359 _d 0 - 
     &     10.650848542359153 _d 0*y)*y)))) + 
     &     x*(937.2099110620707 _d 0 + y*(588.1802812170108 _d 0 +
     &     y*(248.39476522971285 _d 0 + (-3.871557904936333 _d 0 -
     &     2.6268019854268356 _d 0*y)*y)) + 
     &     x*(-1687.914374187449 _d 0 + x*(246.9598888781377 _d 0 + 
     &     x*(123.59576582457964 _d 0 - 48.5891069025409 _d 0*x)) + 
     &     y*( 936.3206544460336 _d 0 + 
     &     y*(-942.7827304544439 _d 0 + y*(369.4389437509002 _d 0 + 
     &     (-33.83664947895248 _d 0 - 9.987880382780322 _d 0*y)*y))))))

      cp0 = 3991.86795711963 _d 0

      gsw_ct_from_pt = pot_enthalpy/cp0

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GSW_GIBBS_PT0_PT0
C     !INTERFACE:
      _RL FUNCTION GSW_GIBBS_PT0_PT0(SA,PT0)
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R GSW_GIBBS_PT0_PT0
C     | o helper routine that computes the specific Gibbs free
C     |   energy from potential temperature (PT) and absolute
C     |   salinity (SA) for pressure 0 dbar
C     | o this is a more or less shameless copy of the teos-10 code
C     |   available at http://www.teos-10.org
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     SA :: Absolute Salinity                               (g/kg)
C     PT :: Potential Temperature at p = 0 dbar             (deg C)
      _RL sa, pt0

C     !FUNCTIONS:
C     == Functions ==

C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL sfac, x2, x, y, g03, g08
CEOP

      sfac = 0.0248826675584615

      x2   = sfac*sa
      x    = 0. _d 0
      if (x2.gt.0. _d 0) x = sqrt(x2)
      y    = pt0*0.025 _d 0

      g03 = -24715.571866078 +
     &     y*(4420.4472249096725 +
     &     y*(-1778.231237203896 +
     &     y*(1160.5182516851419 +
     &     y*(-569.531539542516  + y*128.13429152494615))))

      g08 = x2*(1760.062705994408  + x*(-86.1329351956084 +
     &     x*( -137.1145018408982  + y*(296.20061691375236 +
     &     y* (-205.67709290374563 + 49.9394019139016*y))) + 
     &     y*(  -60.136422517125   + y*10.50720794170734)) +
     &     y*(-1351.605895580406   + y*(1097.1125373015109 +
     &     y*( -433.20648175062206 + 63.905091254154904*y))))

      gsw_gibbs_pt0_pt0 = (g03 + g08)*0.000625

      RETURN
      END
