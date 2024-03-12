CBOP
C    !ROUTINE: EOS.h
C    !INTERFACE:
C    include EOS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | EOS.h
C     | o Header file defining coefficients for equation of state.
C     *==========================================================*
C     | The values from the model standard input file are
C     | stored into the variables held here.
C     *==========================================================*
C     \ev
CEOP

C     SItoBar  :: conversion factor for pressure, from Pa (SI Unit) to Bar
C     SItodBar :: conversion factor for pressure, from Pa (SI Unit) to deci Bar
      _RL SItoBar, SItodBar
      PARAMETER ( SItoBar  = 1.D-05 )
      PARAMETER ( SItodBar = 1.D-04 )

C Shared EOS Parameter
C     eosRefP0  :: reference atmospheric pressure used in EOS formulation
      COMMON /PARM_EOS_SHARED/ eosRefP0, equationOfState
      _RL eosRefP0
      CHARACTER*(6) equationOfState

C Linear equation of state
C     tAlpha    :: Linear EOS thermal expansion coefficient ( 1/degree ).
C     sBeta     :: Linear EOS haline contraction coefficient.
      COMMON /PARM_EOS_LIN/ tAlpha, sBeta
      _RL tAlpha
      _RL sBeta

C Equation of State (polynomial coeffients)
      COMMON /PARM_EOS_POLY3/ eosC,eosSig0,eosRefT,eosRefS
      _RL eosC(9,Nr+1),eosSig0(Nr+1),eosRefT(Nr+1),eosRefS(Nr+1)

C Full Equation of State
C     eosType = 'JMD95' (Jackett and McDougall 1995, JAOT)
C     eosType = 'UNESCO' (Millero et al. 1980, DSR)
C     COMMON /PARM_EOS_JMD95/
C     eosJMDCFw  :: of fresh water at pressure 0
C     eosJMDCSw  :: of sea water at pressure 0
C     eosJMDCKFw :: of secant bulk modulus K of fresh water at pressure 0
C     eosJMDCKSw :: of secant bulk modulus K of sea water at pressure 0
C     eosJMDCKP  :: of secant bulk modulus K at pressure p
C     eosType = 'MDJWF' (McDougall et al. 2003, JAOT)
C     COMMON /PARM_EOS_MDJWF/
C     eosMDJWFnum :: coefficients of numerator
C     eosMDJWFden :: coefficients of denominator
C     eosType = 'TEOS10' (McDougall et al. 2011, http://www.teos-10.org)
C     Note: this eos implies that variables THETA and SALT are interpreted
C     as conservative temperature and absolute salinity
C     COMMON /PARM_TEOS10/
C     teos        :: 48 coeffiencts of numerator and denominator
C     end nonlinear equation of state
      _RL eosJMDCFw(6), eosJMDCSw(9)
      _RL eosJMDCKFw(5), eosJMDCKSw(7), eosJMDCKP(14)
      COMMON /PARM_EOS_JMD95/
     &     eosJMDCFw, eosJMDCSw, eosJMDCKFw, eosJMDCKSw, eosJMDCKP
      _RL eosMDJWFnum(0:11), eosMDJWFden(0:12)
      COMMON /PARM_EOS_MDJWF/
     &     eosMDJWFnum, eosMDJWFden
      _RL teos(48)
      COMMON /PARM_TEOS10/
     &     teos

C     Parameters in the temperature conversion code for TEOS10
C     The TEOS 10 conversion factor to go from reference salinity to
C     practical salinity (nondim)
      _RL Sprac_Sref
C     The inverse of a plausible range of oceanic salinities (kg g-1)
      _RL I_S0
C     The inverse of a plausible range of oceanic temperatures (degC-1)
      _RL I_Ts
C     The inverse of the "specific heat" for use
C     with Conservative Temperature, as defined with TEOS10 (degC kg J-1)
      _RL I_cp0
      PARAMETER (
     &     Sprac_Sref = (35.0/35.16504),
     &     I_S0 = 0.025*Sprac_Sref, I_Ts = 0.025,
     &     I_cp0 = 1.0/3991.86795711963
     &     )

C     The following are coefficients of contributions to conservative
C     temperature as a function of the square root of normalized
C     absolute salinity with an offset (zS) and potential temperature
C     (T) with a contribution Hab * zS**a * T**b.  The numbers here are
C     copied directly from the corresponding gsw module, but the
C     expressions here do not use the same nondimensionalization for
C     pressure or temperature as they do.

C     Tp to Tc fit constant (degC)
      _RL H00
C     Tp to Tc fit T coef. (nondim)
      _RL H01
C     Tp to Tc fit T**2 coef. (degC-1)
      _RL H02
C     Tp to Tc fit T**3 coef. (degC-2)
      _RL H03
C     Tp to Tc fit T**4 coef. (degC-3)
      _RL H04
C     Tp to Tc fit T**5 coef. (degC-4)
      _RL H05
C     Tp to Tc fit T**6 coef. (degC-5)
      _RL H06
C     Tp to Tc fit T**7 coef. (degC-6)
      _RL H07
C     Tp to Tc fit zS**2 coef. (degC)
      _RL H20
C     Tp to Tc fit zS**2 * T coef. (nondim)
      _RL H21
C     Tp to Tc fit zS**2 * T**2 coef. (degC-1)
      _RL H22
C     Tp to Tc fit zS**2 * T**3 coef. (degC-2)
      _RL H23
C     Tp to Tc fit zS**2 * T**4 coef. (degC-3)
      _RL H24
C     Tp to Tc fit zS**2 * T**5 coef. (degC-4)
      _RL H25
C     Tp to Tc fit zS**2 * T**6 coef. (degC-5)
      _RL H26
C     Tp to Tc fit zS**3 coef. (degC)
      _RL H30
C     Tp to Tc fit zS** 3* T coef. (nondim)
      _RL H31
C     Tp to Tc fit zS**3 * T**2 coef. (degC-1)
      _RL H32
C     Tp to Tc fit zS**3 * T**3 coef. (degC-2)
      _RL H33
C     Tp to Tc fit zS**3 * T**4 coef. (degC-3)
      _RL H34
C     Tp to Tc fit zS**4 coef. (degC)
      _RL H40
C     Tp to Tc fit zS**4 * T coef. (nondim)
      _RL H41
C     Tp to Tc fit zS**4 * T**2 coef. (degC-1)
      _RL H42
C     Tp to Tc fit zS**4 * T**3 coef. (degC-2)
      _RL H43
C     Tp to Tc fit zS**4 * T**4 coef. (degC-3)
      _RL H44
C     Tp to Tc fit zS**4 * T**5 coef. (degC-4)
      _RL H45
C     Tp to Tc fit zS**5 coef. (degC)
      _RL H50
C     Tp to Tc fit zS**6 coef. (degC)
      _RL H60
C     Tp to Tc fit zS**7 coef. (degC)
      _RL H70

      PARAMETER (
     &     H00 = 61.01362420681071*I_cp0,
     &     H01 = 168776.46138048015*(I_cp0*I_Ts),
     &     H02 = -2735.2785605119625*(I_cp0*I_Ts**2),
     &     H03 = 2574.2164453821433*(I_cp0*I_Ts**3),
     &     H04 = -1536.6644434977543*(I_cp0*I_Ts**4),
     &     H05 = 545.7340497931629*(I_cp0*I_Ts**5),
     &     H06 = -50.91091728474331*(I_cp0*I_Ts**6),
     &     H07 = -18.30489878927802*(I_cp0*I_Ts**7),
     &     H20 = 268.5520265845071*I_cp0,
     &     H21 = -12019.028203559312*(I_cp0*I_Ts),
     &     H22 = 3734.858026725145*(I_cp0*I_Ts**2),
     &     H23 = -2046.7671145057618*(I_cp0*I_Ts**3),
     &     H24 = 465.28655623826234*(I_cp0*I_Ts**4),
     &     H25 = -0.6370820302376359*(I_cp0*I_Ts**5),
     &     H26 = -10.650848542359153*(I_cp0*I_Ts**6),
     &     H30 = 937.2099110620707*I_cp0,
     &     H31 = 588.1802812170108*(I_cp0*I_Ts),
     &     H32 = 248.39476522971285*(I_cp0*I_Ts**2),
     &     H33 = -3.871557904936333*(I_cp0*I_Ts**3),
     &     H34 = -2.6268019854268356*(I_cp0*I_Ts**4),
     &     H40 = -1687.914374187449*I_cp0,
     &     H41 = 936.3206544460336*(I_cp0*I_Ts),
     &     H42 = -942.7827304544439*(I_cp0*I_Ts**2),
     &     H43 = 369.4389437509002*(I_cp0*I_Ts**3),
     &     H44 = -33.83664947895248*(I_cp0*I_Ts**4),
     &     H45 = -9.987880382780322*(I_cp0*I_Ts**5),
     &     H50 = 246.9598888781377*I_cp0,
     &     H60 = 123.59576582457964*I_cp0,
     &     H70 = -48.5891069025409*I_cp0
     &     )
