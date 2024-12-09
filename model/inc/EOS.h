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

C     TEOS10 coefficients
      _RL teos(48)

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

C     The following are coefficients in the nominator (TPNxx) or
C     denominator (TPDxx) of a simple rational expression that
C     approximately converts conservative temperature to potential
C     temperature.
C     Simple fit numerator constant (degC)
      _RL TPN00
C     Simple fit numerator Sa coef. (degC ppt-1)
      _RL TPN10
C     Simple fit numerator Sa**2 coef. (degC ppt-2)
      _RL TPN20
C     Simple fit numerator Tc coef. (nondim)
      _RL TPN01
C     Simple fit numerator Sa * Tc coef. (ppt-1)
      _RL TPN11
C     Simple fit numerator Tc**2 coef. (degC-1)
      _RL TPN02
C     Simple fit denominator Sa coef. (ppt-1)
      _RL TPD10
C     Simple fit denominator Tc coef. (degC-1)
      _RL TPD01
C     Simple fit denominator Tc**2 coef. (degC-2)
      _RL TPD02

      COMMON /PARM_TEOS10/
     &     teos,
     &     Sprac_Sref, I_S0, I_Ts, I_cp0,
     &     H00, H01, H02, H03, H04, H05, H06, H07,
     &     H20, H21, H22, H23, H24, H25, H26,
     &     H30, H31, H32, H33, H34,
     &     H40, H41, H42, H43, H44, H45,
     &     H50, H60, H70,
     &     TPN00, TPN10, TPN20,
     &     TPN01, TPN11, TPN02, TPD10, TPD01, TPD02
