!BOP
!    !ROUTINE: EOS.h
!    !INTERFACE:
!    include EOS.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | EOS.h
! | o Header file defining coefficients for equation of state.
! *==========================================================*
! | The values from the model standard input file are
! | stored into the variables held here.
! *==========================================================*
! \ev
!EOP

! SItoBar  :: conversion factor for pressure, from Pa (SI Unit) to Bar
! SItodBar :: conversion factor for pressure, from Pa (SI Unit) to deci Bar
      _RL SItoBar, SItodBar
      PARAMETER ( SItoBar  = 1.D-05 )
      PARAMETER ( SItodBar = 1.D-04 )

! Shared EOS Parameter
! eosRefP0  :: reference atmospheric pressure used in EOS formulation
      COMMON /PARM_EOS_SHARED/ eosRefP0, equationOfState
      _RL eosRefP0
      CHARACTER(len=6) :: equationOfState

! Linear equation of state
! tAlpha    :: Linear EOS thermal expansion coefficient ( 1/degree ).
! sBeta     :: Linear EOS haline contraction coefficient.
      COMMON /PARM_EOS_LIN/ tAlpha, sBeta
      _RL tAlpha
      _RL sBeta

! Equation of State (polynomial coeffients)
      COMMON /PARM_EOS_POLY3/ eosC,eosSig0,eosRefT,eosRefS
      _RL eosC(9,Nr+1),eosSig0(Nr+1),eosRefT(Nr+1),eosRefS(Nr+1)

! Full Equation of State
! eosType = 'JMD95' (Jackett and McDougall 1995, JAOT)
! eosType = 'UNESCO' (Millero et al. 1980, DSR)
! COMMON /PARM_EOS_JMD95/
! eosJMDCFw  :: of fresh water at pressure 0
! eosJMDCSw  :: of sea water at pressure 0
! eosJMDCKFw :: of secant bulk modulus K of fresh water at pressure 0
! eosJMDCKSw :: of secant bulk modulus K of sea water at pressure 0
! eosJMDCKP  :: of secant bulk modulus K at pressure p
! eosType = 'MDJWF' (McDougall et al. 2003, JAOT)
! COMMON /PARM_EOS_MDJWF/
! eosMDJWFnum :: coefficients of numerator
! eosMDJWFden :: coefficients of denominator
! eosType = 'TEOS10' (McDougall et al. 2011, http://www.teos-10.org)
! Note: this eos implies that variables THETA and SALT are interpreted
! as conservative temperature and absolute salinity
! COMMON /PARM_TEOS10/
! teos        :: 48 coeffiencts of numerator and denominator
! end nonlinear equation of state
      _RL eosJMDCFw(6), eosJMDCSw(9)
      _RL eosJMDCKFw(5), eosJMDCKSw(7), eosJMDCKP(14)
      COMMON /PARM_EOS_JMD95/                                                     &
     &      eosJMDCFw, eosJMDCSw, eosJMDCKFw, eosJMDCKSw, eosJMDCKP
      _RL eosMDJWFnum(0:11), eosMDJWFden(0:12)
      COMMON /PARM_EOS_MDJWF/                                                     &
     &      eosMDJWFnum, eosMDJWFden

! TEOS10 coefficients
      _RL teos(48)

! Parameters in the temperature conversion code for TEOS10
! The TEOS 10 conversion factor to go from reference salinity to
! practical salinity (nondim)
      _RL Sprac_Sref
! The inverse of a plausible range of oceanic salinities (kg g-1)
      _RL I_S0
! The inverse of a plausible range of oceanic temperatures (degC-1)
      _RL I_Ts
! The inverse of the "specific heat" for use
! with Conservative Temperature, as defined with TEOS10 (degC kg J-1)
      _RL I_cp0

! The following are coefficients of contributions to conservative
! temperature as a function of the square root of normalized
! absolute salinity with an offset (zS) and potential temperature
! (T) with a contribution Hab * zS**a * T**b.  The numbers here are
! copied directly from the corresponding gsw module, but the
! expressions here do not use the same nondimensionalization for
! pressure or temperature as they do.

! Tp to Tc fit constant (degC)
      _RL H00
! Tp to Tc fit T coef. (nondim)
      _RL H01
! Tp to Tc fit T**2 coef. (degC-1)
      _RL H02
! Tp to Tc fit T**3 coef. (degC-2)
      _RL H03
! Tp to Tc fit T**4 coef. (degC-3)
      _RL H04
! Tp to Tc fit T**5 coef. (degC-4)
      _RL H05
! Tp to Tc fit T**6 coef. (degC-5)
      _RL H06
! Tp to Tc fit T**7 coef. (degC-6)
      _RL H07
! Tp to Tc fit zS**2 coef. (degC)
      _RL H20
! Tp to Tc fit zS**2 * T coef. (nondim)
      _RL H21
! Tp to Tc fit zS**2 * T**2 coef. (degC-1)
      _RL H22
! Tp to Tc fit zS**2 * T**3 coef. (degC-2)
      _RL H23
! Tp to Tc fit zS**2 * T**4 coef. (degC-3)
      _RL H24
! Tp to Tc fit zS**2 * T**5 coef. (degC-4)
      _RL H25
! Tp to Tc fit zS**2 * T**6 coef. (degC-5)
      _RL H26
! Tp to Tc fit zS**3 coef. (degC)
      _RL H30
! Tp to Tc fit zS** 3* T coef. (nondim)
      _RL H31
! Tp to Tc fit zS**3 * T**2 coef. (degC-1)
      _RL H32
! Tp to Tc fit zS**3 * T**3 coef. (degC-2)
      _RL H33
! Tp to Tc fit zS**3 * T**4 coef. (degC-3)
      _RL H34
! Tp to Tc fit zS**4 coef. (degC)
      _RL H40
! Tp to Tc fit zS**4 * T coef. (nondim)
      _RL H41
! Tp to Tc fit zS**4 * T**2 coef. (degC-1)
      _RL H42
! Tp to Tc fit zS**4 * T**3 coef. (degC-2)
      _RL H43
! Tp to Tc fit zS**4 * T**4 coef. (degC-3)
      _RL H44
! Tp to Tc fit zS**4 * T**5 coef. (degC-4)
      _RL H45
! Tp to Tc fit zS**5 coef. (degC)
      _RL H50
! Tp to Tc fit zS**6 coef. (degC)
      _RL H60
! Tp to Tc fit zS**7 coef. (degC)
      _RL H70

! The following are coefficients in the nominator (TPNxx) or
! denominator (TPDxx) of a simple rational expression that
! approximately converts conservative temperature to potential
! temperature.
! Simple fit numerator constant (degC)
      _RL TPN00
! Simple fit numerator Sa coef. (degC ppt-1)
      _RL TPN10
! Simple fit numerator Sa**2 coef. (degC ppt-2)
      _RL TPN20
! Simple fit numerator Tc coef. (nondim)
      _RL TPN01
! Simple fit numerator Sa * Tc coef. (ppt-1)
      _RL TPN11
! Simple fit numerator Tc**2 coef. (degC-1)
      _RL TPN02
! Simple fit denominator Sa coef. (ppt-1)
      _RL TPD10
! Simple fit denominator Tc coef. (degC-1)
      _RL TPD01
! Simple fit denominator Tc**2 coef. (degC-2)
      _RL TPD02

      COMMON /PARM_TEOS10/                                                        &
     &      teos,                                                                 &
     &      Sprac_Sref, I_S0, I_Ts, I_cp0,                                        &
     &      H00, H01, H02, H03, H04, H05, H06, H07,                               &
     &      H20, H21, H22, H23, H24, H25, H26,                                    &
     &      H30, H31, H32, H33, H34,                                              &
     &      H40, H41, H42, H43, H44, H45,                                         &
     &      H50, H60, H70,                                                        &
     &      TPN00, TPN10, TPN20,                                                  &
     &      TPN01, TPN11, TPN02, TPD10, TPD01, TPD02
