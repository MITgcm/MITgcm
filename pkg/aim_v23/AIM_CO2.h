C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_CO2.h,v 1.1 2009/01/05 15:15:33 dfer Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     *==========================================================*
C     | AIM_CO2.h
C     | o AIM CO2 fields.
C     *==========================================================*

#ifdef ALLOW_AIM_CO2
C---  COMMON /AIMCO2/
C     aim_CO2      :: atmospheric pCO2 (2d)
C     aimflxCo2    :: air-sea CO2 flux in mol/m^2/s (2d)
C     Atm_CO2_Moles:: number of CO2 moles in the atmosphere 
C     Aim_CO2_Flag :: flag for CO2 mode, (1:prescribed, 2:interactive)
C     atm_pCo2     :: atmospheric pCO2 of well-mixed atm. box
C                     precribed value if mode=1 or initial value if mode=2 
C     total_atmos_moles :: total number of moles in the Earth atmosphere
C                          (taken from DIC pkg, dic_atmos.F)

      COMMON /AIMCO2/ aim_CO2, aimflxCo2, Atm_CO2_Moles,
     &                Aim_CO2_Flag, atm_pCo2
      _RL aim_CO2(NGP,MAX_NO_THREADS)
      _RL aimflxCo2(NGP,MAX_NO_THREADS)
      _RL Atm_CO2_Moles
      INTEGER Aim_CO2_Flag
      _RL atm_pCo2
      _RL total_atmos_moles
      PARAMETER ( total_atmos_moles = 1.77D20 )
#endif

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
