#ifdef ALLOW_AIM

C     *==========================================================*
C     | AIM_CO2.h
C     | o AIM CO2 fields.
C     *==========================================================*

C---  COMMON /AIM_CO2RAD/
C     aim_pCO2     :: uniform pCO2 value for AIM radiation
      COMMON /AIM_CO2RAD/ aim_pCO2
      _RL aim_pCO2

#ifdef ALLOW_AIM_CO2
C---  COMMON /AIM_CO2VAR/
C     aim_CO2      :: atmospheric pCO2 (2d)
C     aimflxCo2    :: air-sea CO2 flux in mol/m^2/s (2d)
C     atm_CO2_Moles:: number of CO2 moles in the atmosphere
C     atm_pC02     :: atmospheric pCO2 of well-mixed atm. box

      COMMON /AIM_CO2VAR/ aim_CO2, aimflxCo2, atm_CO2_Moles, atm_pCO2
      _RL aim_CO2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aimflxCo2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL atm_CO2_Moles
      _RL atm_pCO2

C     total_atmos_moles :: total number of moles in the Earth atmosphere
C                          (taken from DIC pkg, dic_atmos.F)
      _RL total_atmos_moles
      PARAMETER ( total_atmos_moles = 1.77D20 )
#endif /* ALLOW_AIM_CO2 */

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
