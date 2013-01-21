C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM_CO2.h,v 1.3 2013/01/21 21:49:33 jmc Exp $
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
C     atmpC02init  :: atmospheric pCO2 of well-mixed atm. box
C                     prescribed value if mode=1 or initial value if mode=2
C     atm_pC02     :: atmospheric pCO2 of well-mixed atm. box
C     total_atmos_moles :: total number of moles in the Earth atmosphere
C                          (taken from DIC pkg, dic_atmos.F)

      COMMON /AIMCO2/ aim_CO2, aimflxCo2, Atm_CO2_Moles, atm_pCO2,
     &                atmpCO2init, Aim_CO2_Flag
      _RL aim_CO2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aimflxCo2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Atm_CO2_Moles
      _RL atm_pCO2
      _RL atmpCO2init
      INTEGER Aim_CO2_Flag
      _RL total_atmos_moles
      PARAMETER ( total_atmos_moles = 1.77D20 )
#endif

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
