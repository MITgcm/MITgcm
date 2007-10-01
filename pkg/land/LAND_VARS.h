C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_VARS.h,v 1.4 2007/10/01 15:20:53 jmc Exp $
C $Name:  $

#ifdef ALLOW_LAND

C     *==========================================================*
C     | LAND_VARS.h
C     | o Land model variables:
C     |   - prognostic variables
C     |   - forcing fields
C     |   - diagnostic variables
C     *==========================================================*


C---  COMMON /LAND_VARS_R/
C     land_groundT :: ground Temperature (oC) of each layer
C     land_enthalp :: enthalpy of each layer (J/m3) = Cp.T
C     land_groundW :: soil moisture [0-1] (= water content / field capacity)
C     land_skinT   :: surface skin Temperature (oC)
C     land_hSnow   :: thickness of snow over land (m)
C     land_snowAge :: snow age (s)
C     land_runOff  :: run-Off per surface unit [kg/m2/s]
C     land_enRnOf  :: energy flux associated with run-Off [W/m2]
C
      COMMON /LAND_VARS_R/
     &   land_groundT, land_enthalp, land_groundW,
     &   land_skinT, land_hSnow, land_snowAge,
     &   land_runOff, land_enRnOf

      _RL land_groundT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_enthalp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_groundW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_skinT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_hSnow  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_snowAge(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_runOff (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_enRnOf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---  COMMON /LAND_FORCFIELDS/
C     land_HeatFlx :: net surface downward Heat flux  [W/m2]
C     land_Pr_m_Ev :: Precipitation minus Evaporation [kg/m2/s]
C     land_EnWFlux :: Energy flux associated with Precip [W/m2] (snow, T_rain)
      COMMON /LAND_FORCFIELDS/
     &  land_HeatFlx, land_Pr_m_Ev, land_EnWFlux

      _RL land_HeatFlx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_Pr_m_Ev(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_EnWFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_LAND */
