C $Header: /u/gcmpack/MITgcm/pkg/land/LAND_VARS.h,v 1.1 2003/06/12 17:54:22 jmc Exp $
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
C     land_groundW :: soil moisture [0-1] (= water content / field capacity)
C     land_runOff  :: run-Off per surface unit [m/s]
      COMMON /LAND_VARS_R/
     &   land_groundT, land_groundW,
     &   land_runOff
 
      _RL land_groundT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_groundW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,land_nLev,nSx,nSy)
      _RL land_runOff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---  COMMON /LAND_FORCFIELDS/
C     land_HeatFLx :: net surface downward Heat flux [W/m2]
C     land_Pr_m_Ev :: Precipitation minus Evaporation [m/s]
      COMMON /LAND_FORCFIELDS/
     &  land_HeatFLx, land_Pr_m_Ev

      _RL land_HeatFLx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL land_Pr_m_Ev(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
 
#endif /* ALLOW_LAND */
