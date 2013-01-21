C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM2DYN.h,v 1.3 2013/01/21 21:55:32 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM
C     *==========================================================*
C     | AIM2DYN.h
C     | o AIM output fields in dynamics conforming arrays
C     *==========================================================*

C--   COMMON /AIM_DDT/
C     aim_drag :: Holds AIM surface drag term : C_drag*Rho*|V| ( kg.m-2.s-1 )
C                location : AIM "A" grid = center of the dynamics "C" grid
C     aim_dTdt :: Net tendency for potential temperature ( K/s    )
C     aim_dSdt :: Net tendency for water vapor           ( g/kg/s )
C aim_surfWind :: near surface atmospheric wind speed    ( m/s )
C
      COMMON /AIM_DDT/
     &                   aim_drag,
     &                   aim_dTdt,
     &                   aim_dSdt,
     &                   aim_surfWind
      _RL  aim_drag  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aim_dTdt  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  aim_dSdt  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  aim_surfWind (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
