C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/AIM2DYN.h,v 1.2 2005/06/23 20:02:50 edhill Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     /==========================================================\
C     | AIM2DYN.h                                                |
C     | o AIM output fields in dynamics conforming arrays        |
C     \==========================================================/

C     aim_drag - Holds AIM surface drag term : C_drag*Rho*|V| ( kg.m-2.s-1 )
C                location : AIM "A" grid = center of the dynamics "C" grid
C     aim_dTdt - Net tendency for potential temperature ( oK/s    )
C     aim_dSdt - Net tendency for water vapor           ( g/kg/s )
C
      COMMON /AIM_DDT/
     &                   aim_drag, 
     &                   aim_dTdt,
     &                   aim_dSdt
      _RL  aim_drag  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aim_dTdt  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  aim_dSdt  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
