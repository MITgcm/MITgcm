C $Header: /u/gcmpack/MITgcm/verification/aim.5l_LatLon/code/Attic/AIM_DDT.h,v 1.2 2001/05/29 14:01:48 adcroft Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     /==========================================================\
C     | AIM_DDT.h                                                |
C     | o AIM tendency fields in dynamics conforming arrays      |
C     \==========================================================/

C aim_drag - Holds AIM surface C_drag*|V| term ( s-1 )
C                location : AIM "A" grid = center of the dynamics "C" grid
C     AIM_UT - Holds AIM dudt in dynamics conforming array ( m/s^2  )
C     AIM_VT - Holds AIM dvdt in dynamics conforming array ( m/s^2  )
C              Values for both fields are on the AIM 'A' grid. This
C              aligns with the P-points on the dynamics 'C'-grid.
C
      COMMON /AIM_DDT/
     &                   aim_drag, AIM_UT, AIM_VT
      _RL  aim_drag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  AIM_UT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  AIM_VT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_AIM */
