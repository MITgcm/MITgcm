C $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/Attic/AIM_DDT.h,v 1.1 2001/02/08 04:20:39 cnh Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     /==========================================================\
C     | AIM_DDT.h                                                |
C     | o AIM tendency fields in dynamics conforming arrays      |
C     \==========================================================/

C     AIM_UT - Holds AIM dudt in dynamics conforming array ( m/s^2  )
C     AIM_VT - Holds AIM dvdt in dynamics conforming array ( m/s^2  )
C              Values for both fields are on the AIM 'A' grid. This
C              aligns with the P-points on the dynamics 'C'-grid.
C
      COMMON /AIM_DDT/
     &                   AIM_UT, AIM_VT
      _RL  AIM_UT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  AIM_VT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_AIM */
