C     uVelD  :: D grid zonal velocity
C     vVelD  :: D grid meridional velocity

#ifdef ALLOW_CD_CODE
      COMMON /DYNVARS_CD/
     &                   uVelD, vVelD,
     &                   etaNm1,
     &                   uNM1,  vNM1
      _RL  uVelD (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVelD (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  etaNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uNM1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vNM1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /*  ALLOW_CD_CODE  */

