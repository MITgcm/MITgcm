C $Header: /u/gcmpack/MITgcm/model/inc/DYNVARS.h,v 1.9 1999/03/22 15:54:02 adcroft Exp $
C
C     /==========================================================\
C     | DYNVARS.h                                                |
C     | o Dynamical model variables (common block DYNVARS_R)     |
C     |==========================================================|
C     | The value and two levels of time tendency are held for   |
C     | each prognostic variable.                                |
C     \==========================================================/
C
C     uVel  - zonal velocity (m/s, i=1 held at western face)
C     vVel  - meridional velocity (m/s, j=1 held at southern face)
C     theta - potential temperature (oC, held at pressure/tracer point)
C     salt  - salinity (ppt, held at pressure/tracer point)
C     gX, gXNM1 - Time tendencies at current and prvious time levels.
C     uVelD  - D grid zonal velocity
C     vVelD  - D grid meridional velocity

      COMMON /DYNVARS_R/ 
     &                   uVel,vVel,theta,salt,
     &                   gu,gv,gt,gs,guNm1,gvNm1,gtNm1,gsNm1
      _RL  uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  guNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gvNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gtNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gsNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef INCLUDE_CD_CODE
      COMMON /DYNVARS_CD/ 
     &                   uVelD, vVelD,
     &                   cg2d_xNM1,
     &                   uNM1,  vNM1,
     &                   guCD, gvCD
      _RL  uVeld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vVeld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  cg2d_xNM1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uNm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vNm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  guCD  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  gvCD  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
