C $Header: /u/gcmpack/MITgcm/model/inc/DYNVARS.h,v 1.2 1998/04/24 02:11:36 cnh Exp $
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
C     rho   - density ( kg/m^3 )
      COMMON /DYNVARS_R/ uVel,vVel,theta,salt,rho,
     &                   gu,gv,gt,gs,guNm1,gvNm1,gtNm1,gsNm1
CcnhDebugStarts
Cdbg &                   ,phSave
CcnhDebugEnds
      _RL  uVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  vVel (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  theta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  salt (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  rho  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  gu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  gv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  gt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  gs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  guNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  gvNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  gtNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      _RL  gsNm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
CcnhDebugStarts
Cdbg  _RL  phSave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
CcnhDebugEnds
