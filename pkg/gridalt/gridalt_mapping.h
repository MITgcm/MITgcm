C $Header: /u/gcmpack/MITgcm/pkg/gridalt/gridalt_mapping.h,v 1.3 2004/05/05 00:39:21 edhill Exp $
C $Name:  $

c Alternate grid Mapping Common
c ------------------------------
      common /gridalt_mapping/ nlperdyn,dpphys0,dpphys,
     .                                             dxfalt,dyfalt,drfalt
      integer nlperdyn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,Nsy)
      _RL dpphys0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL dpphys(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL dxfalt,dyfalt,drfalt
