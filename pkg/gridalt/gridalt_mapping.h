C $Header: /u/gcmpack/MITgcm/pkg/gridalt/gridalt_mapping.h,v 1.4 2004/06/15 17:19:55 molod Exp $
C $Name:  $

c Alternate grid Mapping Common
c ------------------------------
#ifdef ALLOW_FIZHI
      common /gridalt_mapping/ nlperdyn,dpphys0,dpphys,
     .                                             dxfalt,dyfalt,drfalt
      integer nlperdyn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,Nsy)
      _RL dpphys0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL dpphys(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
      _RL dxfalt,dyfalt,drfalt
#endif
