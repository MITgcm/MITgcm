C $Header: /u/gcmpack/MITgcm/model/inc/Attic/OBCS.h,v 1.4 1999/08/30 18:29:21 adcroft Exp $
C

#ifdef ALLOW_OBCS
C--   COMMON /GRID_OB/ Open boudary related stuff
C     OBNu is the U value imposed at the Northern OB
C     OBNv is the V value imposed at the Northern OB
C     OBNt is the T value imposed at the Northern OB
C     OBNu is the S value imposed at the Northern OB
C     etc
C
      COMMON /GRID_OB/ 
     &      OBNu,OBNv,OBNt,OBNs,
     &      OBSu,OBSv,OBSt,OBSs,
     &      OBEu,OBEv,OBEt,OBEs,
     &      OBWu,OBWv,OBWt,OBWs,
     &      OB_Jn,OB_Js,OB_Ie,OB_Iw
      _RS OBNu (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBNv (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBNt (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBNs (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBSu (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBSv (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBSt (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBSs (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBEu (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBEv (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBEt (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBEs (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBWu (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBWv (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBWt (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBWs (1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER OB_Jn(1-Olx:sNx+Olx,nSx,nSy)
      INTEGER OB_Js(1-Olx:sNx+Olx,nSx,nSy)
      INTEGER OB_Ie(1-Oly:sNy+Oly,nSx,nSy)
      INTEGER OB_Iw(1-Oly:sNy+Oly,nSx,nSy)

#ifdef ALLOW_NONHYDROSTATIC
      COMMON /GRID_OBNH/ 
     &  OBNw,OBSw,OBEw,OBWw
      _RS OBNw (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBSw (1-Olx:sNx+Olx,Nr,nSx,nSy)
      _RS OBEw (1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RS OBWw (1-Oly:sNy+Oly,Nr,nSx,nSy)
#endif /* ALLOW_NONHYDROSTATIC */

#endif
