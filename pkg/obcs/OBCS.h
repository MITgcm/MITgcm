C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/OBCS.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Name:  $

#ifdef ALLOW_OBCS

C--   Private logical flag to record active status of package
      LOGICAL OBCSisON
      COMMON /OBCS_PACKAGE/ OBCSisON

C These are input arrays (of integers) that contain the *absolute*
C computational index of an open-boundary (OB) point.
C A zero (0) element means there is no corresponding OB in that column/row.
C The computational coordinate refers to "tracer" cells.
C For a northern/southern OB, the OB V point is to the south/north.
C For an eastern/western OB, the OB U point is to the west/east.
C eg.
C     OB_Jnorth(3)=34  means that:
C          T( 3 ,34) is a an OB point
C          U(3:4,34) is a an OB point
C          V( 4 ,34) is a an OB point
C while
C     OB_Jsouth(3)=1  means that:
C          T( 3 ,1) is a an OB point
C          U(3:4,1) is a an OB point
C          V( 4 ,2) is a an OB point
C
C For convenience, negative values for Jnorth/Ieast refer to
C points relative to the Northern/Eastern edges of the model
C eg. OB_Jnorth(3)=-1  means that the point (3,Ny) is a northern O-B.
C
      COMMON /PARM_OB/
     & OB_Jnorth,OB_Jsouth,OB_Ieast,OB_Iwest,
     & useOrlanskiNorth,useOrlanskiSouth,
     & useOrlanskiEast,useOrlanskiWest
      INTEGER OB_Jnorth(Nx)
      INTEGER OB_Jsouth(Nx)
      INTEGER OB_Ieast(Ny)
      INTEGER OB_Iwest(Ny)
      LOGICAL useOrlanskiNorth
      LOGICAL useOrlanskiSouth
      LOGICAL useOrlanskiEast
      LOGICAL useOrlanskiWest

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

#endif /* ALLOW_OBCS */
