C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_GRID.h,v 1.5 2014/11/25 01:07:23 jmc Exp $
C $Name:  $

#ifdef ALLOW_OBCS

CBOP
C     !ROUTINE: OBCS_GRID.h
C     !INTERFACE:
C     #include "OBCS_GRID.h"

C     !DESCRIPTION:
C     *==========================================================*
C     | OBCS_GRID.h
C     | o Header file containing OBCS location on the grid
C     *==========================================================*
C     | o Note: does not (and should not) contain any conditional
C     |   statement that depends on OBCS options ; therefore
C     |   can be safely included without OBCS_OPTIONS.h
C     *==========================================================*
CEOP

C tileHasOB[N,S,E,W] :: this tile has OB at Northern/Southern/Eastern/Western edge
C   OB_Jn, OB_Js     :: indices of Northern & Southern OB location (cell center pt)
C   OB_Ie, OB_Iw     :: indices of  Eastern & Western  OB location (cell center pt)
C   OB_indexNone     :: null value for OB index (if row or column without OB)
C OBCS_indexStatus   :: track status of OB indices setting
C OBCS_insideMask    :: Inside OB region mask (zero beyond OB).
C   OB_connectNumber :: total number of connected parts for this level
C   OBN_connect      :: domain connected piece Id of this Northern OB grid point
C   OBS_connect      :: domain connected piece Id of this Southern OB grid point
C   OBE_connect      :: domain connected piece Id of this Eastern OB grid point
C   OBW_connect      :: domain connected piece Id of this Western OB grid point

      COMMON /OBCS_ACTIVE_TILES/
     &  tileHasOBN, tileHasOBS, tileHasOBE, tileHasOBW
      LOGICAL tileHasOBN(nSx,nSy)
      LOGICAL tileHasOBS(nSx,nSy)
      LOGICAL tileHasOBE(nSx,nSy)
      LOGICAL tileHasOBW(nSx,nSy)

      COMMON /OBCS_GRID_INDICES/
     &  OB_Jn, OB_Js, OB_Ie, OB_Iw,
     &  OB_indexNone, OBCS_indexStatus
      INTEGER OB_Jn(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER OB_Js(1-OLx:sNx+OLx,nSx,nSy)
      INTEGER OB_Ie(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER OB_Iw(1-OLy:sNy+OLy,nSx,nSy)
      INTEGER OB_indexNone
      INTEGER OBCS_indexStatus

      COMMON /OBCS_MASK_RS/
     &  OBCS_insideMask
      _RS OBCS_insideMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /OBCS_CONNECTED_ID/
     &  OB_connectNumber,
     &  OBN_connect, OBS_connect, OBE_connect, OBW_connect
      INTEGER OB_connectNumber(Nr)
      INTEGER OBN_connect(1:sNx,Nr,nSx,nSy)
      INTEGER OBS_connect(1:sNx,Nr,nSx,nSy)
      INTEGER OBE_connect(1:sNy,Nr,nSx,nSy)
      INTEGER OBW_connect(1:sNy,Nr,nSx,nSy)

#endif /* ALLOW_OBCS */
