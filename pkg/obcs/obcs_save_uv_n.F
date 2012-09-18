C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_save_uv_n.F,v 1.2 2012/09/18 20:09:17 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_SAVE_UV_N
C     !INTERFACE:
      SUBROUTINE OBCS_SAVE_UV_N(  bi, bj, iMin, iMax, jMin, jMax, kArg,
     I                          uFld, vFld,
     I                          myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_SAVE_UV_N
C     | Save normal velocities at the OB location to be used
C     | in the next time step for Stevens boundary conditions
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    bi, bj    :: indices of current tile
C    iMin,iMax :: array boundaries
C    jMin,jMax ::
C    kArg      :: index of current level which OBC applies to
C                 or, if zero, apply to all levels
C    uFld      :: horizontal velocity field, 1rst component (zonal)
C    vFld      :: horizontal velocity field, 2nd  component (meridional)
C    myThid    :: my Thread Id number
      INTEGER bi, bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER kArg
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS_STEVENS

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER k, kLo, kHi
      INTEGER i, j
      INTEGER Iobc, Jobc

C--   Save model velocity normal to OB for next time step.
      IF ( kArg.EQ.0 ) THEN
        kLo = 1
        kHi = Nr
      ELSE
        kLo = kArg
        kHi = kArg
      ENDIF

#ifdef ALLOW_OBCS_NORTH
      IF ( tileHasOBN(bi,bj) .AND. useStevensNorth ) THEN
C Northern boundary
       DO i=iMin,iMax
        Jobc = OB_Jn(i,bi,bj)
        IF ( Jobc.NE.OB_indexNone ) THEN
         DO k = kLo,kHi
          OBNvStevens(i,k,bi,bj) = vFld(i,Jobc,k,bi,bj)
     &         *_maskS(i,Jobc,k,bi,bj)
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif
#ifdef ALLOW_OBCS_SOUTH
      IF ( tileHasOBS(bi,bj) .AND. useStevensSouth ) THEN
C Southern boundary
       DO i=iMin,iMax
        Jobc = OB_Js(i,bi,bj)
        IF ( Jobc.NE.OB_indexNone ) THEN
         DO k = kLo,kHi
          OBSvStevens(i,k,bi,bj) = vFld(i,Jobc+1,k,bi,bj)
     &         *_maskS(i,Jobc+1,k,bi,bj)
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif

#ifdef ALLOW_OBCS_EAST
      IF ( tileHasOBE(bi,bj) .AND. useStevensEast ) THEN
C Eastern boundary
       DO j=jMin,jMax
        Iobc = OB_Ie(j,bi,bj)
        IF ( Iobc.NE.OB_indexNone ) THEN
         DO k = kLo,kHi
          OBEuStevens(j,k,bi,bj) = uFld(Iobc,j,k,bi,bj)
     &         *_maskW(Iobc,j,k,bi,bj)
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif
#ifdef ALLOW_OBCS_WEST
      IF ( tileHasOBW(bi,bj) .AND. useStevensWest ) THEN
C Western boundary
       DO j=jMin,jMax
        Iobc = OB_Iw(j,bi,bj)
        IF ( Iobc.NE.OB_indexNone ) THEN
         DO k = kLo,kHi
          OBWuStevens(j,k,bi,bj) = uFld(Iobc+1,j,k,bi,bj)
     &         *_maskW(Iobc+1,j,k,bi,bj)
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif
#endif /* ALLOW_OBCS_STEVENS */

      RETURN
      END
