C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_w.F,v 1.10 2012/11/15 15:55:42 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_APPLY_W
C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_W( bi, bj, kArg,
     U                         wFld,
     I                         myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_W
C     |   Apply vertical velocity OB values
C     |   to corresponding field array
C     *==========================================================*

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
C    bi, bj   :: indices of current tile
C    kArg     :: index of current level which OBC apply to
C                or if zero, apply to all levels
C    wFld     :: vertical velocity field
C    myThid   :: my Thread Id number
      INTEGER bi, bj
      INTEGER kArg
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
CEOP

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER kLo, kHi
      INTEGER k, km1
      INTEGER i, j
      INTEGER Iobc, Jobc
      _RL obc_mask

      IF ( nonHydrostatic ) THEN

        IF ( kArg.EQ.0 ) THEN
          kLo = 1
          kHi = Nr
        ELSE
          k   = kArg
          km1 = MAX( k-1, 1 )
        ENDIF

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
        IF ( tileHasOBN(bi,bj) ) THEN
C Northern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Jn(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone .AND. kArg.EQ.0 ) THEN
           DO k = kLo,kHi
            km1 = MAX( k-1, 1 )
            obc_mask = maskC(i,Jobc,km1,bi,bj)*maskC(i,Jobc,k,bi,bj)
            wFld(i,Jobc,k,bi,bj) = OBNw(i,k,bi,bj)*obc_mask
           ENDDO
          ELSEIF ( Jobc.NE.OB_indexNone ) THEN
            obc_mask = maskC(i,Jobc,km1,bi,bj)*maskC(i,Jobc,k,bi,bj)
            wFld(i,Jobc,k,bi,bj) = OBNw(i,k,bi,bj)*obc_mask
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) ) THEN
C Southern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Js(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone .AND. kArg.EQ.0 ) THEN
           DO k = kLo,kHi
            km1 = MAX( k-1, 1 )
            obc_mask = maskC(i,Jobc,km1,bi,bj)*maskC(i,Jobc,k,bi,bj)
            wFld(i,Jobc,k,bi,bj) = OBSw(i,k,bi,bj)*obc_mask
           ENDDO
          ELSEIF ( Jobc.NE.OB_indexNone ) THEN
            obc_mask = maskC(i,Jobc,km1,bi,bj)*maskC(i,Jobc,k,bi,bj)
            wFld(i,Jobc,k,bi,bj) = OBSw(i,k,bi,bj)*obc_mask
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) ) THEN
C Eastern boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Ie(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone .AND. kArg.EQ.0 ) THEN
           DO k = kLo,kHi
            km1 = MAX( k-1, 1 )
            obc_mask = maskC(Iobc,j,km1,bi,bj)*maskC(Iobc,j,k,bi,bj)
            wFld(Iobc,j,k,bi,bj) = OBEw(j,k,bi,bj)*obc_mask
           ENDDO
          ELSEIF ( Iobc.NE.OB_indexNone ) THEN
            obc_mask = maskC(Iobc,j,km1,bi,bj)*maskC(Iobc,j,k,bi,bj)
            wFld(Iobc,j,k,bi,bj) = OBEw(j,k,bi,bj)*obc_mask
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) ) THEN
C Western boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Iw(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone .AND. kArg.EQ.0 ) THEN
           DO k = kLo,kHi
            km1 = MAX( k-1, 1 )
            obc_mask = maskC(Iobc,j,km1,bi,bj)*maskC(Iobc,j,k,bi,bj)
            wFld(Iobc,j,k,bi,bj) = OBWw(j,k,bi,bj)*obc_mask
           ENDDO
          ELSEIF ( Iobc.NE.OB_indexNone ) THEN
            obc_mask = maskC(Iobc,j,km1,bi,bj)*maskC(Iobc,j,k,bi,bj)
            wFld(Iobc,j,k,bi,bj) = OBWw(j,k,bi,bj)*obc_mask
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_WEST */

      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
