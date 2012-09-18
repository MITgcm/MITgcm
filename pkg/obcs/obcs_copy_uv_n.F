C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_copy_uv_n.F,v 1.2 2012/09/18 20:09:17 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_COPY_UV_N
C     !INTERFACE:

      SUBROUTINE OBCS_COPY_UV_N(
     U                             uFld, vFld,
     I                             kSiz, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R OBCS_COPY_UV_N
C     | Copy normal value from the OB location to narrow band
C     |   region (width = same as overlap) beyond the OB.
C     *==========================================================*
C     | Note: need to copy at the beginning of the dynamics loop
C     | only when Stevens boundary conditions are used
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     u/vFld  :: u and v-components which OBC applies to.
C     kSiz    :: 3rd dim of tracer-field array "trFld"
C     bi, bj  :: indices of current tile
C     myThid  :: my Thread Id number
      INTEGER kSiz
      _RL     uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSiz)
      _RL     vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSiz)
      INTEGER bi, bj
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS_STEVENS

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i, j, k
      INTEGER Iobc, Jobc

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
      IF ( tileHasOBN(bi,bj) .AND. useStevensNorth ) THEN
C     Northern boundary
       DO i=1-OLx,sNx+OLx
        Jobc = OB_Jn(i,bi,bj)
        IF ( Jobc.NE.OB_indexNone ) THEN
         DO k = 1, kSiz
          DO j = Jobc+1, Jobc+OLy
           vFld(i,j,k) = vFld(i,Jobc,k)
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      IF ( tileHasOBS(bi,bj) .AND. useStevensSouth ) THEN
C     Southern boundary
       DO i=1-OLx,sNx+OLx
        Jobc = OB_Js(i,bi,bj)
        IF ( Jobc.NE.OB_indexNone ) THEN
         DO k = 1, kSiz
          DO j = Jobc-OLy, Jobc
           vFld(i,j,k) = vFld(i,Jobc+1,k)
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
      IF ( tileHasOBE(bi,bj) .AND. useStevensEast ) THEN
C     Eastern boundary
       DO j=1-OLy,sNy+OLy
        Iobc = OB_Ie(j,bi,bj)
        IF ( Iobc.NE.OB_indexNone ) THEN
         DO k = 1, kSiz
          DO i = Iobc+1, Iobc+OLx
           uFld(i,j,k) = uFld(Iobc,j,k)
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      IF ( tileHasOBW(bi,bj) .AND. useStevensWest ) THEN
C     Western boundary
       DO j=1-OLy,sNy+OLy
        Iobc = OB_Iw(j,bi,bj)
        IF ( Iobc.NE.OB_indexNone ) THEN
         DO k = 1, kSiz
          DO i = Iobc-OLx, Iobc
           uFld(i,j,k) = uFld(Iobc+1,j,k)
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_WEST */

#endif /* ALLOW_OBCS_STEVENS */

      RETURN
      END
