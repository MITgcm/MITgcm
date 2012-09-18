C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_copy_tracer.F,v 1.3 2012/09/18 20:09:17 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_COPY_TRACER
C     !INTERFACE:

      SUBROUTINE OBCS_COPY_TRACER(
     U                             trFld,
     I                             kSiz, bi, bj, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_COPY_TRACER
C     | Copy tracer value from the OB location to narrow band
C     |   region (width = same as overlap) beyond the OB.
C     *==========================================================*
C     | Note: need this copy after an EXCH when:
C     |  1) a several grid-point stencil scheme (e.g., high order
C     |     advection scheme) is used for tracer.
C     |  2) OB is so close to tile edge (e.g., OB_Iw=1) that, to
C     |     compute fluxes at the OB, a high-order scheme will
C     |     use grid points from the overlap region.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS_GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    trFld    :: tracer-field array which OBC applies to.
C    kSiz     :: 3rd dim of tracer-field array "trFld"
C    bi, bj   :: indices of current tile
C    myThid   :: my Thread Id number
      INTEGER kSiz
      _RL     trFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSiz)
      INTEGER bi, bj
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i, j, k
      INTEGER Iobc, Jobc

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
      IF ( tileHasOBN(bi,bj) ) THEN
C Northern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Jn(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = 1,kSiz
            DO j = Jobc+1, Jobc+OLy
             trFld(i,j,k) = trFld(i,Jobc,k)
            ENDDO
           ENDDO
          ENDIF
         ENDDO
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      IF ( tileHasOBS(bi,bj) ) THEN
C Southern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Js(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = 1,kSiz
            DO j = Jobc-OLy, Jobc-1
             trFld(i,j,k) = trFld(i,Jobc,k)
            ENDDO
           ENDDO
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
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = 1,kSiz
            DO i = Iobc+1, Iobc+OLx
             trFld(i,j,k) = trFld(Iobc,j,k)
            ENDDO
           ENDDO
          ENDIF
         ENDDO
      ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      IF ( tileHasOBW(bi,bj) ) THEN
C Western boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Iw(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = 1,kSiz
            DO i = Iobc-OLx, Iobc-1
             trFld(i,j,k) = trFld(Iobc,j,k)
            ENDDO
           ENDDO
          ENDIF
         ENDDO
      ENDIF
#endif /* ALLOW_OBCS_WEST */

#endif /* ALLOW_OBCS */

      RETURN
      END
