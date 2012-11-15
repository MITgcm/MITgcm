C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_ts.F,v 1.11 2012/11/15 15:55:42 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_APPLY_TS
C     !INTERFACE:

      SUBROUTINE OBCS_APPLY_TS( bi, bj, kArg,
     U                          tFld, sFld,
     I                          myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_TS
C     |   Apply OB values to corresponding field array
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi, bj   :: indices of current tile
C     kArg     :: index of current level which OBC apply to
C                 or if zeros, apply to all levels
C     tFld     :: temperature field
C     sFld     :: salinity field
C     myThid   :: my Thread Id number
c     INTEGER biArg, bjArg
      INTEGER bi, bj
      INTEGER kArg
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
c     INTEGER bi, bj, itLo, itHi, jtLo, jtHi
      INTEGER k, kLo, kHi
      INTEGER i, j
      INTEGER Iobc, Jobc

c     IF ( biArg.EQ.0 .OR. bjArg.EQ.0 ) THEN
c       itLo = myBxLo(myThid)
c       itHi = myBxHi(myThid)
c       jtLo = myByLo(myThid)
c       jtHi = myByHi(myThid)
c     ELSE
c       itLo = biArg
c       itHi = biArg
c       jtLo = bjArg
c       jtHi = bjArg
c     ENDIF
      IF ( kArg.EQ.0 ) THEN
        kLo = 1
        kHi = Nr
      ELSE
        kLo = kArg
        kHi = kArg
      ENDIF

c     DO bj = jtLo,jtHi
c      DO bi = itLo,itHi

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
        IF ( tileHasOBN(bi,bj) ) THEN
C Northern boundary
# ifdef ALLOW_OBCS_STEVENS
         IF ( useStevensNorth ) THEN
          DO i=1-OLx,sNx+OLx
C     add tendency term instead of overwriting field with boundary value
           Jobc = OB_Jn(i,bi,bj)
           IF ( Jobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(i,Jobc,k,bi,bj) = tFld(i,Jobc,k,bi,bj)
     &            + dTtracerLev(k)*OBNt(i,k,bi,bj)
             sFld(i,Jobc,k,bi,bj) = sFld(i,Jobc,k,bi,bj)
     &            + dTtracerLev(k)*OBNs(i,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ELSE
# else
         IF ( .TRUE. ) THEN
# endif /* ALLOW_OBCS_STEVENS */
          DO i=1-OLx,sNx+OLx
           Jobc = OB_Jn(i,bi,bj)
           IF ( Jobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(i,Jobc,k,bi,bj) = OBNt(i,k,bi,bj)
             sFld(i,Jobc,k,bi,bj) = OBNs(i,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ENDIF
        ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) ) THEN
C Southern boundary
# ifdef ALLOW_OBCS_STEVENS
         IF ( useStevensSouth ) THEN
C     add tendency term instead of overwriting field with boundary value
          DO i=1-OLx,sNx+OLx
           Jobc = OB_Js(i,bi,bj)
           IF ( Jobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(i,Jobc,k,bi,bj) = tFld(i,Jobc,k,bi,bj)
     &            + dTtracerLev(k)*OBSt(i,k,bi,bj)
             sFld(i,Jobc,k,bi,bj) = sFld(i,Jobc,k,bi,bj)
     &            + dTtracerLev(k)*OBSs(i,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ELSE
# else
         IF ( .TRUE. ) THEN
# endif /* ALLOW_OBCS_STEVENS */
          DO i=1-OLx,sNx+OLx
           Jobc = OB_Js(i,bi,bj)
           IF ( Jobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(i,Jobc,k,bi,bj) = OBSt(i,k,bi,bj)
             sFld(i,Jobc,k,bi,bj) = OBSs(i,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ENDIF
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) ) THEN
C Eastern boundary
# ifdef ALLOW_OBCS_STEVENS
         IF ( useStevensEast ) THEN
C     add tendency term instead of overwriting field with boundary value
          DO j=1-OLy,sNy+OLy
           Iobc = OB_Ie(j,bi,bj)
           IF ( Iobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(Iobc,j,k,bi,bj) = tFld(Iobc,j,k,bi,bj)
     &            + dTtracerLev(k)*OBEt(j,k,bi,bj)
             sFld(Iobc,j,k,bi,bj) = sFld(Iobc,j,k,bi,bj)
     &            + dTtracerLev(k)*OBEs(j,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ELSE
# else
         IF ( .TRUE. ) THEN
# endif /* ALLOW_OBCS_STEVENS */
          DO j=1-OLy,sNy+OLy
           Iobc = OB_Ie(j,bi,bj)
           IF ( Iobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(Iobc,j,k,bi,bj) = OBEt(j,k,bi,bj)
             sFld(Iobc,j,k,bi,bj) = OBEs(j,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ENDIF
        ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) ) THEN
C Western boundary
# ifdef ALLOW_OBCS_STEVENS
         IF ( useStevensWest ) THEN
C     add tendency term instead of overwriting field with boundary value
          DO j=1-OLy,sNy+OLy
           Iobc = OB_Iw(j,bi,bj)
           IF ( Iobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(Iobc,j,k,bi,bj) = tFld(Iobc,j,k,bi,bj)
     &            + dTtracerLev(k)*OBWt(j,k,bi,bj)
             sFld(Iobc,j,k,bi,bj) = sFld(Iobc,j,k,bi,bj)
     &            + dTtracerLev(k)*OBWs(j,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ELSE
# else
         IF ( .TRUE. ) THEN
# endif /* ALLOW_OBCS_STEVENS */
          DO j=1-OLy,sNy+OLy
           Iobc = OB_Iw(j,bi,bj)
           IF ( Iobc.NE.OB_indexNone ) THEN
            DO k = kLo,kHi
             tFld(Iobc,j,k,bi,bj) = OBWt(j,k,bi,bj)
             sFld(Iobc,j,k,bi,bj) = OBWs(j,k,bi,bj)
            ENDDO
           ENDIF
          ENDDO
         ENDIF
        ENDIF
#endif /* ALLOW_OBCS_WEST */

c      ENDDO
c     ENDDO

      RETURN
      END
