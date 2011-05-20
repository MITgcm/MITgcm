C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_ts.F,v 1.6 2011/05/20 16:57:03 jmc Exp $
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
#include "OBCS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    bi, bj   :: indices of current tile
C    kArg     :: index of current level which OBC apply to
C                or if zeros, apply to all levels
C    tFld     :: temperature field
C    sFld     :: salinity field
C    myThid   :: my Thread Id number
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
         DO i=1-Olx,sNx+Olx
          Jobc = OB_Jn(i,bi,bj)
          IF (Jobc.NE.0) THEN
           DO k = kLo,kHi
            tFld(i,Jobc,k,bi,bj) = OBNt(i,k,bi,bj)
            sFld(i,Jobc,k,bi,bj) = OBNs(i,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) ) THEN
C Southern boundary
         DO i=1-Olx,sNx+Olx
          Jobc = OB_Js(i,bi,bj)
          IF (Jobc.NE.0) THEN
           DO k = kLo,kHi
            tFld(i,Jobc,k,bi,bj) = OBSt(i,k,bi,bj)
            sFld(i,Jobc,k,bi,bj) = OBSs(i,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) ) THEN
C Eastern boundary
         DO j=1-Oly,sNy+Oly
          Iobc = OB_Ie(j,bi,bj)
          IF (Iobc.NE.0) THEN
           DO k = kLo,kHi
            tFld(Iobc,j,k,bi,bj) = OBEt(j,k,bi,bj)
            sFld(Iobc,j,k,bi,bj) = OBEs(j,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) ) THEN
C Western boundary
         DO j=1-Oly,sNy+Oly
          Iobc = OB_Iw(j,bi,bj)
          IF (Iobc.NE.0) THEN
           DO k = kLo,kHi
            tFld(Iobc,j,k,bi,bj) = OBWt(j,k,bi,bj)
            sFld(Iobc,j,k,bi,bj) = OBWs(j,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS_WEST */

c      ENDDO
c     ENDDO

      RETURN
      END
