C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_uv.F,v 1.8 2012/09/18 20:09:17 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_APPLY_UV
C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_UV( bi, bj, kArg,
     U                          uFld, vFld,
     I                          myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_UV
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
C    kArg     :: index of current level which OBC applies to
C                or, if zero, apply to all levels
C    uFld     :: horizontal velocity field, 1rst component (zonal)
C    vFld     :: horizontal velocity field, 2nd  component (meridional)
C    myThid   :: my Thread Id number
c     INTEGER biArg, bjArg
      INTEGER bi, bj
      INTEGER kArg
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS

C     !LOCAL VARIABLES:
C     == Local variables ==
c     INTEGER bi, bj, itLo, itHi, jtLo, jtHi
      INTEGER k, kLo, kHi
      INTEGER i, j
      INTEGER Iobc, Jobc

C--   Set model variables to OB values on North/South Boundaries:
C     2 steps: 1) set tangential component ; 2) set normal component.
C     This ensures that the normal component is set correctly even
C     when it conficts with tangential setting from an other OB.

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

C--   Set Tangential component first:

#ifdef ALLOW_OBCS_NORTH
        IF ( tileHasOBN(bi,bj) ) THEN
C Northern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Jn(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             uFld(i,Jobc,k,bi,bj) = OBNu(i,k,bi,bj)
     &                            *_maskW(i,Jobc,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif
#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) ) THEN
C Southern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Js(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             uFld(i,Jobc,k,bi,bj) = OBSu(i,k,bi,bj)
     &                            *_maskW(i,Jobc,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) ) THEN
C Eastern boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Ie(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             vFld(Iobc,j,k,bi,bj) = OBEv(j,k,bi,bj)
     &                            *_maskS(Iobc,j,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif
#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) ) THEN
C Western boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Iw(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             vFld(Iobc,j,k,bi,bj) = OBWv(j,k,bi,bj)
     &                            *_maskS(Iobc,j,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif

C--   Then set Normal component:

#ifdef ALLOW_OBCS_NORTH
        IF ( tileHasOBN(bi,bj) ) THEN
C Northern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Jn(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             vFld(i,Jobc,k,bi,bj)   = OBNv(i,k,bi,bj)
     &                              *_maskS(i,Jobc,k,bi,bj)
             vFld(i,Jobc+1,k,bi,bj) = OBNv(i,k,bi,bj)
     &                              *_maskS(i,Jobc,k,bi,bj)
     &                              *OBCS_uvApplyFac
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif
#ifdef ALLOW_OBCS_SOUTH
        IF ( tileHasOBS(bi,bj) ) THEN
C Southern boundary
         DO i=1-OLx,sNx+OLx
          Jobc = OB_Js(i,bi,bj)
          IF ( Jobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             vFld(i,Jobc+1,k,bi,bj) = OBSv(i,k,bi,bj)
     &                              *_maskS(i,Jobc+1,k,bi,bj)
             vFld(i,Jobc,k,bi,bj)   = OBSv(i,k,bi,bj)
     &                              *_maskS(i,Jobc+1,k,bi,bj)
     &                              *OBCS_uvApplyFac
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif

C     Set model variables to OB values on East/West Boundaries
#ifdef ALLOW_OBCS_EAST
        IF ( tileHasOBE(bi,bj) ) THEN
C Eastern boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Ie(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             uFld(Iobc,j,k,bi,bj)   = OBEu(j,k,bi,bj)
     &                              *_maskW(Iobc,j,k,bi,bj)
             uFld(Iobc+1,j,k,bi,bj) = OBEu(j,K,bi,bj)
     &                              *_maskW(Iobc,j,k,bi,bj)
     &                              *OBCS_uvApplyFac
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif
#ifdef ALLOW_OBCS_WEST
        IF ( tileHasOBW(bi,bj) ) THEN
C Western boundary
         DO j=1-OLy,sNy+OLy
          Iobc = OB_Iw(j,bi,bj)
          IF ( Iobc.NE.OB_indexNone ) THEN
           DO k = kLo,kHi
             uFld(Iobc+1,j,k,bi,bj) = OBWu(j,k,bi,bj)
     &                              *_maskW(Iobc+1,j,k,bi,bj)
             uFld(Iobc,j,k,bi,bj)   = OBWu(j,k,bi,bj)
     &                              *_maskW(Iobc+1,j,k,bi,bj)
     &                              *OBCS_uvApplyFac
           ENDDO
          ENDIF
         ENDDO
        ENDIF
#endif

c      ENDDO
c     ENDDO

#endif /* ALLOW_OBCS */

      RETURN
      END
