C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_ptracer.F,v 1.4 2010/11/14 23:28:19 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_APPLY_PTRACER
C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_PTRACER(
     I                               bi, bj, kArg, iTracer,
     U                               pFld,
     I                               myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_PTRACER
C     | In this routine the open boundary values are applied
C     | that have been calculated in OBCS_CALC.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    bi, bj   :: indices of current tile
C    kArg     :: index of current level which OBC apply to
C                or if zeros, apply to all levels
C    iTracer  :: index of current passive tracer
C    pFld     :: passive tracer field
C    myThid   :: my Thread Id number
      INTEGER bi, bj
      INTEGER kArg
      INTEGER iTracer
      _RL pFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,*)
      INTEGER myThid
CEOP

#if ( defined ALLOW_OBCS && defined ALLOW_PTRACERS )

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER k, kLo, kHi
      INTEGER kl, kMx
      INTEGER i, j
      INTEGER Iobc, Jobc
      _RL obc_mask

      IF ( kArg.EQ.0 ) THEN
        kLo = 1
        kHi = Nr
        kMx = Nr
      ELSE
        kLo = kArg
        kHi = kArg
        kMx = 1
      ENDIF

C     Set model variables to OB values on North/South Boundaries
#ifdef ALLOW_OBCS_NORTH
      IF ( tileHasOBN(bi,bj) ) THEN
C Northern boundary
       DO i=1-Olx,sNx+Olx
        Jobc = OB_Jn(i,bi,bj)
        IF (Jobc.NE.0) THEN
         DO k = kLo,kHi
          kl = MIN(k,kMx)
          obc_mask = _maskS(i,Jobc,k,bi,bj)
          DO j = Jobc, Jobc+Oly
           pFld(i,j,kl) = OBNptr(i,k,bi,bj,iTracer)*obc_mask
          ENDDO
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
          kl = MIN(k,kMx)
          obc_mask = _maskS(i,Jobc+1,K,bi,bj)
          DO j = Jobc-Oly, Jobc
           pFld(i,j,kl) = OBSptr(i,k,bi,bj,iTracer)*obc_mask
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
       DO j=1-Oly,sNy+Oly
        Iobc = OB_Ie(j,bi,bj)
        IF (Iobc.NE.0) THEN
         DO k = kLo,kHi
          kl = MIN(k,kMx)
          obc_mask = _maskW(Iobc,j,k,bi,bj)
          DO I = Iobc, Iobc+Olx
           pFld(i,j,kl) = OBEptr(j,k,bi,bj,iTracer)*obc_mask
          ENDDO
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
          kl = MIN(k,kMx)
          obc_mask = _maskW(Iobc+1,j,k,bi,bj)
          DO i = Iobc-Olx, Iobc
           pFld(i,j,kl) = OBWptr(j,k,bi,bj,iTracer)*obc_mask
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_WEST */

#endif /* ALLOW_OBCS and ALLOW_PTRACERS */
      RETURN
      END
