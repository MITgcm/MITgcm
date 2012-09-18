C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_ptracer.F,v 1.7 2012/09/18 20:09:17 jmc Exp $
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
#include "OBCS_GRID.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "OBCS_PTRACERS.h"
#endif /* ALLOW_PTRACERS */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C    bi, bj   :: indices of current tile
C    kArg     :: index of current level which OBC apply to
C                or, if zero, apply to all levels
C    iTracer  :: index of current passive tracer
C    pFld     :: passive tracer field
C    myThid   :: my Thread Id number
      INTEGER bi, bj
      INTEGER kArg
      INTEGER iTracer
      _RL pFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,*)
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER k, kLo, kHi
      INTEGER kl, kMx
      INTEGER i, j
      INTEGER Iobc, Jobc

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
       DO i=1-OLx,sNx+OLx
        Jobc = OB_Jn(i,bi,bj)
        IF ( Jobc.NE.OB_indexNone ) THEN
         DO k = kLo,kHi
          kl = MIN(k,kMx)
          pFld(i,Jobc,kl) = OBNptr(i,k,bi,bj,iTracer)
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
         DO k = kLo,kHi
          kl = MIN(k,kMx)
          pFld(i,Jobc,kl) = OBSptr(i,k,bi,bj,iTracer)
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
         DO k = kLo,kHi
          kl = MIN(k,kMx)
          pFld(Iobc,j,kl) = OBEptr(j,k,bi,bj,iTracer)
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
         DO k = kLo,kHi
          kl = MIN(k,kMx)
          pFld(Iobc,j,kl) = OBWptr(j,k,bi,bj,iTracer)
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_WEST */

#endif /* ALLOW_PTRACERS */

      RETURN
      END
