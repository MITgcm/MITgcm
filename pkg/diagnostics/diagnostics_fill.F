C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_fill.F,v 1.6 2005/05/19 01:23:39 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGNOSTICS_FILL
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_FILL (inpfld, chardiag, 
     I                kLev, nLevs, bibjflg, biArg, bjArg, myThid) 

C     !DESCRIPTION:
C***********************************************************************
C   Wrapper routine to increment the diagnostics arrays with a field
C***********************************************************************
C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C***********************************************************************
C  Arguments Description
C  ----------------------
C     inpfld ..... Field to increment diagnostics array
C     chardiag ... Character expression for diag to fill
C     kLev   ..... Integer flag for vertical levels:
C                  > 0 (any integer): WHICH single level to increment in qdiag.
C                  0,-1 to increment "nLevs" levels in qdiag,
C                  0 : fill-in in the same order as the input array 
C                  -1: fill-in in reverse order.
C     nLevs ...... indicates Number of levels of the input field array
C                  (whether to fill-in all the levels (kLev<1) or just one (kLev>0))
C     bibjflg .... Integer flag to indicate instructions for bi bj loop
C                  0 indicates that the bi-bj loop must be done here
C                  1 indicates that the bi-bj loop is done OUTSIDE
C                  2 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array (with overlap regions)
C                  3 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array
C                     AND that the array has no overlap region (interior only)
C                  NOTE - bibjflg can be NEGATIVE to indicate not to increment counter
C     biArg ...... X-direction tile number - used for bibjflg=1-3
C     bjArg ...... Y-direction tile number - used for bibjflg=1-3
C     myThid     ::  my thread Id number
C***********************************************************************
C                  NOTE: User beware! If a local (1 tile only) array
C                        is sent here, bibjflg MUST NOT be set to 0
C                        or there will be out of bounds problems!
C***********************************************************************
      _RL inpfld(*)
      CHARACTER*8 chardiag
      INTEGER kLev, nLevs, bibjflg, biArg, bjArg
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C ===============
      INTEGER m, n, j
      INTEGER ndiagnum, ipointer, iSp, jSd
      INTEGER region2fill(0:nRegions)

C--   2D/3D Diagnostics :
C Run through list of active diagnostics to make sure
C we are trying to fill a valid diagnostic

      ndiagnum = 0
      ipointer = 0
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( chardiag.EQ.flds(m,n) ) THEN
         ndiagnum = jdiag(m,n)
         IF (ndiag(ndiagnum).GE.0) ipointer = idiag(ndiagnum)
        ENDIF
       ENDDO
      ENDDO

C if we are a valid and an active diagnostic, do the filling:
      IF ( ipointer.NE.0 ) THEN
        CALL DIAGNOSTICS_FILL_FIELD( inpfld, ndiagnum, ipointer,
     I                kLev, nLevs, bibjflg, biArg, bjArg, myThid ) 
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN 
      END
