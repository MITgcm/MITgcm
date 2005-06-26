C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_fill_field.F,v 1.2 2005/06/26 16:51:49 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGNOSTICS_FILL_FIELD
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_FILL_FIELD( inpFld, ndiagnum, ipointer,
     I                   kLev, nLevs, bibjFlg, biArg, bjArg, myThid )

C     !DESCRIPTION:
C***********************************************************************
C   routine to increment the diagnostics array with a field
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
C     inpFld ..... Field to increment diagnostics array
C     ndiagnum ... Diagnostics Number (in available diag list) of diag to process
C     ipointer ... Pointer to the slot in qdiag to fill
C     kLev   ..... Integer flag for vertical levels:
C                  > 0 (any integer): WHICH single level to increment in qdiag.
C                  0,-1 to increment "nLevs" levels in qdiag,
C                  0 : fill-in in the same order as the input array
C                  -1: fill-in in reverse order.
C     nLevs ...... indicates Number of levels of the input field array
C                  (whether to fill-in all the levels (kLev<1) or just one (kLev>0))
C     bibjFlg .... Integer flag to indicate instructions for bi bj loop
C                  0 indicates that the bi-bj loop must be done here
C                  1 indicates that the bi-bj loop is done OUTSIDE
C                  2 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array (with overlap regions)
C                  3 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array
C                     AND that the array has no overlap region (interior only)
C                  NOTE - bibjFlg can be NEGATIVE to indicate not to increment counter
C     biArg ...... X-direction tile number - used for bibjFlg=1-3
C     bjArg ...... Y-direction tile number - used for bibjFlg=1-3
C     myThid     ::  my thread Id number
C***********************************************************************
C                  NOTE: User beware! If a local (1 tile only) array
C                        is sent here, bibjFlg MUST NOT be set to 0
C                        or there will be out of bounds problems!
C***********************************************************************
      _RL inpFld(*)
      INTEGER ndiagnum, ipointer
      INTEGER kLev, nLevs, bibjFlg, biArg, bjArg
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C ===============
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER sizTx,sizTy
      INTEGER iRun, jRun, k, bi, bj
      INTEGER kFirst, kLast
      INTEGER kd, kd0, ksgn, kStore
      CHARACTER*8 parms1
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C If-sequence to see if we are a valid and an active diagnostic
c     IF ( ndiagnum.NE.0 .AND. ipointer.NE.0 ) THEN

       IF ( bibjFlg.GE.0 .AND. ABS(kLev).LE.1 ) THEN
C Increment the counter for the diagnostic
        IF ( bibjFlg.EQ.0 ) THEN
         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
           ndiag(ipointer,bi,bj) = ndiag(ipointer,bi,bj) + 1
          ENDDO
         ENDDO
        ELSE
           bi = MIN(biArg,nSx)
           bj = MIN(bjArg,nSy)
           ndiag(ipointer,bi,bj) = ndiag(ipointer,bi,bj) + 1
        ENDIF
       ENDIF

C-      select range for 1rst & 2nd indices to accumulate
C         depending on variable location on C-grid,
        parms1 = gdiag(ndiagnum)(1:8)
        IF ( parms1(2:2).EQ.'M' ) THEN
         iRun = sNx
         jRun = sNy
        ELSEIF ( parms1(2:2).EQ.'U' ) THEN
         iRun = sNx+1
         jRun = sNy
        ELSEIF ( parms1(2:2).EQ.'V' ) THEN
         iRun = sNx
         jRun = sNy+1
        ELSEIF ( parms1(2:2).EQ.'Z' ) THEN
         iRun = sNx+1
         jRun = sNy+1
        ELSE
         iRun = sNx
         jRun = sNy
        ENDIF

C-      Dimension of the input array:
        IF (abs(bibjFlg).EQ.3) THEN
          sizI1 = 1
          sizI2 = sNx
          sizJ1 = 1
          sizJ2 = sNy
          iRun = sNx
          jRun = sNy
        ELSE
          sizI1 = 1-OLx
          sizI2 = sNx+OLx
          sizJ1 = 1-OLy
          sizJ2 = sNy+OLy
        ENDIF
        IF (abs(bibjFlg).GE.2) THEN
         sizTx = 1
         sizTy = 1
        ELSE
         sizTx = nSx
         sizTy = nSy
        ENDIF
C-      Which part of inpFld to add : k = 3rd index,
C         and do the loop >> do k=kFirst,kLast <<
        IF (kLev.LE.0) THEN
          kFirst = 1
          kLast  = nLevs
        ELSEIF ( nLevs.EQ.1 ) THEN
          kFirst = 1
          kLast  = 1
        ELSEIF ( kLev.LE.nLevs ) THEN
          kFirst = kLev
          kLast  = kLev
        ELSE
          STOP 'ABNORMAL END in DIAGNOSTICS_FILL_FIELD: kLev > nLevs >0'
        ENDIF
C-      Which part of qdiag to update: kd = 3rd index,
C         and do the loop >> do k=kFirst,kLast ; kd = kd0 + k*ksgn <<
        IF ( kLev.EQ.-1 ) THEN
          ksgn = -1
          kd0 = ipointer + nLevs
        ELSEIF ( kLev.EQ.0 ) THEN
          ksgn = 1
          kd0 = ipointer - 1
        ELSE
          ksgn = 0
          kd0 = ipointer + kLev - 1
        ENDIF

C-      Check for consistency with Nb of levels reserved in storage array
        kStore = kd0 + MAX(ksgn*kFirst,ksgn*kLast) - ipointer + 1
        IF ( kStore.GT.kdiag(ndiagnum) ) THEN
         _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(2A,I3,A)') 'DIAGNOSTICS_FILL_FIELD: ',
     &     'exceed Nb of levels(=',kdiag(ndiagnum),' ) reserved '
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_FILL_FIELD: ',
     &     'for Diagnostics #', ndiagnum, ' : ', cdiag(ndiagnum)
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(2A,2I4,I3)') 'calling DIAGNOSTICS_FILL_FIELD ',
     I     'with kLev,nLevs,bibjFlg=', kLev,nLevs,bibjFlg
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(2A,I6,A)') 'DIAGNOSTICS_FILL_FIELD: ',
     I     '==> trying to store up to ', kStore, ' levels'
          CALL PRINT_ERROR( msgBuf , myThid )
          STOP 'ABNORMAL END: S/R DIAGNOSTICS_FILL_FIELD'
         _END_MASTER(myThid)
        ENDIF

        IF ( bibjFlg.EQ.0 ) THEN

         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
           DO k = kFirst,kLast
            kd = kd0 + ksgn*k
            CALL DIAGNOSTICS_DO_FILL(
     U                  qdiag(1-OLx,1-OLy,kd,bi,bj),
     I                  inpFld,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  myThid)
           ENDDO
          ENDDO
         ENDDO
        ELSE
          bi = MIN(biArg,sizTx)
          bj = MIN(bjArg,sizTy)
          DO k = kFirst,kLast
            kd = kd0 + ksgn*k
            CALL DIAGNOSTICS_DO_FILL(
     U                  qdiag(1-OLx,1-OLy,kd,biArg,bjArg),
     I                  inpFld,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  myThid)
          ENDDO
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     ELSE
c     IF (myThid.EQ.1) WRITE(6,1000) cdiag(ndiagnum)

c     ENDIF

 1000 format(' ',' Warning: Trying to write to diagnostic ',a8,
     &        ' But it is not a valid (or active) name ')
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: DIAGNOSTICS_DO_FILL
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_DO_FILL(
     U                  cumFld,
     I                  inpFld,
     I                  sizI1,sizI2,sizJ1,sizJ2,sizK,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  myThid)

C     !DESCRIPTION:
C     Update array cumFld
C     by adding content of input field array inpFld
C     over the range [1:iRun],[1:jRun]

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     cumFld      :: cumulative array (updated)
C     inpFld      :: input field array to add to cumFld
C     sizI1,sizI2 :: size of inpFld array: 1rst index range (min,max)
C     sizJ1,sizJ2 :: size of inpFld array: 2nd  index range (min,max)
C     sizK        :: size of inpFld array: 3rd  dimension
C     sizTx,sizTy :: size of inpFld array: tile dimensions
C     iRun,jRun   :: range of 1rst & 2nd index
C     k,bi,bj     :: level and tile indices of inFld array
C                    to add to cumFld array
C     myThid      :: my Thread Id number
      _RL cumFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER sizK,sizTx,sizTy
      _RL inpFld(sizI1:sizI2,sizJ1:sizJ2,sizK,sizTx,sizTy)
      INTEGER iRun, jRun, k, bi, bj
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j    :: loop indices
      INTEGER i, j

      DO j = 1,jRun
       DO i = 1,iRun
C- jmc: try with fixed ranges, that are known at compiling stage
C        (might produce a better cash optimisation ?)
c     DO j = 1,sNy
c      DO i = 1,sNx
        cumFld(i,j) = cumFld(i,j) + inpFld(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
