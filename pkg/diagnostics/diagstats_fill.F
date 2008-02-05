C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_fill.F,v 1.4 2008/02/05 15:31:19 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_FILL
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_FILL(
     I               inpFld, fractFld, scaleFact, power, nLevFract,
     I               ndId, kInQSd, region2fill, kLev, nLevs,
     I               bibjflg, biArg, bjArg, myThid )

C     !DESCRIPTION:
C***********************************************************************
C   compute statistics over 1 tile
C   and increment the diagnostics array
C     using a scaling factor & square option (power=2),
C     and with the option to use a fraction-weight (assumed
C         to be the counter-mate of the current diagnostics)
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
C     inpFld    :: Field to increment diagnostics array
C     fractFld  :: fraction used for weighted average diagnostics
C     scaleFact :: scaling factor
C     power     :: option to fill-in with the field square (power=2)
C     nLevFract :: number of levels of the fraction field, =0 : do not use fraction
C     ndId      :: Diagnostics Id Number (in available diag list) of diag to process
C     kInQSd    :: Pointer to the slot in qSdiag to fill
C   region2fill :: array, indicates whether to compute statistics over region
C                   "j" (if region2fill(j)=1) or not (if region2fill(j)=0)
C     kLev      :: Integer flag for vertical levels:
C                  > 0 (any integer): WHICH single level to increment in qSdiag.
C                  0,-1 to increment "nLevs" levels in qSdiag,
C                  0 : fill-in in the same order as the input array
C                  -1: fill-in in reverse order.
C     nLevs     :: indicates Number of levels of the input field array
C                  (whether to fill-in all the levels (kLev<1) or just one (kLev>0))
C     bibjflg   :: Integer flag to indicate instructions for bi bj loop
C                  0 indicates that the bi-bj loop must be done here
C                  1 indicates that the bi-bj loop is done OUTSIDE
C                  2 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array (with overlap regions)
C                  3 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array
C                     AND that the array has no overlap region (interior only)
C                  NOTE - bibjflg can be NEGATIVE to indicate not to increment counter
C     biArg     :: X-direction tile number - used for bibjflg=1-3
C     bjArg     :: Y-direction tile number - used for bibjflg=1-3
C     myThid    :: my thread Id number
C***********************************************************************
C                  NOTE: User beware! If a local (1 tile only) array
C                        is sent here, bibjflg MUST NOT be set to 0
C                        or there will be out of bounds problems!
C***********************************************************************
      _RL inpFld(*)
      _RL fractFld(*)
      _RL scaleFact
      INTEGER power
      INTEGER nLevFract
      INTEGER ndId, kInQSd
      INTEGER region2fill(0:nRegions)
      INTEGER kLev, nLevs, bibjflg, biArg, bjArg
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C ===============
C     useFract  :: flag to increment (or not) with fraction-weigted inpFld
      LOGICAL useFract
      INTEGER sizF
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER sizTx,sizTy
      INTEGER iRun, jRun, k, bi, bj
      INTEGER kFirst, kLast
      INTEGER kd, kd0, ksgn, kStore
      CHARACTER*8 parms1
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER km, km0

C If-sequence to see if we are a valid and an active diagnostic
c     IF ( ndId.NE.0 .AND. kInQSd.NE.0 ) THEN

C-      select range for 1rst & 2nd indices to accumulate
C         depending on variable location on C-grid,
        parms1 = gdiag(ndId)(1:8)
        IF ( parms1(2:2).EQ.'Z' ) THEN
         iRun = sNx+1
         jRun = sNy+1
c       ELSEIF ( parms1(2:2).EQ.'U' ) THEN
c        iRun = sNx+1
c        jRun = sNy
c       ELSEIF ( parms1(2:2).EQ.'V' ) THEN
c        iRun = sNx
c        jRun = sNy+1
        ELSE
         iRun = sNx
         jRun = sNy
        ENDIF

C-      Dimension of the input array:
        IF (ABS(bibjflg).EQ.3) THEN
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
        IF (ABS(bibjflg).GE.2) THEN
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
          STOP 'ABNORMAL END in DIAGSTATS_FILL: kLev > nLevs > 0'
        ENDIF
C-      Which part of qSdiag to update: kd = 3rd index,
C         and do the loop >> do k=kFirst,kLast ; kd = kd0 + k*ksgn <<
C  1rst try this: for the mask: km = km0 + k*ksgn so that kd= km + kInQSd - 1
        IF ( kLev.EQ.-1 ) THEN
          ksgn = -1
          kd0 = kInQSd + nLevs
          km0 = 1 + nLevs
        ELSEIF ( kLev.EQ.0 ) THEN
          ksgn = 1
          kd0 = kInQSd - 1
          km0 = 0
        ELSE
          ksgn = 0
          kd0 = kInQSd + kLev - 1
          km0 = kLev
        ENDIF
C-      Set fraction-weight option :
        useFract = nLevFract.GT.0
        IF ( useFract ) THEN
          sizF = nLevFract
        ELSE
          sizF = 1
        ENDIF

C-      Check for consistency with Nb of levels reserved in storage array
        kStore = kd0 + MAX(ksgn*kFirst,ksgn*kLast) - kInQSd + 1
        IF ( kStore.GT.kdiag(ndId) ) THEN
         _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(2A,I4,A)') 'DIAGSTATS_FILL: ',
     &     'exceed Nb of levels(=',kdiag(ndId),' ) reserved '
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(2A,I6,2A)') 'DIAGSTATS_FILL: ',
     &     'for Diagnostics #', ndId, ' : ', cdiag(ndId)
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(2A,2I4,I3)') 'calling DIAGSTATS_FILL ',
     I     'with kLev,nLevs,bibjFlg=', kLev,nLevs,bibjFlg
          CALL PRINT_ERROR( msgBuf , myThid )
          WRITE(msgBuf,'(2A,I6,A)') 'DIAGSTATS_FILL: ',
     I     '==> trying to store up to ', kStore, ' levels'
          CALL PRINT_ERROR( msgBuf , myThid )
          STOP 'ABNORMAL END: S/R DIAGSTATS_FILL'
         _END_MASTER(myThid)
        ENDIF

        IF ( bibjflg.EQ.0 ) THEN

         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
           DO k = kFirst,kLast
            kd = kd0 + ksgn*k
            km = km0 + ksgn*k
            CALL DIAGSTATS_LOCAL(
     U                  qSdiag(0,0,kd,bi,bj),
     I                  inpFld, fractFld,
     I                  scaleFact, power, useFract, sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  km, bi, bj, region2fill,
     I                  ndId, gdiag(ndId), myThid )
           ENDDO
          ENDDO
         ENDDO
        ELSE
          bi = MIN(biArg,sizTx)
          bj = MIN(bjArg,sizTy)
          DO k = kFirst,kLast
            kd = kd0 + ksgn*k
            km = km0 + ksgn*k
            CALL DIAGSTATS_LOCAL(
     U                  qSdiag(0,0,kd,biArg,bjArg),
     I                  inpFld, fractFld,
     I                  scaleFact, power, useFract, sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  km, biArg, bjArg, region2fill,
     I                  ndId, gdiag(ndId), myThid )
          ENDDO
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     ELSE

c     ENDIF

      RETURN
      END
