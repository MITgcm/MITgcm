C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_fill.F,v 1.6 2014/08/08 19:29:48 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C--   File diagstats_fill.F:
C--    Contents:
C--    o DIAGSTATS_FILL
C--    o DIAGSTATS_TO_RL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: DIAGSTATS_FILL
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_FILL(
     I               inpFldRL, fracFldRL,
#ifndef REAL4_IS_SLOW
     I               inpFldRS, fracFldRS,
#endif
     I               scaleFact, power, arrType, nLevFract,
     I               ndId, kInQSd, region2fill, kLev, nLevs,
     I               bibjFlg, biArg, bjArg, myThid )

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
C     inpFldRL  :: Field to increment diagnostics array (arrType=0,1)
C     fracFldRL :: fraction used for weighted average diagnostics (arrType=0,2)
C     inpFldRS  :: Field to increment diagnostics array (arrType=2,3)
C     fracFldRS :: fraction used for weighted average diagnostics (arrType=1,3)
C     scaleFact :: scaling factor
C     power     :: option to fill-in with the field square (power=2)
C     arrType   :: select which array & fraction (RL/RS) to process:
C                  0: both RL ; 1: inpRL & fracRS ; 2: inpRS,fracRL ; 3: both RS
C     nLevFract :: number of levels of the fraction field, =0: do not use fraction
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
C     bibjFlg   :: Integer flag to indicate instructions for bi bj loop
C                  0 indicates that the bi-bj loop must be done here
C                  1 indicates that the bi-bj loop is done OUTSIDE
C                  2 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array (with overlap regions)
C                  3 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array
C                     AND that the array has no overlap region (interior only)
C                  NOTE - bibjFlg can be NEGATIVE to indicate not to increment counter
C     biArg     :: X-direction tile number - used for bibjFlg=1-3
C     bjArg     :: Y-direction tile number - used for bibjFlg=1-3
C     myThid    :: my thread Id number
C***********************************************************************
C                  NOTE: User beware! If a local (1 tile only) array
C                        is sent here, bibjFlg MUST NOT be set to 0
C                        or there will be out of bounds problems!
C***********************************************************************
      _RL inpFldRL(*)
      _RL fracFldRL(*)
#ifndef REAL4_IS_SLOW
      _RS inpFldRS(*)
      _RS fracFldRS(*)
#endif
      _RL scaleFact
      INTEGER power
      INTEGER arrType
      INTEGER nLevFract
      INTEGER ndId, kInQSd
      INTEGER region2fill(0:nRegions)
      INTEGER kLev, nLevs, bibjFlg, biArg, bjArg
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
#ifndef REAL4_IS_SLOW
      _RL tmpFldRL( sNx+1,sNy+1)
      _RL tmpFracRL(sNx+1,sNy+1)
#endif

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
        IF (ABS(bibjFlg).EQ.3) THEN
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
        IF (ABS(bibjFlg).GE.2) THEN
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

#ifndef REAL4_IS_SLOW
      IF ( arrType.EQ.0 .OR. ( arrType.EQ.1 .AND. .NOT.useFract ) ) THEN
#endif
        IF ( bibjFlg.EQ.0 ) THEN
         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
           DO k = kFirst,kLast
            kd = kd0 + ksgn*k
            km = km0 + ksgn*k
            CALL DIAGSTATS_LOCAL(
     U                  qSdiag(0,0,kd,bi,bj),
     I                  inpFldRL, fracFldRL,
     I                  scaleFact, power, useFract, sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  km, bi, bj, bibjFlg, region2fill,
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
     I                  inpFldRL, fracFldRL,
     I                  scaleFact, power, useFract, sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  km, biArg, bjArg, bibjFlg, region2fill,
     I                  ndId, gdiag(ndId), myThid )
          ENDDO
        ENDIF

#ifndef REAL4_IS_SLOW
      ELSE
        IF ( bibjFlg.EQ.0 ) THEN
         DO bj=myByLo(myThid), myByHi(myThid)
          DO bi=myBxLo(myThid), myBxHi(myThid)
           DO k = kFirst,kLast
            kd = kd0 + ksgn*k
            km = km0 + ksgn*k
            CALL DIAGSTATS_TO_RL(
     I                  inpFldRL, fracFldRL, inpFldRS, fracFldRS,
     O                  tmpFldRL, tmpFracRL,
     I                  arrType, useFract, sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj, myThid )
            CALL DIAGSTATS_LOCAL(
     U                  qSdiag(0,0,kd,bi,bj),
     I                  tmpFldRL, tmpFracRL,
     I                  scaleFact, power, useFract, 1,
     I                  1, iRun, 1, jRun, 1, 1, 1,
     I                  iRun, jRun, 1, 1, 1,
     I                  km, bi, bj, bibjFlg, region2fill,
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
            CALL DIAGSTATS_TO_RL(
     I                  inpFldRL, fracFldRL, inpFldRS, fracFldRS,
     O                  tmpFldRL, tmpFracRL,
     I                  arrType, useFract, sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj, myThid )
            CALL DIAGSTATS_LOCAL(
     U                  qSdiag(0,0,kd,biArg,bjArg),
     I                  tmpFldRL, tmpFracRL,
     I                  scaleFact, power, useFract, 1,
     I                  1, iRun, 1, jRun, 1, 1, 1,
     I                  iRun, jRun, 1, 1, 1,
     I                  km, biArg, bjArg, bibjFlg, region2fill,
     I                  ndId, gdiag(ndId), myThid )
          ENDDO
        ENDIF
      ENDIF
#endif /* ndef REAL4_IS_SLOW */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     ELSE

c     ENDIF

      RETURN
      END

#ifndef REAL4_IS_SLOW
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: DIAGSTATS_TO_RL
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_TO_RL(
     I                  inpFldRL, inpFrcRL, inpFldRS, inpFrcRS,
     O                  outFldRL, outFrcRL,
     I                  arrType, useFract, sizF,
     I                  sizI1,sizI2,sizJ1,sizJ2,sizK,sizTx,sizTy,
     I                  iRun,jRun,kIn,biIn,bjIn,
     I                  myThid )

C     !DESCRIPTION:
C     Do a local copy with conversion to RL type array

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     inpFldRL    :: input field array    to convert (arrType=0,1)
C     inpFrcRL    :: input fraction array to convert (arrType=0,2)
C     inpFldRS    :: input field array    to convert (arrType=2,3)
C     inpFrcRS    :: input fraction array to convert (arrType=1,3)
C     outFldRL    :: output field array
C     outFrcRL    :: output fraction array
C     arrType     :: select which array & fraction (RL/RS) to process:
C                    0: both RL ; 1: fldRL & frcRS ; 2: fldRS,frcRL ; 3: both RS
C     useFract    :: if True, process fraction-weight
C     sizF        :: size of inpFrc array: 3rd  dimension
C     sizI1,sizI2 :: size of inpFld array: 1rst index range (min,max)
C     sizJ1,sizJ2 :: size of inpFld array: 2nd  index range (min,max)
C     sizK        :: size of inpFld array: 3rd  dimension
C     sizTx,sizTy :: size of inpFld array: tile dimensions
C     iRun,jRun   :: range of 1rst & 2nd index
C     kIn         :: level index of inpFld array to process
C     biIn,bjIn   :: tile indices of inpFld array to process
C     myThid      :: my Thread Id number
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER sizF,sizK,sizTx,sizTy
      INTEGER iRun, jRun, kIn, biIn, bjIn
      _RL     inpFldRL(sizI1:sizI2,sizJ1:sizJ2,sizK,sizTx,sizTy)
      _RL     inpFrcRL(sizI1:sizI2,sizJ1:sizJ2,sizF,sizTx,sizTy)
      _RS     inpFldRS(sizI1:sizI2,sizJ1:sizJ2,sizK,sizTx,sizTy)
      _RS     inpFrcRS(sizI1:sizI2,sizJ1:sizJ2,sizF,sizTx,sizTy)
      _RL     outFldRL(1:iRun,1:jRun)
      _RL     outFrcRL(1:iRun,1:jRun)
      INTEGER arrType
      LOGICAL useFract
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j    :: loop indices
      INTEGER i, j, kFr

      IF ( arrType.LE.1 ) THEN
        DO j=1,jRun
         DO i=1,iRun
           outFldRL(i,j) = inpFldRL(i,j,kIn,biIn,bjIn)
         ENDDO
        ENDDO
      ELSE
        DO j=1,jRun
         DO i=1,iRun
           outFldRL(i,j) = inpFldRS(i,j,kIn,biIn,bjIn)
         ENDDO
        ENDDO
      ENDIF

      IF ( useFract ) THEN
       kFr = MIN(kIn,sizF)
       IF ( arrType.EQ.0 .OR. arrType.EQ.2 ) THEN
        DO j=1,jRun
         DO i=1,iRun
           outFrcRL(i,j) = inpFrcRL(i,j,kFr,biIn,bjIn)
         ENDDO
        ENDDO
       ELSE
        DO j=1,jRun
         DO i=1,iRun
           outFrcRL(i,j) = inpFrcRS(i,j,kFr,biIn,bjIn)
         ENDDO
        ENDDO
       ENDIF
      ENDIF

      RETURN
      END
#endif /* ndef REAL4_IS_SLOW */
