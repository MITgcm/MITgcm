#include "LAYERS_OPTIONS.h"
C--  File layers_save.F:
C--   Contents
C--   o LAYERS_FILL
C--   o LAYERS_FILL_FIELD

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP

      SUBROUTINE LAYERS_FILL(
     I               df, trIdentity, fluxid,
     I               kLev, nLevs, bibjFlg, biArg, bjArg, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_FILL
C     | "Remember" the merid. advective flux for use later in layers_thermodynamics
C     *==========================================================*
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"
C***********************************************************************
C   This is designed to look and work exactly like the a regular
C   diagnostics_fill call.
C***********************************************************************
C     surfflux  :: The surface temperature flux, the same as what is filled into
C                   the TFLUX and SFLUX diagnostics
C     trIdentity:: Index to let us know what tracer it is (1 for T, 2 for S)
C     kLev      :: Integer flag for vertical levels:
C                  > 0 (any integer): WHICH single level to increment in qdiag.
C                  0,-1 to increment "nLevs" levels in qdiag,
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

C       _RL df(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       _RL df(*)
       INTEGER trIdentity, kLev, nLevs, bibjFlg, biArg, bjArg
       INTEGER myThid
       CHARACTER*(3) fluxid

#ifdef LAYERS_THERMODYNAMICS

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C msgBuf           :: error message buffer
       CHARACTER*(MAX_LEN_MBUF) msgBuf

       IF ((trIdentity.EQ.1).OR.(trIdentity.EQ.2)) THEN

       IF (fluxid.EQ.'SUR') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, 1, layers_surfflux,'M',
     &           klev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE IF (fluxid.EQ.'DFX') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, Nr, layers_dfx,'U',
     &           kLev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE IF (fluxid.EQ.'DFY') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, Nr, layers_dfy,'V',
     &           kLev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE IF (fluxid.EQ.'DFR') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, Nr, layers_dfr,'M',
     &           kLev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE IF (fluxid.EQ.'AFX') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, Nr, layers_afx,'U',
     &           kLev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE IF (fluxid.EQ.'AFY') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, Nr, layers_afy,'V',
     &           kLev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE IF (fluxid.EQ.'AFR') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, Nr, layers_afr,'M',
     &           kLev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE IF (fluxid.EQ.'TOT') THEN
         CALL LAYERS_FILL_FIELD(df, trIdentity, Nr, layers_tottend,'M',
     &           kLev, nLevs, bibjFlg, biArg, bjArg, myThid)
       ELSE
         WRITE(msgBuf,'(2A)')
     &          'S/R LAYERS_FILL: ',
     &          'invalid flux ID'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R LAYERS_FILL'
       ENDIF

       ELSE
C  ---- Cannot throw an error for different trIdentity
C       because subroutine also gets called for ptracers.
C       Just have to do nothing

C         WRITE(msgBuf,'(5A,I2)')
C     &          'S/R LAYERS_FILL: ',
C     &          'only works on THETA (1) or SALT (2)',
C     &          'fluxid=', fluxid, 'trId=',
C     &          trIdentity
C         CALL PRINT_ERROR( msgBuf, myThid )
C         STOP 'ABNORMAL END: S/R LAYERS_FILL'
       ENDIF

#endif /* LAYERS_THERMODYNAMICS */

      RETURN
      END
C end of S/R LAYERS_FILL

      SUBROUTINE LAYERS_FILL_FIELD(
     I               df, trIdentity, myNr,
     U               layers_saved_flux,
     I               fldType,
     I               kLev, nLevs, bibjFlg, biArg, bjArg, myThid )

       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

       INTEGER trIdentity, myNr, kLev, nLevs, bibjFlg, biArg, bjArg
       CHARACTER fldType
       _RL layers_saved_flux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                       myNr,2,nSx,nSy)
C       _RL df(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       _RL df(*)
       INTEGER myThid

#ifdef LAYERS_THERMODYNAMICS

C !LOCAL VARIABLES: ====================================================
C i,j              :: loop indices
C msgBuf           :: error message buffer
      INTEGER sizI1,sizI2,sizJ1,sizJ2
      INTEGER sizTx,sizTy
      INTEGER iRun, jRun, k, bi, bj
      INTEGER kFirst, kLast
      INTEGER kd, kd0, ksgn
C       CHARACTER*(MAX_LEN_MBUF) msgBuf

C-      select range for 1rst & 2nd indices to accumulate
C         depending on variable location on C-grid,
      IF ( fldType.EQ.'U' ) THEN
       iRun = sNx+1
       jRun = sNy
      ELSEIF ( fldType.EQ.'V' ) THEN
       iRun = sNx
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
        STOP 'ABNORMAL END in LAYERS_SAVE: kLev > nLevs >0'
      ENDIF
C-      Which part of qdiag to update: kd = 3rd index,
C         and do the loop >> do k=kFirst,kLast ; kd = kd0 + k*ksgn <<
      IF ( kLev.EQ.-1 ) THEN
        ksgn = -1
        kd0 = 1 + nLevs
      ELSEIF ( kLev.EQ.0 ) THEN
        ksgn = 1
        kd0 = 0
      ELSE
        ksgn = 0
        kd0 = kLev
      ENDIF

      IF ( bibjFlg.EQ.0 ) THEN

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
         DO k = kFirst,kLast
          kd = kd0 + ksgn*k
          CALL LAYERS_CUMULATE(
     U      layers_saved_flux(1-OLx,1-OLy,kd,trIdentity,bi,bj),
     I      df,
     I      sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I      iRun,jRun,k,bi,bj,
     I      myThid)
         ENDDO
        ENDDO
       ENDDO
      ELSE
        bi = MIN(biArg,sizTx)
        bj = MIN(bjArg,sizTy)
        DO k = kFirst,kLast
          kd = kd0 + ksgn*k
          CALL LAYERS_CUMULATE(
     U      layers_saved_flux(1-OLx,1-OLy,kd,trIdentity,biArg,bjArg),
     I      df,
     I      sizI1,sizI2,sizJ1,sizJ2,nLevs,sizTx,sizTy,
     I      iRun,jRun,k,bi,bj,
     I      myThid)
        ENDDO
      ENDIF

C        IF (bibjFlg.EQ.2) THEN
C C --   called INSIDE the bi-bj loop, with overlap present
C         DO k = kstart,kend
C          DO j = 1-OLy,sNy+OLy
C           DO i = 1-OLx,sNx+OLx
C            layers_saved_flux(i,j,k,trIdentity,biArg,bjArg) =
C      &       layers_saved_flux(i,j,k,trIdentity,biArg,bjArg) +
C      &        df(i,j,1,1)
C           ENDDO
C          ENDDO
C         ENDDO
C        ELSE IF (bibjFlg.EQ.0) THEN
C C --   the bi-bj loop must be done here
C         DO bj=myByLo(myThid), myByHi(myThid)
C          DO bi=myBxLo(myThid), myBxHi(myThid)
C           DO k = kstart,kend
C            DO j = 1-OLy,sNy+OLy
C             DO i = 1-OLx,sNx+OLx
C               layers_saved_flux(i,j,k,trIdentity,bi,bj) =
C      &          layers_saved_flux(i,j,k,trIdentity,bi,bj) +
C      &          df(i,j,bi,bj)
C             ENDDO
C            ENDDO
C           ENDDO
C          ENDDO
C         ENDDO
C        ELSE
C            WRITE(msgBuf,'(2A)')
C      &          'S/R LAYERS_FILL_FIELD: ',
C      &          'got unexpected bibjFlg'
C            CALL PRINT_ERROR( msgBuf, myThid )
C            STOP 'ABNORMAL END: S/R LAYERS_FILL_FIELD'
C        ENDIF

#endif /* LAYERS_THERMODYNAMICS */

      RETURN
      END
C end of S/R LAYERS_FILL_FIELD

      SUBROUTINE LAYERS_CUMULATE(
     U                  cumFld,
     I                  inpFld,
     I                  sizI1,sizI2,sizJ1,sizJ2,sizK,sizTx,sizTy,
     I                  iRun,jRun,k,bi,bj,
     I                  myThid )

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
C     k,bi,bj     :: level and tile indices of inFld array to add to cumFld array
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
C      _RL     tmpFact

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO j = 1,jRun
       DO i = 1,iRun
        cumFld(i,j) = cumFld(i,j) + inpFld(i,j,k,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END

