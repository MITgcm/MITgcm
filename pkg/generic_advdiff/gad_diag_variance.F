#include "GAD_OPTIONS.h"

C--   File gad_diag_variance.F:
C      Contents
C      o GAD_DIAG_VARIANCE_K
C      o GAD_DIAG_VARIANCE_PREP
C      o GAD_DIAG_VARIANCE_SET

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GAD_DIAG_VARIANCE_K

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIAG_VARIANCE_K(
     I     trIdentity, trMultiDimAdvec, calcAdvection,
     I     xA, yA, uTrans, vTrans, rTransKp,
     I     fZon, fMer, fVert, gForc,
     I     tracer,
     U     tracTmp,
     I     gTracer, deltaTLev,
     I     k, bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef EXACT_CONSERV
# include "SURFACE.h"
#endif
#ifdef ALLOW_ADDFLUID
# include "FFIELDS.h"
#endif

C !INPUT/OUTPUT PARAMETERS: ============================================
C  trIdentity       :: tracer identifier
C  trMultiDimAdvec  :: tracer use Multi-Dim advection
C  calcAdvection    :: advective contribution is part of input fluxes
C  xA,yA            :: areas of X and Y face of tracer cells
C  uTrans,vTrans    :: 2-D arrays of volume transports at U,V points
C  rTransKp         :: vertical volume transport at interface k+1
C  fZon             :: 2-D array for horizontal advective flux, x direction
C  fMer             :: 2-D array for horizontal advective flux, y direction
C  fVert            :: 2-D array for vertical advective flux (@ interface k+1)
C  gForc            :: 2-D array for tracer forcing tendency
C  tracer           :: tracer field
C  tracTmp          :: intermediate tracer field (after multi-dim update)
C  gTracer          :: tracer tendency
C  deltaTLev        :: time-step [s] (vertical dimension vector)
C  k                :: current vertical index
C  bi, bj           :: tile indices
C  myTime           :: current time
C  myIter           :: iteration number
C  myThid           :: my Thread Id number
      INTEGER trIdentity
      LOGICAL trMultiDimAdvec
      LOGICAL calcAdvection
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVert   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gForc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tracTmp (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gTracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL deltaTLev(Nr)
      INTEGER k, bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS
C !FUNCTIONS: ==========================================================
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

C !LOCAL VARIABLES: ====================================================
C  tracNew       :: updated tracer field at level k
C  i,j           :: loop indices
      INTEGER i, j, kSurface, dBl
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      _RL tracNew (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagTr  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagT2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagTrKp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagT2Kp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagVTR (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagVPR (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagSqD (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diagForc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL delTx, delTy, delTr, delT2
      _RL barTx, barTy, barTr, barT2
      _RL oldTr, newTr, maskDwn
      _RL advFac, tmpFac

C--   Evaluate square of tracer gradient from before the update (consistent
C      with forward-in-time time-stepping); otherwise, from time-centered
C     averaged (half sum of tracer-value before and after the update)
      LOGICAL     diagAdvVarE
      LOGICAL     fwdSqGrad
      PARAMETER ( fwdSqGrad  = .TRUE. )
c     PARAMETER ( fwdSqGrad  = .FALSE. )
      LOGICAL     diagNumDif
      LOGICAL     diagNumDf1
CEOP

      advFac = zeroRL
      IF ( calcAdvection ) advFac = oneRL
      dBl = 2
      IF ( trMultiDimAdvec ) dBl = -2

      diagAdvVarE = .TRUE.
      diagNumDf1 = .FALSE.
      diagNumDif = .FALSE.
C  diagAdvVarE :: this is method 2
C  diagNumDf1 :: this is method 1
C  diagNumDif :: Klingbeil etal method
c     diagNumDf1 = diagAdvVarE
c     diagNumDif = diagAdvVarE
      IF ( diagNumDf1 .OR. diagNumDif ) diagAdvVarE = .FALSE.
      diagNumDif = diagNumDif .AND. calcAdvection
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
c     WRITE(errorMessageUnit,'(A,I9,A,4I4)')
c    &   'Entering DIAG_VARIANCE_K iter=',  myIter,
c    &   ' : tr-Id, bi,bj,k:', trIdentity, bi, bj, k

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingZCoords .AND. useShelfIce ) THEN
       kSurface = -1
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Copy tracer array into tracTmp if not filled by multi-dim advection
c     IF ( k.LT.Nr .AND. .NOT.trMultiDimAdvec ) THEN
c      DO j=1-OLy,sNy+OLy
c       DO i=1-OLx,sNx+OLx
c         tracTmp(i,j,k)   = tracer(i,j,k)
c         tracTmp(i,j,k+1) = tracer(i,j,k+1)
c       ENDDO
c      ENDDO
c     ELSEIF ( .NOT.trMultiDimAdvec ) THEN
C-    Note: level k+1 has been filled up by previous call
      IF ( .NOT.trMultiDimAdvec ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          tracTmp(i,j,k) = tracer(i,j,k)
        ENDDO
       ENDDO
      ENDIF

C--   Calculate updated (=new) tracer value at current level k:
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          tracNew(i,j) = tracer(i,j,k) + deltaTLev(k)*gTracer(i,j,k)
        ENDDO
      ENDDO
C--   Calculate centered-in-time average and square of tracer
C-    at current level k:
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          oldTr = tracTmp(i,j,k)
          newTr = tracNew(i,j)
          diagTr(i,j) = ( oldTr + newTr )*halfRL
          diagT2(i,j) = ( oldTr * newTr )
        ENDDO
      ENDDO
C-    at level k+1:
      IF ( k.LT.Nr ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          oldTr = tracTmp(i,j,k+1)
          newTr = tracer(i,j,k+1) + deltaTLev(k+1)*gTracer(i,j,k+1)
          diagTrKp(i,j) = ( oldTr + newTr )*halfRL
          diagT2Kp(i,j) = ( oldTr * newTr )
        ENDDO
       ENDDO
      ENDIF

C--   Calculate Forcing contribution:
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
C-jmc: could think of re-using an other 2-d local array, such as diagSqD
          diagForc(i,j) = twoRL*diagTr(i,j)*gForc(i,j)
     &                  * rA(i,j,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
        ENDDO
      ENDDO
#ifdef EXACT_CONSERV
      IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &     .AND. useRealFreshWaterFlux ) THEN
       IF (  k .EQ. kSurface ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           diagForc(i,j) = diagForc(i,j)
c    &      + ( twoRL*diagTr(i,j)*theta(i,j,k,bi,bj) - diagT2(i,j) )
     &      + ( twoRL*diagTr(i,j)*tracer(i,j,k) - diagT2(i,j) )
     &       *rA(i,j,bi,bj)*PmEpR(i,j,bi,bj)*mass2rUnit
         ENDDO
        ENDDO
       ELSEIF ( kSurface.EQ.-1 ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kSurfC(i,j,bi,bj).EQ.k ) THEN
           diagForc(i,j) = diagForc(i,j)
c    &      + ( twoRL*diagTr(i,j)*theta(i,j,k,bi,bj) - diagT2(i,j) )
     &      + ( twoRL*diagTr(i,j)*tracer(i,j,k) - diagT2(i,j) )
     &       *rA(i,j,bi,bj)*PmEpR(i,j,bi,bj)*mass2rUnit
          ENDIF
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif
#ifdef ALLOW_ADDFLUID
      IF ( selectAddFluid.GE.1 .AND. nonlinFreeSurf.GT.0 ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           diagForc(i,j) = diagForc(i,j)
c    &      + ( twoRL*diagTr(i,j)*theta(i,j,k,bi,bj) - diagT2(i,j) )
     &      + ( twoRL*diagTr(i,j)*tracer(i,j,k) - diagT2(i,j) )
     &       *addMass(i,j,k,bi,bj)*mass2rUnit
         ENDDO
        ENDDO
      ENDIF
#endif
      diagName = 'VPRf'//diagSufx
      CALL DIAGNOSTICS_FILL( diagForc,diagName,k,1,2,bi,bj,myThid )

      IF ( diagNumDif ) THEN
C--   Residual is Total change of variance (delT2) per time unit
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          oldTr = tracTmp(i,j,k)
          newTr = tracNew(i,j)
          diagVPR(i,j) = ( newTr*newTr - oldTr*oldTr )
     &                 * rA(i,j,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
     &                 / dTtracerLev(k)
     &                 + diagForc(i,j)
        ENDDO
       ENDDO
      ENDIF

C--   X direction
c     IF ( diagAdvVarE .AND. fwdSqGrad ) THEN
      IF ( ( diagAdvVarE .OR. diagNumDf1 ) .AND. fwdSqGrad ) THEN
        DO j=1,sNy
         DO i=1-OLx+2,sNx+OLx-1
           delTx = ( tracTmp(i,j,k) - tracTmp(i-1,j,k) )
           diagSqD(i,j) = delTx*delTx*xA(i,j)*maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF
      IF ( diagAdvVarE ) THEN
        DO j=1,sNy
         DO i=1-OLx+2,sNx+OLx-1
           delTx = ( diagTr(i,j) - diagTr(i-1,j) )*maskW(i,j,k,bi,bj)
           barTx = ( diagTr(i,j) + diagTr(i-1,j) )*halfRL
C- notes: masking delT2 is (most likely) not necessary
           delT2 = ( diagT2(i,j) - diagT2(i-1,j) )*maskW(i,j,k,bi,bj)
           barT2 = ( diagT2(i,j) + diagT2(i-1,j) )*halfRL
           diagVTR(i,j) = twoRL*fZon(i,j)*barTx
     &                  - uTrans(i,j)*barT2*advFac
           diagVPR(i,j) = twoRL*fZon(i,j)*delTx
     &                  - uTrans(i,j)*delT2*advFac
           IF ( .NOT.fwdSqGrad )
     &     diagSqD(i,j) = delTx*delTx*xA(i,j)
         ENDDO
        ENDDO
      ENDIF
      IF ( diagNumDf1 ) THEN
C--   Compute 2nd Order advective flux in X, to substract to Advective Flux;
C     this gives the numerical-diffusion flux; then, to get the effect on
C     tracer variance, multiply by the gradient in X
        DO j=1,sNy
         DO i=1-OLx+2,sNx+OLx-1
           delTx = ( diagTr(i,j) - diagTr(i-1,j) )*maskW(i,j,k,bi,bj)
           barTx = ( diagTr(i,j) + diagTr(i-1,j) )*halfRL
           barT2 = ( diagTr(i,j) * diagTr(i,j)
     &             + diagTr(i-1,j)*diagTr(i-1,j) )*halfRL
           diagVTR(i,j) = twoRL*fZon(i,j)*barTx
     &                  - uTrans(i,j)*barT2*advFac
           diagVPR(i,j) = twoRL*( fZon(i,j)
     &                          - uTrans(i,j)*barTx*advFac )*delTx
           IF ( .NOT.fwdSqGrad )
     &     diagSqD(i,j) = delTx*delTx*xA(i,j)
         ENDDO
        ENDDO
      ENDIF
      IF ( diagNumDif ) THEN
        DO j=1,sNy
         DO i=1-OLx+2,sNx+OLx-1
           IF ( uTrans(i,j).EQ.zeroRL ) THEN
             diagVTR(i,j) = 0.
           ELSE
             diagVTR(i,j) = fZon(i,j)*fZon(i,j)/uTrans(i,j)
           ENDIF
         ENDDO
        ENDDO
C--   Update residual by adding divergence in X-direction of var.transport
        DO j=1,sNy
         DO i=1-OLx+2,sNx+OLx-2
           diagVPR(i,j) = diagVPR(i,j)
     &                  - ( diagVTR(i,j) - diagVTR(i+1,j) )
         ENDDO
        ENDDO
      ENDIF
C-    fill diagnostics:
      IF ( diagAdvVarE .OR. diagNumDf1 .OR. diagNumDif ) THEN
        diagName = 'VTRx'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVTR,diagName,k,1,dBl,bi,bj,myThid )
      ENDIF
      IF ( diagAdvVarE .OR. diagNumDf1 ) THEN
        diagName = 'VPRx'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVPR,diagName,k,1,dBl,bi,bj,myThid )
       IF ( .NOT.trMultiDimAdvec ) THEN
        diagName = 'SqDx'//diagSufx
        CALL DIAGNOSTICS_FILL( diagSqD,diagName,k,1,dBl,bi,bj,myThid )
       ENDIF
      ENDIF

C--   Y direction
c     IF ( diagAdvVarE .AND. fwdSqGrad ) THEN
      IF ( ( diagAdvVarE .OR. diagNumDf1 ) .AND. fwdSqGrad ) THEN
        DO j=1-OLy+2,sNy+OLy-1
         DO i=1,sNx
           delTy = ( tracTmp(i,j,k) - tracTmp(i,j-1,k) )
           diagSqD(i,j) = delTy*delTy*yA(i,j)*maskS(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF
      IF ( diagAdvVarE ) THEN
        DO j=1-OLy+2,sNy+OLy-1
         DO i=1,sNx
           delTy = ( diagTr(i,j) - diagTr(i,j-1) )*maskS(i,j,k,bi,bj)
           barTy = ( diagTr(i,j) + diagTr(i,j-1) )*halfRL
C- notes: masking delT2 is (most likely) not necessary
           delT2 = ( diagT2(i,j) - diagT2(i,j-1) )*maskS(i,j,k,bi,bj)
           barT2 = ( diagT2(i,j) + diagT2(i,j-1) )*halfRL
           diagVTR(i,j) = twoRL*fMer(i,j)*barTy
     &                  - vTrans(i,j)*barT2*advFac
           diagVPR(i,j) = twoRL*fMer(i,j)*delTy
     &                  - vTrans(i,j)*delT2*advFac
           IF ( .NOT.fwdSqGrad )
     &     diagSqD(i,j) = delTy*delTy*yA(i,j)
         ENDDO
        ENDDO
      ENDIF
      IF ( diagNumDf1 ) THEN
C--   Compute 2nd Order advective flux in Y, to substract to Advective Flux;
C     this gives the numerical-diffusion flux; then, to get the effect on
C     tracer variance, multiply by by the gradient in Y
        DO j=1-OLy+2,sNy+OLy-1
         DO i=1,sNx
           delTy = ( diagTr(i,j) - diagTr(i,j-1) )*maskS(i,j,k,bi,bj)
           barTy = ( diagTr(i,j) + diagTr(i,j-1) )*halfRL
           barT2 = ( diagTr(i,j) * diagTr(i,j)
     &             + diagTr(i,j-1)*diagTr(i,j-1) )*halfRL
           diagVTR(i,j) = twoRL*fMer(i,j)*barTy
     &                  - vTrans(i,j)*barT2*advFac
           diagVPR(i,j) = twoRL*( fMer(i,j)
     &                          - vTrans(i,j)*barTy*advFac )*delTy
           IF ( .NOT.fwdSqGrad )
     &     diagSqD(i,j) = delTy*delTy*yA(i,j)
         ENDDO
        ENDDO
      ENDIF
      IF ( diagNumDif ) THEN
        DO j=1-OLy+2,sNy+OLy-1
         DO i=1,sNx
           IF ( vTrans(i,j).EQ.zeroRL ) THEN
             diagVTR(i,j) = 0.
           ELSE
             diagVTR(i,j) = fMer(i,j)*fMer(i,j)/vTrans(i,j)
           ENDIF
         ENDDO
        ENDDO
C--   Update residual by adding divergence in Y-direction of var.transport
        DO j=1-OLy+2,sNy+OLy-2
         DO i=1,sNx
           diagVPR(i,j) = diagVPR(i,j)
     &                  - ( diagVTR(i,j) - diagVTR(i,j+1) )
         ENDDO
        ENDDO
      ENDIF
C-    fill diagnostics:
      IF ( diagAdvVarE .OR. diagNumDf1 .OR. diagNumDif ) THEN
        diagName = 'VTRy'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVTR,diagName,k,1,dBl,bi,bj,myThid )
      ENDIF
      IF ( diagAdvVarE .OR. diagNumDf1 ) THEN
        diagName = 'VPRy'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVPR,diagName,k,1,dBl,bi,bj,myThid )
       IF ( .NOT.trMultiDimAdvec ) THEN
        diagName = 'SqDy'//diagSufx
        CALL DIAGNOSTICS_FILL( diagSqD,diagName,k,1,dBl,bi,bj,myThid )
       ENDIF
      ENDIF

C--   Vertical direction
      IF ( k.LT.Nr .AND. diagAdvVarE ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           maskDwn = maskC(i,j,k,bi,bj)*maskC(i,j,k+1,bi,bj)
           delTr = ( diagTrKp(i,j) - diagTr(i,j) )*rkSign*maskDwn
           barTr = ( diagTrKp(i,j) + diagTr(i,j) )*halfRL
C- notes: masking delT2 is (most likely) not necessary
           delT2 = ( diagT2Kp(i,j) - diagT2(i,j) )*rkSign*maskDwn
           barT2 = ( diagT2Kp(i,j) + diagT2(i,j) )*halfRL
           diagVTR(i,j) = twoRL*fVerT(i,j)*barTr
     &                  - rTransKp(i,j)*barT2*advFac
           diagVPR(i,j) = twoRL*fVerT(i,j)*delTr
     &                  - rTransKp(i,j)*delT2*advFac
           IF ( fwdSqGrad ) THEN
            delTr = ( tracTmp(i,j,k+1) - tracTmp(i,j,k) )*rkSign*maskDwn
           ENDIF
           diagSqD(i,j) = delTr*delTr*rA(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF
      IF ( k.LT.Nr .AND. diagNumDf1 ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           maskDwn = maskC(i,j,k,bi,bj)*maskC(i,j,k+1,bi,bj)
           delTr = ( diagTrKp(i,j) - diagTr(i,j) )*rkSign*maskDwn
           barTr = ( diagTrKp(i,j) + diagTr(i,j) )*halfRL
c          barT2 = ( diagT2Kp(i,j) + diagT2(i,j) )*halfRL
           barT2 = ( diagTrKp(i,j)*diagTrKp(i,j)
     &             + diagTr(i,j) * diagTr(i,j) )*halfRL
           diagVTR(i,j) = twoRL*fVerT(i,j)*barTr
     &                  - rTransKp(i,j)*barT2*advFac
           diagVPR(i,j) = twoRL*( fVerT(i,j)
     &                          - rTransKp(i,j)*barTr*advFac )*delTr
           IF ( fwdSqGrad ) THEN
            delTr = ( tracTmp(i,j,k+1) - tracTmp(i,j,k) )*rkSign*maskDwn
           ENDIF
           diagSqD(i,j) = delTr*delTr*rA(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF
      IF ( k.LT.Nr .AND. diagNumDif ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( rTransKp(i,j).EQ.zeroRL ) THEN
             diagVTR(i,j) = 0.
          ELSE
             diagVTR(i,j) = fVerT(i,j)*fVerT(i,j)/rTransKp(i,j)
          ENDIF
         ENDDO
        ENDDO
        diagName = 'VTRr'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVTR,diagName,k+1,1,dBl,bi,bj,myThid )
C--   Update residual by adding divergence in R-direction of var.transport
C     account for lower interf flux:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           diagVPR(i,j) = diagVPR(i,j) + diagVTR(i,j)*rkSign
         ENDDO
        ENDDO
C     account for upper interf flux directly in Diagnostics storage array:
C     added to level k+1, never called with k+1 = 1 --> counter not incremented
        diagName = 'VRes'//diagSufx
        tmpFac = -rkSign
        CALL DIAGNOSTICS_SCALE_FILL( diagVTR, tmpFac, 1, diagName,
     &                         k+1, 1, -2, bi, bj, myThid )
      ENDIF
      IF ( diagNumDif ) THEN
        diagName = 'VRes'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVPR,diagName,k,1,dBl,bi,bj,myThid )
      ENDIF
C-    fill diagnostics:
      IF ( k.LT.Nr .AND. ( diagAdvVarE .OR. diagNumDf1 ) ) THEN
        diagName = 'VTRr'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVTR,diagName,k+1,1,dBl,bi,bj,myThid )
        diagName = 'VPRr'//diagSufx
        CALL DIAGNOSTICS_FILL( diagVPR,diagName,k+1,1,dBl,bi,bj,myThid )
       IF ( .NOT.trMultiDimAdvec ) THEN
        diagName = 'SqDr'//diagSufx
        CALL DIAGNOSTICS_FILL( diagSqD,diagName,k+1,1,dBl,bi,bj,myThid )
       ENDIF
      ENDIF

C--   save current level value for next call (will be for level k-1):
c     DO j=1-OLy,sNy+OLy
c       DO i=1-OLx,sNx+OLx
c         diagTrKp(i,j) = diagTr(i,j)
c         diagT2Kp(i,j) = diagT2(i,j)
c       ENDDO
c     ENDDO
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GAD_DIAG_VARIANCE_PREP

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIAG_VARIANCE_PREP(
     I               trIdentity, trMultiDimAdvec,
     I               tracer, gTracer, deltaTLev,
     O               localTrc,
     I               bi, bj, myThid )

C !DESCRIPTION:

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
C  trIdentity       :: tracer identifier
C  trMultiDimAdvec  :: tracer use Multi-Dim advection
C  tracer           :: tracer field
C  gTracer          :: tendency array
C  deltaTLev        :: time-step
C  localTrc         :: intermediate tracer value (partial update)
C  bi, bj           :: tile indices
C  myThid           :: my Thread Id number
      INTEGER trIdentity
      LOGICAL trMultiDimAdvec
      _RL tracer  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gTracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL localTrc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL deltaTLev(Nr)
      INTEGER bi, bj
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C  i, j, k          :: indices
      INTEGER i, j, k
CEOP

      IF ( trMultiDimAdvec ) THEN
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            localTrc(i,j,k) = tracer(i,j,k)
     &                      + deltaTLev(k)*gTracer(i,j,k)
          ENDDO
         ENDDO
        ENDDO
      ELSE
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            localTrc(i,j,k) = tracer(i,j,k)
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: GAD_DIAG_VARIANCE_SET

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DIAG_VARIANCE_SET(
     O     diagAdvVarE, diagAdvVarI,
     I     implVertAdv, trIdentity, bi, bj, myThid )

C !DESCRIPTION:

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C !INPUT/OUTPUT PARAMETERS: ===================================================
C  diagAdvVarE  :: true when diagnosing Variance Budget
C               :: for this tracer Explicit time-stepping
C  diagAdvVarI  :: true when diagnosing Variance Budget
C               :: for this tracer Implicit time-stepping
C  trIdentity   :: tracer identifier
C  implVertAdv  :: True if vertical advection is done implicitly
C  bi, bj       :: tile indices
C  myThid       :: my Thread Id number
      LOGICAL diagAdvVarE, diagAdvVarI
      LOGICAL implVertAdv
      INTEGER trIdentity
      INTEGER bi, bj
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS
C !FUNCTIONS: ==========================================================
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

C !LOCAL VARIABLES: ====================================================
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
#endif /* ALLOW_DIAGNOSTICS */
CEOP

      diagAdvVarE = .FALSE.
      diagAdvVarI = .FALSE.
#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostics flags and suffix for the current tracer
c     IF ( useDiagnostics ) THEN
        diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
        diagName = 'VTRx'//diagSufx
c       IF ( .NOT.diagAdvVarE ) diagAdvVarE = DIAGNOSTICS_IS_ON( diagName, myThid )
        diagAdvVarE = diagAdvVarE.OR.DIAGNOSTICS_IS_ON(diagName,myThid)
        diagName = 'VTRy'//diagSufx
        diagAdvVarE = diagAdvVarE.OR.DIAGNOSTICS_IS_ON(diagName,myThid)
        diagName = 'VPRx'//diagSufx
        diagAdvVarE = diagAdvVarE.OR.DIAGNOSTICS_IS_ON(diagName,myThid)
        diagName = 'VPRy'//diagSufx
        diagAdvVarE = diagAdvVarE.OR.DIAGNOSTICS_IS_ON(diagName,myThid)
        diagName = 'SqDx'//diagSufx
        diagAdvVarE = diagAdvVarE.OR.DIAGNOSTICS_IS_ON(diagName,myThid)
        diagName = 'SqDy'//diagSufx
        diagAdvVarE = diagAdvVarE.OR.DIAGNOSTICS_IS_ON(diagName,myThid)
        diagName = 'VTRr'//diagSufx
        IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
          diagAdvVarE = .TRUE.
          CALL DIAGNOSTICS_COUNT( diagName, bi, bj, myThid )
        ENDIF
        diagName = 'VPRr'//diagSufx
        IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
          diagAdvVarE = .TRUE.
          CALL DIAGNOSTICS_COUNT( diagName, bi, bj, myThid )
        ENDIF
        diagName = 'SqDr'//diagSufx
        IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
          diagAdvVarE = .TRUE.
          CALL DIAGNOSTICS_COUNT( diagName, bi, bj, myThid )
        ENDIF
        diagName = 'VPRf'//diagSufx
        diagAdvVarE = diagAdvVarE.OR.DIAGNOSTICS_IS_ON(diagName,myThid)
C--
       IF ( implicitDiffusion .OR. implVertAdv ) THEN
        diagName = 'VTRi'//diagSufx
        IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
          diagAdvVarI = .TRUE.
          CALL DIAGNOSTICS_COUNT( diagName, bi, bj, myThid )
        ENDIF
        diagName = 'VPRi'//diagSufx
        IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
          diagAdvVarI = .TRUE.
          CALL DIAGNOSTICS_COUNT( diagName, bi, bj, myThid )
        ENDIF
        diagName = 'SqDi'//diagSufx
        IF ( DIAGNOSTICS_IS_ON( diagName, myThid ) ) THEN
          diagAdvVarI = .TRUE.
          CALL DIAGNOSTICS_COUNT( diagName, bi, bj, myThid )
        ENDIF
       ENDIF
c     ENDIF
#endif /* ALLOW_DIAGNOSTICS */
c     WRITE(errorMessageUnit,'(2A,3I4,2L5)') ' GAD_DIAG_VARIANCE_SET:',
c    &  ' tr-Id, bi,bj, diagAdvVar(E,I):', trIdentity, bi, bj,
c    &                                     diagAdvVarE, diagAdvVarI

      RETURN
      END
