C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_calc_phivel.F,v 1.11 2014/11/25 01:12:11 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_CALC_PHIVEL

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_CALC_PHIVEL(
     I                        listId, md, ndId, ip, im, lm,
     I                        NrMax,
     U                        qtmp1, qtmp2,
     I                        myTime, myIter, myThid )

C     !DESCRIPTION:
C     Compute Velocity Potential and Velocity Stream-Function

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"
#include "DIAGNOSTICS_CALC.h"

C     !INPUT PARAMETERS:
C     listId  :: Diagnostics list number being written
C     md      :: field number in the list "listId".
C     ndId    :: diagnostics  Id number (in available diagnostics list)
C     ip      :: diagnostics  pointer to storage array
C     im      :: counter-mate pointer to storage array
C     lm      :: index in the averageCycle
C     NrMax   :: 3rd dimension of input/output arrays
C     qtmp1   :: horizontal velocity input diag., u-component
C     qtmp2   :: horizontal velocity input diag., v-component
C     myTime  :: current time of simulation (s)
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      INTEGER listId, md, ndId, ip, im, lm
      INTEGER NrMax
      _RL     qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     qtmp2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     myTime
      INTEGER myIter, myThid

C     !OUTPUT PARAMETERS:
C     qtmp1   :: horizontal-velocity potential
C     qtmp2   :: horizontal-velocity stream-function
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     bi, bj  :: tile indices
C     i,j,k   :: loop indices
C     uTrans  :: horizontal transport, u-component
C     vTrans  :: horizontal transport, u-component
C     psiVel  :: horizontal stream-function
C     psiLoc  :: horizontal stream-function at special location
      INTEGER bi, bj
      INTEGER i, j, k

      INTEGER ks
      INTEGER numIters, nIterMin
      LOGICAL normaliseMatrice, diagNormaliseRHS
      _RL  residCriter, firstResidual, minResidual, lastResidual
      _RL  a2dMax, a2dNorm
      _RL  rhsMax, b2dNorm, x2dNorm
      _RS  aW2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  b2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  x2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL psiVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL psiLoc(2)
      INTEGER iL
      CHARACTER*(MAX_LEN_FNAM) dataFName
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('DIAGNOSTICS_CALC_PHIVEL',myThid)
#endif

      DO ks = 1,nlevels(listId)
        k = NINT(levs(ks,listId))
C--   Solve for velocity potential for each level:

        a2dMax = 0. _d 0
        rhsMax = 0. _d 0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C-    Initialise fist guess & RHS
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              b2d(i,j,bi,bj) = 0.
              x2d(i,j,bi,bj) = 0.
            ENDDO
           ENDDO
C-    calculate cg2d matrix:
C     Note: Here, at Open-Boundary location, we keep non-zero aW & aS (using
C     maskInW & maskInS) whereas in S/R CG2D they are zero (product of maskInC)
           DO j = 1,sNy+1
            DO i = 1,sNx+1
              aW2d(i,j,bi,bj) = dyG(i,j,bi,bj)*recip_dxC(i,j,bi,bj)
     &                         *drF(k)*hFacW(i,j,k,bi,bj)
     &                         *maskInW(i,j,bi,bj)
              aS2d(i,j,bi,bj) = dxG(i,j,bi,bj)*recip_dyC(i,j,bi,bj)
     &                         *drF(k)*hFacS(i,j,k,bi,bj)
     &                         *maskInS(i,j,bi,bj)
              a2dMax = MAX(a2dMax,aW2d(i,j,bi,bj))
              a2dMax = MAX(a2dMax,aS2d(i,j,bi,bj))
            ENDDO
           ENDDO

C-    calculate horizontal transport
           DO j = 1,sNy+1
            DO i = 1,sNx+1
              uTrans(i,j,bi,bj) = dyG(i,j,bi,bj)*drF(k)
     &                          *qtmp1(i,j,ks,bi,bj)*maskInW(i,j,bi,bj)
              vTrans(i,j,bi,bj) = dxG(i,j,bi,bj)*drF(k)
     &                          *qtmp2(i,j,ks,bi,bj)*maskInS(i,j,bi,bj)
            ENDDO
           ENDDO
C-   end bi,bj loops
         ENDDO
        ENDDO

C-    calculate RHS = rAc*Div(uVel,vVel):
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              b2d(i,j,bi,bj) = (
     &                    ( uTrans(i+1,j,bi,bj) - uTrans(i,j,bi,bj) )
     &                  + ( vTrans(i,j+1,bi,bj) - vTrans(i,j,bi,bj) )
     &                         )*maskInC(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_OBCS
C  There is ambiguity in splitting OB cross flow into divergent (grad.Phi)
C    contribution and rotational (rot.Psi) contribution:
C  a) In most cases, will interpret most of the OB cross flow (except
C     the net inflow which has to come from grad.Phi) as non divergent
C     and only keep the divergence associated with the net inflow
C     (assuming here uniform distribution along the OB section;
C     This processing must be done for each domain-connected section
C     of the OB (using pkg/obcs OB[N,S,E,W]_connect Id) otherwise the
C     solver will not converge.
C  b) When setting a null domain-connected Id for some OB section,
C     we can recover the other extreme where the OB cross flow is
C     interpreted as a pure divergent (grad.Phi) contribution (-> constant
C      Psi along this section). This is done by keeping the full RHS just
C     outside OB (i.e., where tracer OBCS are applied.
        IF ( useOBCS ) THEN
         CALL OBCS_DIAG_BALANCE(
     U             b2d,
     I             uTrans, vTrans, k,
     I             myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_OBCS */

C-    Normalise Matrice & RHS :
        diagNormaliseRHS = diagCG_resTarget.GT.0.
        normaliseMatrice = .TRUE.
        diagNormaliseRHS = .TRUE.
        IF ( diagNormaliseRHS ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
              rhsMax = MAX(ABS(b2d(I,J,bi,bj)),rhsMax)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        a2dNorm = 1. _d 0
        b2dNorm = 1. _d 0
        x2dNorm = 1. _d 0
        IF ( normaliseMatrice ) THEN
          _GLOBAL_MAX_RL( a2dMax, myThid )
          IF ( a2dMax .GT. 0. _d 0 ) a2dNorm = 1. _d 0/a2dMax
          b2dNorm = a2dNorm
        ENDIF
        IF ( diagNormaliseRHS ) THEN
          _GLOBAL_MAX_RL( rhsMax, myThid )
          IF ( rhsMax .GT. 0. _d 0 ) THEN
            b2dNorm = 1. _d 0/rhsMax
            x2dNorm = a2dNorm*rhsMax
          ENDIF
          residCriter = diagCG_resTarget
        ELSE
          residCriter = a2dNorm * ABS(diagCG_resTarget)
     &                          * globalArea / deltaTMom
        ENDIF
        IF ( normaliseMatrice .OR. diagNormaliseRHS ) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1,sNy+1
             DO i = 1,sNx+1
              aW2d(i,j,bi,bj) = aW2d(i,j,bi,bj)*a2dNorm
              aS2d(i,j,bi,bj) = aS2d(i,j,bi,bj)*a2dNorm
              b2d(i,j,bi,bj)  = b2d(i,j,bi,bj) *b2dNorm
c             x2d(i,j,bi,bj) =  x2d(i,j,bi,bj) /x2dNorm
             ENDDO
            ENDDO
           ENDDO
          ENDDO
        ENDIF

        IF ( debugLevel.GE.debLevA .AND. k.EQ.1 ) THEN
          _BEGIN_MASTER( myThid )
          WRITE(standardMessageUnit,'(A,I9,2(A,1P1E13.6),A,1P1E9.2)')
     &     ' diag_cg2d (it=', myIter,') a2dNorm,x2dNorm=', a2dNorm,
     &     ' ,', x2dNorm, ' ; Criter=', residCriter
          _END_MASTER( myThid )
        ENDIF

        numIters = diagCG_maxIters
        CALL DIAG_CG2D(
     I                aW2d, aS2d, b2d,
     I                diagCG_pcOffDFac, residCriter,
     O                firstResidual, minResidual, lastResidual,
     U                x2d, numIters,
     O                nIterMin,
     I                diagCG_prtResFrq, myThid )

        IF ( debugLevel.GE.debLevA ) THEN
          _BEGIN_MASTER( myThid )
          WRITE(standardMessageUnit,'(A,I4,A,2I6,A,1P3E14.7)')
     &    ' diag_cg2d : k=', k, ' , it=', nIterMin, numIters,
     &    ' ; ini,min,last_Res=',firstResidual,minResidual,lastResidual
          _END_MASTER( myThid )
        ENDIF

        _EXCH_XY_RL( x2d, myThid )

C-    Un-normalise the answer
        IF (diagNormaliseRHS) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              x2d(i,j,bi,bj) =  x2d(i,j,bi,bj)*x2dNorm
             ENDDO
            ENDDO
           ENDDO
          ENDDO
        ENDIF

C-    Compte divergence-free transport:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy+1
           DO i = 1,sNx+1
            uTrans(i,j,bi,bj) = uTrans(i,j,bi,bj)
     &                        - ( x2d(i,j,bi,bj) - x2d(i-1,j,bi,bj) )
     &                         *recip_dxC(i,j,bi,bj)*dyG(i,j,bi,bj)
     &                         *drF(k)*hFacW(i,j,k,bi,bj)
     &                         *maskInW(i,j,bi,bj)
            vTrans(i,j,bi,bj) = vTrans(i,j,bi,bj)
     &                        - ( x2d(i,j,bi,bj) - x2d(i,j-1,bi,bj) )
     &                         *recip_dyC(i,j,bi,bj)*dxG(i,j,bi,bj)
     &                         *drF(k)*hFacS(i,j,k,bi,bj)
     &                         *maskInS(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL DIAG_CALC_PSIVEL(
     I                         k, iPsi0, jPsi0, uTrans, vTrans,
     O                         psiVel, psiLoc,
     I                         prtFirstCall, myTime, myIter, myThid )

        _BEGIN_MASTER( myThid)
        IF ( useCubedSphereExchange .AND.
     &       diag_mdsio .AND. myProcId.EQ.0 ) THEN
C-      Missing-corner value are not written in MDS output file
C       Write separately these 2 values (should be part of DIAGNOSTICS_OUT)
         IF ( diagLoc_ioUnit.EQ.0 ) THEN
          CALL MDSFINDUNIT( diagLoc_ioUnit, myThid )
          WRITE(dataFName,'(2A,I10.10,A)')
     &         'diags_CScorners', '.', nIter0, '.txt'
          OPEN( diagLoc_ioUnit, FILE=dataFName, STATUS='unknown' )
          iL = ILNBLNK(dataFName)
          WRITE(msgBuf,'(2A,I6,2A)') 'DIAGNOSTICS_CALC_PHIVEL: ',
     &         'open unit=',diagLoc_ioUnit, ', file: ',dataFName(1:iL)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         ENDIF
         IF ( diagLoc_ioUnit.GT.0 ) THEN
          WRITE(diagLoc_ioUnit,'(1P2E18.10,A,2I4,I8,A,2I4,I6,2A)')
     &      psiLoc, ' #', k, lm, myIter,
     &      ' :',listId, md, ndId, ' ', cdiag(ndId)
C-       check accuracy (f1.SW-corner = f6.NW-corner = f5-NE-corner)
c         WRITE(0,'(1P2E18.10,A,2I4,I8)')
c    &         psiVel(1,1+sNy,nSx,nSy)- psiVel(1,1,1,1),
c    &     psiVel(1+sNx,1+sNy,nSx-1,nSy)-psiVel(1,1,1,1),
c    &      ' #', k, lm, myIter
         ENDIF
        ENDIF
        IF ( prtFirstCall ) prtFirstCall = .FALSE.
        _END_MASTER( myThid)

C-    Put the results back in qtmp[1,2]
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1,sNy+1
           DO i = 1,sNx+1
              qtmp1(i,j,ks,bi,bj) =  x2d(i,j,bi,bj)
              qtmp2(i,j,ks,bi,bj) =  psiVel(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('DIAGNOSTICS_CALC_PHIVEL',myThid)
#endif

      RETURN
      END
