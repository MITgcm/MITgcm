C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_calc_phivel.F,v 1.3 2011/06/24 22:15:48 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_CALC_PHIVEL

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_CALC_PHIVEL(
     I                        listId, md, ndId, ip, im, lm,
     U                        qtmp1, qtmp2,
     I                        myTime, myIter, myThid )

C     !DESCRIPTION:
C     Compute Velocity Potential

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

      INTEGER NrMax
      PARAMETER( NrMax = numLevels )


C     !INPUT PARAMETERS:
C     listId  :: Diagnostics list number being written
C     md      :: field number in the list "listId".
C     ndId    :: diagnostics  Id number (in available diagnostics list)
C     ip      :: diagnostics  pointer to storage array
C     im      :: counter-mate pointer to storage array
C     lm      :: index in the averageCycle
C     qtmp1   :: horizontal velocity input diag., u-component
C     qtmp2   :: horizontal velocity input diag., v-component
C     myTime  :: current time of simulation (s)
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      INTEGER listId, md, ndId, ip, im, lm
      _RL     qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     qtmp2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     myTime
      INTEGER myIter, myThid

C     !OUTPUT PARAMETERS:
C     qtmp1   :: horizontal-velocity potential
C     qtmp2   :: horizontal-velocity stream-function
CEOP

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
      INTEGER i, j, k
      INTEGER bi, bj
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER ks
      INTEGER numIters
      LOGICAL normaliseMatrice, diagNormaliseRHS
      _RL  residCriter, firstResidual, lastResidual
      _RL  a2dMax, a2dNorm
      _RL  rhsMax, rhsNorm
      _RS  aW2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  aS2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  b2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  x2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO ks = 1,kdiag(ndId)
        k = NINT(levs(ks,listId))
C--   Solve for velocity potential for each level:

        a2dMax = 0. _d 0
        rhsMax = 0. _d 0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C-    Initialise fist guess & RHS
           DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx,sNx+Olx
              b2d(i,j,bi,bj) = 0.
              x2d(i,j,bi,bj) = 0.
            ENDDO
           ENDDO
C-    calculate cg2d matrix:
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

C-    calculate RHS = Div(uVel,vVel):
           DO j = 1,sNy+1
            DO i = 1,sNx+1
              uTrans(i,j) = dyG(i,j,bi,bj)*drF(k)
     &                     *qtmp1(i,j,ks,bi,bj)*maskInW(i,j,bi,bj)
              vTrans(i,j) = dxG(i,j,bi,bj)*drF(k)
     &                     *qtmp2(i,j,ks,bi,bj)*maskInS(i,j,bi,bj)
            ENDDO
           ENDDO
           DO j = 1,sNy
            DO i = 1,sNx
              b2d(i,j,bi,bj) = ( ( uTrans(i+1,j) - uTrans(i,j) )
     &                         + ( vTrans(i,j+1) - vTrans(i,j) )
     &                         )*maskInC(i,j,bi,bj)
              rhsMax = MAX(ABS(b2d(I,J,bi,bj)),rhsMax)
            ENDDO
           ENDDO

C-   end bi,bj loops
         ENDDO
        ENDDO

C-    Normalise Matrice & RHS :
        diagNormaliseRHS = cg2dTargetResWunit.LE.0.
        normaliseMatrice = .TRUE.
        diagNormaliseRHS = .TRUE.
        a2dNorm = 1. _d 0
        rhsNorm = 1. _d 0
        IF ( normaliseMatrice ) THEN
          _GLOBAL_MAX_RL( a2dMax, myThid )
          IF ( a2dMax .GE. 0. _d 0 ) a2dNorm = 1. _d 0/a2dMax
        ENDIF
        IF ( diagNormaliseRHS ) THEN
          _GLOBAL_MAX_RL( rhsMax, myThid )
          IF ( rhsMax .GE. 0. _d 0 ) rhsNorm = 1. _d 0/(a2dNorm*rhsMax)
          residCriter = cg2dTargetResidual
        ELSE
          residCriter = a2dNorm * cg2dTargetResWunit
     &                          * globalArea / deltaTmom
        ENDIF
        IF ( normaliseMatrice .OR. diagNormaliseRHS ) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1,sNy+1
             DO i = 1,sNx+1
              aW2d(i,j,bi,bj) = aW2d(i,j,bi,bj)*a2dNorm
              aS2d(i,j,bi,bj) = aS2d(i,j,bi,bj)*a2dNorm
              b2d(i,j,bi,bj)  = b2d(i,j,bi,bj) *a2dNorm*rhsNorm
c             x2d(i,j,bi,bj) =  x2d(i,j,bi,bj) *rhsNorm
             ENDDO
            ENDDO
           ENDDO
          ENDDO
        ENDIF

        IF ( debugLevel.GE.debLevA .AND. k.EQ.1 ) THEN
          _BEGIN_MASTER( myThid )
          WRITE(standardMessageUnit,'(A,I9,2(A,1P1E13.6),A,1P1E9.2)')
     &     ' diag_cg2d (it=', myIter,') a2dNorm,rhsNorm=', a2dNorm,
     &     ' ,', rhsNorm, ' ; Criter=', residCriter
          _END_MASTER( myThid )
        ENDIF

        numIters = cg2dMaxIters
        CALL DIAG_CG2D(
     I                aW2d, aS2d, b2d,
     I                residCriter,
     O                firstResidual, lastResidual,
     U                x2d, numIters,
     I                myThid )

        IF ( debugLevel.GE.debLevA ) THEN
          _BEGIN_MASTER( myThid )
          WRITE(standardMessageUnit,'(A,I4,A,I6,A,1P2E17.9)')
     &     ' diag_cg2d : k=', k, ' , iters=', numIters,
     &     ' ; ini,last_Res=', firstResidual, lastResidual
          _END_MASTER( myThid )
        ENDIF

c       _EXCH_XY_RL( x2d, myThid )

C-    Un-normalise the answer
        IF (diagNormaliseRHS) THEN
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1,sNy
             DO i = 1,sNx
c             x2d(i,j,bi,bj) =  x2d(i,j,bi,bj) /rhsNorm
              qtmp1(i,j,ks,bi,bj) =  x2d(i,j,bi,bj)/rhsNorm
             ENDDO
            ENDDO
           ENDDO
          ENDDO
        ENDIF

      ENDDO

      RETURN
      END
