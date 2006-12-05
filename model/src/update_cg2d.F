C $Header: /u/gcmpack/MITgcm/model/src/update_cg2d.F,v 1.7 2006/12/05 05:25:08 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_CG2D
C     !INTERFACE:
      SUBROUTINE UPDATE_CG2D( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_CG2D
C     | o Update 2d conjugate gradient solver operators
C     |   account for Free-Surf effect on total column thickness
C     *==========================================================*
C     | This routine is based on INI_CG2D, and simplified. It is
C     | only active when the non-linear free surface mode of
C     | equations is active.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "CG2D.h"
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C-- Note : compared to "INI_CG2D", no needs to compute again
C   the solver norn=malisation factor of the solver tolerance
C     === Local variables ===
C     bi,bj  :: tile indices
C     I,J,K  :: Loop counters
C     faceArea :: Temporary used to hold cell face areas.
      INTEGER bi, bj
      INTEGER I, J, K, ks
      _RL     faceArea
      _RL     pW_tmp, pS_tmp
      LOGICAL updatePreCond
CEOP

C--   Decide when to update cg2d Preconditioner :
      IF ( cg2dPreCondFreq.EQ.0 ) THEN
        updatePreCond = .FALSE.
      ELSE
        updatePreCond = ( myIter.EQ.nIter0 )
        IF ( MOD(myIter,cg2dPreCondFreq).EQ.0 ) updatePreCond=.TRUE.
      ENDIF

C--   Initialise laplace operator
C     aW2d: integral in Z Ax/dX
C     aS2d: integral in Z Ay/dY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy+1
         DO I=1,sNx+1
          aW2d(I,J,bi,bj) = 0. _d 0
          aS2d(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        DO K=1,Nr
         DO J=1,sNy+1
          DO I=1,sNx+1
C  deep-model: *deepFacC (faceArea), /deepFacC (recip_dx,y): => no net effect
           faceArea = _dyG(I,J,bi,bj)*drF(K)
     &               *_hFacW(I,J,K,bi,bj)
           aW2d(I,J,bi,bj) = aW2d(I,J,bi,bj)
     &              + faceArea*recip_dxC(I,J,bi,bj)
           faceArea = _dxG(I,J,bi,bj)*drF(K)
     &               *_hFacS(I,J,K,bi,bj)
           aS2d(I,J,bi,bj) = aS2d(I,J,bi,bj)
     &              + faceArea*recip_dyC(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_OBCS
        IF (useOBCS) THEN
C  Note: would need loop from 1 to sNx+1 (and below, from 1 to sNy+1)
C    to get the same solver-matrix as from INI_CG2D,
C    since aS2d & aW2d are not exchanged here (but in INI_CG2D, they are).
         DO I=1,sNx
          IF (OB_Jn(I,bi,bj).NE.0) aS2d(I,OB_Jn(I,bi,bj),bi,bj)=0.
          IF (OB_Jn(I,bi,bj).NE.0) aS2d(I,OB_Jn(I,bi,bj)+1,bi,bj)=0.
          IF (OB_Js(I,bi,bj).NE.0) aS2d(I,OB_Js(I,bi,bj)+1,bi,bj)=0.
          IF (OB_Js(I,bi,bj).NE.0) aS2d(I,OB_Js(I,bi,bj),bi,bj)=0.
         ENDDO
         DO J=1,sNy
          IF (OB_Ie(J,bi,bj).NE.0) aW2d(OB_Ie(J,bi,bj),J,bi,bj)=0.
          IF (OB_Ie(J,bi,bj).NE.0) aW2d(OB_Ie(J,bi,bj)+1,J,bi,bj)=0.
          IF (OB_Iw(J,bi,bj).NE.0) aW2d(OB_Iw(J,bi,bj)+1,J,bi,bj)=0.
          IF (OB_Iw(J,bi,bj).NE.0) aW2d(OB_Iw(J,bi,bj),J,bi,bj)=0.
         ENDDO
        ENDIF
#endif
        DO J=1,sNy+1
         DO I=1,sNx+1
          aW2d(I,J,bi,bj) = aW2d(I,J,bi,bj)*cg2dNorm
     &                     *implicSurfPress*implicDiv2DFlow
          aS2d(I,J,bi,bj) = aS2d(I,J,bi,bj)*cg2dNorm
     &                     *implicSurfPress*implicDiv2DFlow
         ENDDO
        ENDDO
C--   compute matrix main diagonal :
        IF ( deepAtmosphere ) THEN
         DO J=1,sNy
          DO I=1,sNx
           ks = ksurfC(I,J,bi,bj)
           aC2d(I,J,bi,bj) = -(
     &       aW2d(I,J,bi,bj) + aW2d(I+1,J  ,bi,bj)
     &      +aS2d(I,J,bi,bj) + aS2d(I  ,J+1,bi,bj)
     &      +freeSurfFac*cg2dNorm*recip_Bo(I,J,bi,bj)*deepFac2F(ks)
     &                  *rA(I,J,bi,bj)/deltaTMom/deltaTfreesurf
     &                        )
          ENDDO
         ENDDO
        ELSE
         DO J=1,sNy
          DO I=1,sNx
           aC2d(I,J,bi,bj) = -(
     &       aW2d(I,J,bi,bj) + aW2d(I+1,J  ,bi,bj)
     &      +aS2d(I,J,bi,bj) + aS2d(I  ,J+1,bi,bj)
     &      +freeSurfFac*cg2dNorm*recip_Bo(I,J,bi,bj)
     &                  *rA(I,J,bi,bj)/deltaTMom/deltaTfreesurf
     &                        )
          ENDDO
         ENDDO
        ENDIF
C-    end bi,bj loops
       ENDDO
      ENDDO

      IF ( updatePreCond ) THEN
C--   Update overlap regions
      CALL EXCH_XY_RS(aC2d, myThid)

C--   Initialise preconditioner
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy+1
         DO I=1,sNx+1
          IF ( aC2d(I,J,bi,bj) .EQ. 0. ) THEN
            pC(I,J,bi,bj) = 1. _d 0
          ELSE
           pC(I,J,bi,bj) =  1. _d 0 / aC2d(I,J,bi,bj)
          ENDIF
          pW_tmp = aC2d(I,J,bi,bj)+aC2d(I-1,J,bi,bj)
          IF ( pW_tmp .EQ. 0. ) THEN
           pW(I,J,bi,bj) = 0.
          ELSE
           pW(I,J,bi,bj) =
     &     -aW2d(I,J,bi,bj)/((cg2dpcOffDFac *pW_tmp)**2 )
          ENDIF
          pS_tmp = aC2d(I,J,bi,bj)+aC2d(I,J-1,bi,bj)
          IF ( pS_tmp .EQ. 0. ) THEN
           pS(I,J,bi,bj) = 0.
          ELSE
           pS(I,J,bi,bj) =
     &     -aS2d(I,J,bi,bj)/((cg2dpcOffDFac *pS_tmp)**2 )
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C-    if update Preconditioner : end
      ENDIF

#endif /* NONLIN_FRSURF */

      RETURN
      END
