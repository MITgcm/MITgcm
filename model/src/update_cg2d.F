C $Header: /u/gcmpack/MITgcm/model/src/update_cg2d.F,v 1.2 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

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
C     bi,bj  - Loop counters
C     I,J,K
C     faceArea - Temporary used to hold cell face areas.
      INTEGER bi, bj
      INTEGER I, J, K
      _RL     faceArea
      _RL     aC, aCw, aCs
CEOP

C--   Initialise laplace operator
C     aW2d: integral in Z Ax/dX
C     aS2d: integral in Z Ay/dY
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          aW2d(I,J,bi,bj) = 0. _d 0
          aS2d(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           faceArea = _dyG(I,J,bi,bj)*drF(K)
     &               *_hFacW(I,J,K,bi,bj)
           aW2d(I,J,bi,bj) = aW2d(I,J,bi,bj) + 
     &             faceArea*recip_dxC(I,J,bi,bj)
           faceArea = _dxG(I,J,bi,bj)*drF(K)
     &               *_hFacS(I,J,K,bi,bj)
           aS2d(I,J,bi,bj) = aS2d(I,J,bi,bj) + 
     &             faceArea*recip_dyC(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_OBCS
        IF (useOBCS) THEN
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
        DO J=1,sNy
         DO I=1,sNx
          aW2d(I,J,bi,bj) = aW2d(I,J,bi,bj)*cg2dNorm
     &                     *implicSurfPress*implicDiv2DFlow
          aS2d(I,J,bi,bj) = aS2d(I,J,bi,bj)*cg2dNorm
     &                     *implicSurfPress*implicDiv2DFlow
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      
C--   Update overlap regions
c     _EXCH_XY_R4(aW2d, myThid)
c     _EXCH_XY_R4(aS2d, myThid)
      CALL EXCH_UV_XY_RS(aW2d,aS2d,.FALSE.,myThid)

C--   Initialise preconditioner
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          pC(I,J,bi,bj) = 1. _d 0
          aC = -(
     &     aW2d(I,J,bi,bj) + aW2d(I+1,J  ,bi,bj)
     &    +aS2d(I,J,bi,bj) + aS2d(I  ,J+1,bi,bj)
     &    +freeSurfFac*cg2dNorm*recip_Bo(I,J,bi,bj)* 
     &     rA(I,J,bi,bj)/deltaTMom/deltaTMom
     &    )
          aCs = -(
     &     aW2d(I,J-1,bi,bj) + aW2d(I+1,J-1,bi,bj)
     &    +aS2d(I,J-1,bi,bj) + aS2d(I  ,J  ,bi,bj)
     &    +freeSurfFac*cg2dNorm*recip_Bo(I,J-1,bi,bj)* 
     &     rA(I,J-1,bi,bj)/deltaTMom/deltaTMom
     &    )
          aCw = -(
     &     aW2d(I-1,J,bi,bj) + aW2d(I  ,J  ,bi,bj)
     &    +aS2d(I-1,J,bi,bj) + aS2d(I-1,J+1,bi,bj)
     &    +freeSurfFac*cg2dNorm*recip_Bo(I-1,J,bi,bj)* 
     &     rA(I-1,J,bi,bj)/deltaTMom/deltaTMom
     &    )
          IF ( aC .EQ. 0. ) THEN
            pC(I,J,bi,bj) = 1. _d 0
          ELSE
           pC(I,J,bi,bj) =  1. _d 0 / aC
          ENDIF
          IF ( aC + aCw .EQ. 0. ) THEN
           pW(I,J,bi,bj) = 0.
          ELSE
           pW(I,J,bi,bj) = 
     &     -aW2d(I  ,J  ,bi,bj)/((cg2dpcOffDFac *(aCw+aC))**2 )
          ENDIF
          IF ( aC + aCs .EQ. 0. ) THEN
           pS(I,J,bi,bj) = 0.
          ELSE
           pS(I,J,bi,bj) =
     &     -aS2d(I  ,J  ,bi,bj)/((cg2dpcOffDFac *(aCs+aC))**2 )
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C--   Update overlap regions
      _EXCH_XY_R4(pC, myThid)
c     _EXCH_XY_R4(pW, myThid)
c     _EXCH_XY_R4(pS, myThid)
      CALL EXCH_UV_XY_RS(pW,pS,.FALSE.,myThid)

#endif /* NONLIN_FRSURF */

      RETURN
      END
