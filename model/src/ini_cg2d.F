C $Header: /u/gcmpack/MITgcm/model/src/ini_cg2d.F,v 1.27 2000/03/14 17:47:25 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_CG2D( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_CG2D                                      |
C     | o Initialise 2d conjugate gradient solver operators.     |
C     |==========================================================|
C     | These arrays are purely a function of the basin geom.    |
C     | We set then here once and them use then repeatedly.      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "CG2D.h"
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     xG, yG - Global coordinate location.
C     zG
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     faceArea - Temporary used to hold cell face areas.
C     I,J,K
C     myNorm - Work variable used in clculating normalisation factor
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj
      INTEGER I,  J, K
      _RL     faceArea
      _RS     myNorm
      _RL     aC, aCw, aCs

C--   Initialise laplace operator
C     aW2d: integral in Z Ax/dX
C     aS2d: integral in Z Ay/dY
      myNorm = 0. _d 0
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
     &      gBaro*faceArea*recip_dxC(I,J,bi,bj)
           faceArea = _dxG(I,J,bi,bj)*drF(K)
     &               *_hFacS(I,J,K,bi,bj)
           aS2d(I,J,bi,bj) = aS2d(I,J,bi,bj) + 
     &      gBaro*faceArea*recip_dyC(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_OBCS
        IF (openBoundaries) THEN
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
          myNorm = MAX(ABS(aW2d(I,J,bi,bj)),myNorm)
          myNorm = MAX(ABS(aS2d(I,J,bi,bj)),myNorm)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _GLOBAL_MAX_R4( myNorm, myThid )
      IF ( myNorm .NE. 0. _d 0 ) THEN
       myNorm = 1. _d 0/myNorm
      ELSE
       myNorm = 1. _d 0
      ENDIF
       cg2dNorm = myNorm
      _BEGIN_MASTER( myThid )
CcnhDebugStarts
       WRITE(msgBuf,'(A,E40.25)') '// CG2D normalisation factor = ', 
     &               cg2dNorm
       CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
       WRITE(msgBuf,*) '                               '
       CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
CcnhDebugEnds
      _END_MASTER( myThid )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          aW2d(I,J,bi,bj) = aW2d(I,J,bi,bj)*myNorm
          aS2d(I,J,bi,bj) = aS2d(I,J,bi,bj)*myNorm
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      
C--   Update overlap regions
CcnhDebugStarts
C     CALL PLOT_FIELD_XYRS( aW2d, 'AW2D INI_CG2D.1' , 1, myThid )
C     CALL PLOT_FIELD_XYRS( aS2d, 'AS2D INI_CG2D.1' , 1, myThid )
CcnhDebugEnds
      _EXCH_XY_R4(aW2d, myThid)
      _EXCH_XY_R4(aS2d, myThid)
CcnhDebugStarts
C     CALL PLOT_FIELD_XYRS( aW2d, 'AW2D INI_CG2D.2' , 1, myThid )
C     CALL PLOT_FIELD_XYRS( aS2d, 'AS2D INI_CG2D.2' , 1, myThid )
CcnhDebugEnds

C--   Initialise preconditioner
C     Note. 20th May 1998
C           I made a weird discovery! In the model paper we argue
C           for the form of the preconditioner used here ( see
C           A Finite-volume, Incompressible Navier-Stokes Model
C           ...., Marshall et. al ). The algebra gives a simple
C           0.5 factor for the averaging of ac and aCw to get a
C           symmettric pre-conditioner. By using a factor of 0.51
C           i.e. scaling the off-diagonal terms in the
C           preconditioner down slightly I managed to get the
C           number of iterations for convergence in a test case to
C           drop form 192 -> 134! Need to investigate this further!
C           For now I have introduced a parameter cg2dpcOffDFac which
C           defaults to 0.51 but can be set at runtime.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
          pC(I,J,bi,bj) = 1. _d 0
          aC = -(
     &     aW2d(I,J,bi,bj) + aW2d(I+1,J  ,bi,bj)
     &    +aS2d(I,J,bi,bj) + aS2d(I  ,J+1,bi,bj)
     &    +freeSurfFac*myNorm* horiVertRatio*
     &     rA(I,J,bi,bj)/deltaTMom/deltaTMom
     &    )
          aCs = -(
     &     aW2d(I,J-1,bi,bj) + aW2d(I+1,J-1,bi,bj)
     &    +aS2d(I,J-1,bi,bj) + aS2d(I  ,J  ,bi,bj)
     &    +freeSurfFac*myNorm* horiVertRatio*
     &     rA(I,J-1,bi,bj)/deltaTMom/deltaTMom
     &    )
          aCw = -(
     &     aW2d(I-1,J,bi,bj) + aW2d(I  ,J  ,bi,bj)
     &    +aS2d(I-1,J,bi,bj) + aS2d(I-1,J+1,bi,bj)
     &    +freeSurfFac*myNorm* horiVertRatio*
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
C         pC(I,J,bi,bj) = 1.
C         pW(I,J,bi,bj) = 0.
C         pS(I,J,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C--   Update overlap regions
      _EXCH_XY_R4(pC, myThid)
      _EXCH_XY_R4(pW, myThid)
      _EXCH_XY_R4(pS, myThid)
CcnhDebugStarts
C     CALL PLOT_FIELD_XYRS( pC, 'pC   INI_CG2D.2' , 1, myThid )
C     CALL PLOT_FIELD_XYRS( pW, 'pW   INI_CG2D.2' , 1, myThid )
C     CALL PLOT_FIELD_XYRS( pS, 'pS   INI_CG2D.2' , 1, myThid )
CcnhDebugEnds

C--   Set default values for initial guess and RHS
      IF ( startTime .EQ. 0 ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
           cg2d_x(I,J,bi,bj) = 0. _d 0
           cg2d_b(I,J,bi,bj) = 0. _d 0
#ifdef INCLUDE_CD_CODE
           cg2d_xNM1(I,J,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C--    Update overlap regions
       _EXCH_XY_R8(cg2d_x, myThid)
       _EXCH_XY_R8(cg2d_b, myThid)
#ifdef INCLUDE_CD_CODE
       _EXCH_XY_R8(cg2d_xNM1, myThid)
#endif
      ENDIF

      RETURN
      END
