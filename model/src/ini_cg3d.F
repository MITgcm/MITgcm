C $Header: /u/gcmpack/MITgcm/model/src/ini_cg3d.F,v 1.13 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CG2D
C     !INTERFACE:
      SUBROUTINE INI_CG3D( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_CG3D                                       
C     | o Initialise 3d conjugate gradient solver operators.      
C     *==========================================================*
C     | These arrays are purely a function of the basin geom.     
C     | We set then here once and them use then repeatedly.       
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c_jmc #include "DYNVARS.h"
#include "CG3D.h"
#include "SOLVE_FOR_PRESSURE3D.h"
#ifdef ALLOW_OBCS
#include "OBCS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC

C     !LOCAL VARIABLES:
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
      _RL     aCw, aCs
      _RL     theRecip_Dr
      _RL     aU, aL, aW, aE, aN, aS, aC
CEOP

CcnhDebugStarts
      _RL    phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CcnhDebugEnds

C--   Initialise laplace operator
C     aW3d: Ax/dX
C     aS3d: Ay/dY
C     aV3d: Ar/dR
      myNorm = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           faceArea = _dyG(I,J,bi,bj)*drF(K)
     &                *_hFacW(I,J,K,bi,bj)
           aW3d(I,J,K,bi,bj) = faceArea*recip_dxC(I,J,bi,bj)
           faceArea = _dxG(I,J,bi,bj)*drF(K)
     &                *_hFacS(I,J,K,bi,bj)
           aS3d(I,J,K,bi,bj) = faceArea*recip_dyC(I,J,bi,bj)
           myNorm = MAX(ABS(aW3d(I,J,K,bi,bj)),myNorm)
           myNorm = MAX(ABS(aS3d(I,J,K,bi,bj)),myNorm)
          ENDDO
         ENDDO
        ENDDO
        DO K=1,1
         DO J=1,sNy
          DO I=1,sNx
           aV3d(I,J,K,bi,bj) =  0.
           myNorm = MAX(ABS(aV3d(I,J,K,bi,bj)),myNorm)
          ENDDO
         ENDDO
        ENDDO
        DO K=2,Nr
         DO J=1,sNy
          DO I=1,sNx
           IF ( _hFacC(i,j,k,bi,bj) .EQ. 0. ) THEN
            faceArea = 0.
           ELSE
            faceArea = _rA(I,J,bi,bj)
           ENDIF
           theRecip_Dr = 
     &      drC(K)
caja &      drF(K  )*_hFacC(i,j,k  ,bi,bj)*0.5
caja &     +drF(K-1)*_hFacC(i,j,k-1,bi,bj)*0.5
           IF ( theRecip_Dr .NE. 0. ) 
     &      theRecip_Dr = 1. _d 0/theRecip_Dr
           aV3d(I,J,K,bi,bj) = faceArea*theRecip_Dr
     &                 *horiVertRatio*horiVertRatio 
           myNorm = MAX(ABS(aV3d(I,J,K,bi,bj)),myNorm)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_OBCS
        DO K=1,Nr
         IF (useOBCS) THEN
          DO I=1,sNx
           IF (OB_Jn(I,bi,bj).NE.0) THEN
            aS3d(I,OB_Jn(I,bi,bj),K,bi,bj)=0.
            aS3d(I,OB_Jn(I,bi,bj)+1,K,bi,bj)=0.
            aW3d(I,OB_Jn(I,bi,bj),K,bi,bj)=0.
            aW3d(I+1,OB_Jn(I,bi,bj),K,bi,bj)=0.
            aV3d(I,OB_Jn(I,bi,bj),K,bi,bj)=0.
           ENDIF
           IF (OB_Js(I,bi,bj).NE.0) THEN
            aS3d(I,OB_Js(I,bi,bj)+1,K,bi,bj)=0.
            aS3d(I,OB_Js(I,bi,bj),K,bi,bj)=0.
            aW3d(I,OB_Js(I,bi,bj),K,bi,bj)=0.
            aW3d(I+1,OB_Js(I,bi,bj),K,bi,bj)=0.
            aV3d(I,OB_Js(I,bi,bj),K,bi,bj)=0.
           ENDIF
          ENDDO
          DO J=1,sNy
           IF (OB_Ie(J,bi,bj).NE.0) THEN
            aW3d(OB_Ie(J,bi,bj),J,K,bi,bj)=0.
            aW3d(OB_Ie(J,bi,bj)+1,J,K,bi,bj)=0.
            aS3d(OB_Ie(J,bi,bj),J,K,bi,bj)=0.
            aS3d(OB_Ie(J,bi,bj),J+1,K,bi,bj)=0.
            aV3d(OB_Ie(J,bi,bj),J,K,bi,bj)=0.
           ENDIF
           IF (OB_Iw(J,bi,bj).NE.0) THEN
            aW3d(OB_Iw(J,bi,bj)+1,J,K,bi,bj)=0.
            aW3d(OB_Iw(J,bi,bj),J,K,bi,bj)=0.
            aS3d(OB_Iw(J,bi,bj),J,K,bi,bj)=0.
            aS3d(OB_Iw(J,bi,bj),J+1,K,bi,bj)=0.
            aV3d(OB_Iw(J,bi,bj),J,K,bi,bj)=0.
           ENDIF
          ENDDO
         ENDIF
        ENDDO
#endif
       ENDDO
      ENDDO
      _GLOBAL_MAX_R4( myNorm, myThid )
      IF ( myNorm .NE. 0. _d 0 ) THEN
       myNorm = 1. _d 0/myNorm
      ELSE
       myNorm = 1. _d 0
      ENDIF
       cg3dNorm = myNorm
      _BEGIN_MASTER( myThid )
CcnhDebugStarts
       WRITE(msgBuf,'(A,E40.25)') '// CG3D normalisation factor = '
     &                , cg3dNorm
       CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
       WRITE(msgBuf,*) '                               '
       CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
CcnhDebugEnds
      _END_MASTER( myThid )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           aW3d(I,J,K,bi,bj) = aW3d(I,J,K,bi,bj)*myNorm
           aS3d(I,J,K,bi,bj) = aS3d(I,J,K,bi,bj)*myNorm
           aV3d(I,J,K,bi,bj) = aV3d(I,J,K,bi,bj)*myNorm
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      
C--   Update overlap regions
      _EXCH_XYZ_R4(aW3d, myThid)
      _EXCH_XYZ_R4(aS3d, myThid)
      _EXCH_XYZ_R4(aV3d, myThid)
CcnhDebugStarts
C     CALL PLOT_FIELD_XYZRS( aW3d, 'AW3D INI_CG3D.1' , Nr, 1, myThid )
C     CALL PLOT_FIELD_XYZRS( aS3d, 'AS3D INI_CG3D.1' , Nr, 1, myThid )
CcnhDebugEnds

C--   Initialise preconditioner
C     For now PC is just the identity. Change to 
C     be LU factorization of d2/dz2 later. Note
C     check for consistency with S/R CG3D before
C     assuming zML is lower and zMU is upper!
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr  
         DO J=1,sNy
          DO I=1,sNx
           aW = aW3d(I  ,J,K,bi,bj)
           aE = aW3d(I+1,J,K,bi,bj)
           aN = aS3d(I,J+1,K,bi,bj)
           aS = aS3d(I,J  ,K,bi,bj)
           IF ( K .NE. 1 ) THEN
            aU = aV3d(I,J,K,bi,bj)
           ELSE
            aU = 0
           ENDIF
           IF ( K .NE. Nr ) THEN
            aL = aV3d(I,J,K+1,bi,bj)
           ELSE
            aL = 0
           ENDIF
           aC = -aW-aE-aN-aS-aU-aL
           IF ( K .EQ. 1 .AND. aC .NE. 0. ) THEN
            aC = aC
     &         -freeSurfFac*myNorm*(horiVertRatio/gravity)*
     &          rA(I,J,bi,bj)/deltaTMom/deltaTMom
           ENDIF
           IF ( aC .NE. 0. ) THEN
            zMC(i,j,k,bi,bj) = aC
            zMU(i,j,k,bi,bj) = aL
            zML(i,j,k,bi,bj) = aU
CcnhDebugStarts
C           zMC(i,j,k,bi,bj) = 1.
C           zMU(i,j,k,bi,bj) = 0.
C           zML(i,j,k,bi,bj) = 0.
CcnhDebugEnds
           ELSE
            zMC(i,j,k,bi,bj) = 1. _d 0
            zMU(i,j,k,bi,bj) = 0.
            zML(i,j,k,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
          zMC(i,j,1,bi,bj)=
     &     1. _d 0 / zMC(i,j,1,bi,bj)
          zMU(i,j,1,bi,bj)=
     &     zMU(i,j,1,bi,bj)*zMC(i,j,1,bi,bj)
         ENDDO
        ENDDO
        DO K=2,Nr  
         DO J=1,sNy
          DO I=1,sNx
           zMC(i,j,k,bi,bj) = 1. _d 0 /
     &     (zMC(i,j,k,bi,bj)-zML(i,j,k,bi,bj)*zMU(i,j,k-1,bi,bj))
           zMU(i,j,k,bi,bj)=zMU(i,j,k,bi,bj)*zMC(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
        DO K=1,Nr  
         DO J=1,sNy
          DO I=1,sNx
           aW = aW3d(I  ,J,K,bi,bj)
           aE = aW3d(I+1,J,K,bi,bj)
           aN = aS3d(I,J+1,K,bi,bj)
           aS = aS3d(I,J  ,K,bi,bj)
           IF ( K .NE. 1 ) THEN
            aU = aV3d(I,J,K-1,bi,bj)
           ELSE
            aU = 0
           ENDIF
           IF ( K .NE. Nr ) THEN
            aL = aV3d(I,J,K+1,bi,bj)
           ELSE
            aL = 0
           ENDIF
           aC = -aW-aE-aN-aS-aU-aL
           IF ( aC .EQ. 0. ) THEN
            zMC(i,j,k,bi,bj) = 1.
            zML(i,j,k,bi,bj) = 0.
            zMU(i,j,k,bi,bj) = 0.
CcnhDebugStarts
C          ELSE
C           zMC(i,j,k,bi,bj) = 1.
C           zML(i,j,k,bi,bj) = 0.
C           zMU(i,j,k,bi,bj) = 0.
CcnhDEbugEnds
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C--   Update overlap regions
      _EXCH_XYZ_R4(zMC, myThid)
      _EXCH_XYZ_R4(zML, myThid)
      _EXCH_XYZ_R4(zMU, myThid)

CcnhDebugStarts
      DO k=1,nr
      DO j=1-OLy,sNy+OLy
      DO i=1-OLx,sNx+OLx
       phi(i,j,1,1) = zMc(i,j,k,1,1)
      ENDDO
      ENDDO
C     CALL PLOT_FIELD_XYRS( phi, 'zMC INI_CG3D.1' , 1, myThid )
      ENDDO
C     CALL PLOT_FIELD_XYRS( zMU, 'zMU INI_CG3D.1' , Nr, 1, myThid )
C     CALL PLOT_FIELD_XYRS( zML, 'zML INI_CG3D.1' , Nr, 1, myThid )
CcnhDebugEnds

C--   Set default values for initial guess and RHS
      IF ( startTime .EQ. 0 ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1-OLy,sNy+OLy
           DO I=1-OLx,sNx+OLx
            cg3d_b(I,J,K,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C--    Update overlap regions
       _EXCH_XYZ_R8(cg3d_b, myThid)
      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
