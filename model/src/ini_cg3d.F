C $Header: /u/gcmpack/MITgcm/model/src/ini_cg3d.F,v 1.21 2006/10/17 18:52:34 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_CG3D
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
#include "SURFACE.h"
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
C     bi,bj  - Loop counters
C     I,J,K  - Loop counters
C     faceArea - Temporary used to hold cell face areas.
C     myNorm - Work variable used in clculating normalisation factor
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj
      INTEGER I, J, K, ks
      _RL     faceArea
      _RS     myNorm
      _RL     theRecip_Dr
      _RL     aU, aL, aW, aE, aN, aS, aC
      _RL     tmpFac, nh_Fac, igwFac
CEOP

CcnhDebugStarts
c     _RL    phi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CcnhDebugEnds

C--   Initialise to zero over the full range of indices
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
C-       From common bloc CG3D_R:
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           aW3d(I,J,K,bi,bj) = 0.
           aS3d(I,J,K,bi,bj) = 0.
           aV3d(I,J,K,bi,bj) = 0.
           aC3d(I,J,K,bi,bj) = 0.
           zMC (I,J,K,bi,bj) = 0.
           zML (I,J,K,bi,bj) = 0.
           zMU (I,J,K,bi,bj) = 0.
          ENDDO
         ENDDO
C-       From common bloc CG3D_WK_R:
         DO J=0,sNy+1
          DO I=0,sNx+1
           cg3d_q(I,J,K,bi,bj) = 0.
           cg3d_r(I,J,K,bi,bj) = 0.
           cg3d_s(I,J,K,bi,bj) = 0.
          ENDDO
         ENDDO
C-       From common bloc SFP3D_COMMON_R:
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           cg3d_b(I,J,K,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      nh_Fac = 0.
      igwFac = 0.
      IF ( nonHydrostatic
     &      .AND. nh_Am2.NE.0. ) nh_Fac = 1. _d 0 / nh_Am2
      IF ( implicitIntGravWave ) igwFac = 1. _d 0

      IF ( use3Dsolver ) THEN
C--   Initialise laplace operator
C     aW3d: Ax/dX
C     aS3d: Ay/dY
C     aV3d: Ar/dR
      myNorm = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx+1
           faceArea = _dyG(I,J,bi,bj)*drF(K)
     &                *_hFacW(I,J,K,bi,bj)
           aW3d(I,J,K,bi,bj) = faceArea*recip_dxC(I,J,bi,bj)
           myNorm = MAX(ABS(aW3d(I,J,K,bi,bj)),myNorm)
          ENDDO
         ENDDO
         DO J=1,sNy+1
          DO I=1,sNx
           faceArea = _dxG(I,J,bi,bj)*drF(K)
     &                *_hFacS(I,J,K,bi,bj)
           aS3d(I,J,K,bi,bj) = faceArea*recip_dyC(I,J,bi,bj)
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
         tmpFac = nh_Fac*recip_horiVertRatio*recip_horiVertRatio
     &          + igwFac*dBdrRef(k)*deltaTMom*dTtracerLev(k)
         IF (tmpFac.GT.0. ) tmpFac = 1. _d 0 / tmpFac
         DO J=1,sNy
          DO I=1,sNx
           faceArea = _rA(I,J,bi,bj)*maskC(I,J, K ,bi,bj)
     &                              *maskC(I,J,K-1,bi,bj)
           theRecip_Dr = recip_drC(K)
c          theRecip_Dr = 
caja &      drF(K  )*_hFacC(i,j,k  ,bi,bj)*0.5
caja &     +drF(K-1)*_hFacC(i,j,k-1,bi,bj)*0.5
c          IF ( theRecip_Dr .NE. 0. ) 
c    &      theRecip_Dr = 1. _d 0/theRecip_Dr
           aV3d(I,J,K,bi,bj) = faceArea*theRecip_Dr*tmpFac
           myNorm = MAX(ABS(aV3d(I,J,K,bi,bj)),myNorm)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_OBCS
        IF ( useOBCS ) THEN
         DO K=1,Nr
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
         ENDDO
        ENDIF
#endif
       ENDDO
      ENDDO
      _GLOBAL_MAX_R4( myNorm, myThid )
      IF ( myNorm .NE. 0. _d 0 ) THEN
       myNorm = 1. _d 0/myNorm
      ELSE
       myNorm = 1. _d 0
      ENDIF
      _BEGIN_MASTER( myThid )
       cg3dNorm = myNorm
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
C-    Set solver main diagonal term
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           aW = aW3d(I  ,J,K,bi,bj)
           aE = aW3d(I+1,J,K,bi,bj)
           aN = aS3d(I,J+1,K,bi,bj)
           aS = aS3d(I,J  ,K,bi,bj)
           aU = aV3d(I,J,K,bi,bj)
           IF ( K .NE. Nr  ) THEN
            aL = aV3d(I,J,K+1,bi,bj)
           ELSE
            aL = 0.
           ENDIF
           aC3d(I,J,K,bi,bj) = -aW-aE-aN-aS-aU-aL
          ENDDO
         ENDDO
        ENDDO
C-    Add free-surface source term
        DO J=1,sNy
         DO I=1,sNx
           ks = ksurfC(I,J,bi,bj)
           IF ( ks.LE.Nr ) THEN
             aC3d(I,J,ks,bi,bj) = aC3d(I,J,ks,bi,bj)
     &         -freeSurfFac*recip_Bo(I,J,bi,bj)
     &          *rA(I,J,bi,bj)/deltaTMom/deltaTfreesurf
           ENDIF
         ENDDO
        ENDDO
C-    Matrix solver normalisation
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           aW3d(I,J,K,bi,bj) = aW3d(I,J,K,bi,bj)*myNorm
           aS3d(I,J,K,bi,bj) = aS3d(I,J,K,bi,bj)*myNorm
           aV3d(I,J,K,bi,bj) = aV3d(I,J,K,bi,bj)*myNorm
           aC3d(I,J,K,bi,bj) = aC3d(I,J,K,bi,bj)*myNorm
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlap regions
c     _EXCH_XYZ_R4(aW3d, myThid)
c     _EXCH_XYZ_R4(aS3d, myThid)
      CALL EXCH_UV_XYZ_RS(aW3d,aS3d,.FALSE.,myThid)
      _EXCH_XYZ_R4(aV3d, myThid)
      _EXCH_XYZ_R4(aC3d, myThid)
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
           IF ( aC3d(I,J,K,bi,bj) .NE. 0. ) THEN
            zMC(i,j,k,bi,bj) = aC3d(I,J,K,bi,bj)
            zML(i,j,k,bi,bj) = aV3d(I,J,K,bi,bj)
            zMU(i,j,k,bi,bj) = 0.
            IF ( K.NE.Nr )
     &      zMU(i,j,k,bi,bj) = aV3d(I,J,K+1,bi,bj)
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
            aU = 0.
           ENDIF
           IF ( K .NE. Nr ) THEN
            aL = aV3d(I,J,K+1,bi,bj)
           ELSE
            aL = 0.
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
c     DO k=1,Nr
c     DO j=1-OLy,sNy+OLy
c     DO i=1-OLx,sNx+OLx
c      phi(i,j,1,1) = zMc(i,j,k,1,1)
c     ENDDO
c     ENDDO
C     CALL PLOT_FIELD_XYRS( phi, 'zMC INI_CG3D.1' , 1, myThid )
c     ENDDO
C     CALL PLOT_FIELD_XYRS( zMU, 'zMU INI_CG3D.1' , Nr, 1, myThid )
C     CALL PLOT_FIELD_XYRS( zML, 'zML INI_CG3D.1' , Nr, 1, myThid )
CcnhDebugEnds

C--   end if (use3Dsolver)
      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
