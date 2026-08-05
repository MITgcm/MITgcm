#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_CALC_PSI_BVP
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_PSI_BVP(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_PSI_BVP
C     | o Calculate stream-functions for GM bolus velocity using
C     |   the BVP in Ferrari et al. (OM, 2010)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi,bj   :: Tile indices
C   iMin,iMax :: computation domain 1rst index bounds
C   jMin,jMax :: computation domain 2nd  index bounds
C     sigmaX  :: Zonal      gradient of density
C     sigmaY  :: Meridional gradient of density
C     sigmaR  :: Vertical   gradient of Pot.density (locally referenced)
C     myThid  :: My Thread Id number
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid
CEOP

#ifdef GM_BOLUS_ADVEC
#ifdef GM_BOLUS_BVP

C     !LOCAL VARIABLES:
      INTEGER i,j,k, km1
      INTEGER errCode
      _RL half_K
      _RL sigmaX_W
      _RL sigmaY_W
      _RL dSigmaDrW
      _RL dSigmaDrS
      _RL wkb_cW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wkb_cS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rPI, c2
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpFac
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      PARAMETER ( rPI    = 0.318309886183791 _d 0 )

C-    Matrix elements for tri-diagonal solver
      _RL GM_a3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL GM_b3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL GM_c3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C-    Initialization : <= done in S/R gmredi_init
      IF (GM_UseBVP) THEN

C     Initialize the WKB wave speeds to zero
C     We use c = int_{-H}^0 N dz/(GM_BVP_ModeNumber*PI) and have absorbed
C     a factor of g/rhoConst
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          wkb_cW(i,j) = 0. _d 0
          wkb_cS(i,j) = 0. _d 0
        ENDDO
       ENDDO

C      Surface BC : set to zero
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          GM_PsiX(i,j,1,bi,bj) = 0. _d 0
          GM_PsiY(i,j,1,bi,bj) = 0. _d 0
        ENDDO
       ENDDO

C      Initialise matrix to identity
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          GM_a3d(i,j,k) = 0. _d 0
          GM_b3d(i,j,k) = 1. _d 0
          GM_c3d(i,j,k) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO

       DO k=2,Nr
        km1 = k-1
        half_K = GM_background_K
     &         *(GM_bolFac1d(km1)+GM_bolFac1d(k))*op25
C      Gradient of Sigma below U and V points
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          sigmaX_W = op5*( sigmaX(i,j,km1)+sigmaX(i,j,k) )
     &                  *maskW(i,j,km1,bi,bj)*maskW(i,j,k,bi,bj)
          dSigmaDrW = op5*( sigmaR(i-1,j,k)+sigmaR(i,j,k) )
     &                  *maskW(i,j,km1,bi,bj)*maskW(i,j,k,bi,bj)

          wkb_cW(i,j) = wkb_cW(i,j)
     &                + SQRT(MAX( -dSigmaDrW, 0. _d 0 ))
     &                 *drC(k)*GM_BVP_rModeNumber*rPI

C      Part of main diagonal coming from zeroth order derivative
          GM_b3d(i,j,k) = MAX( -dSigmaDrW, GM_Small_Number )

C      This is initially the RHS
          GM_PsiX(i,j,k,bi,bj) = half_K*sigmaX_W
     &       *(GM_bolFac2d(i-1,j,bi,bj)+GM_bolFac2d(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDDO

C Note: Use Dirichlet BC @ Surface & Bottom (whereas we use Neumann BC for
C       implicit diffusion/advection Pb).
C     Surface BC implementation: => keep non zero coeff @ k=2
C                               and set Psi=1 with Identity coeff @ k=1
C     Same for bottom, except if kBottom=Nr (solver only process k=1:Nr)
C       should substract c3d(k=Nr)*Psi(k=Nr+1) to RHS @ k=Nr ;
C       However in our case this term is zero since Psi(k=Nr+1)=0
       DO k=2,Nr
        km1 = k-1
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          IF ( maskW(i,j,km1,bi,bj).NE.zeroRS .AND.
     &         maskW(i,j, k, bi,bj).NE.zeroRS ) THEN
            c2 = MAX( wkb_cW(i,j)*wkb_cW(i,j), GM_BVP_cHat2Min )
            GM_a3d(i,j,k) = -c2*recip_drC(k)
     &                      *recip_drF(km1)*recip_hFacW(i,j,km1,bi,bj)
            GM_b3d(i,j,k) = GM_b3d(i,j,k)
     &                    + c2*recip_drC(k)
     &                     *(recip_drF(km1)*recip_hFacW(i,j,km1,bi,bj)
     &                      +recip_drF(k)*recip_hFacW(i,j,k,bi,bj) )
            GM_c3d(i,j,k) = -c2*recip_drC(k)
     &                      *recip_drF(k)*recip_hFacW(i,j,k,bi,bj)
          ELSE
            GM_b3d(i,j,k) = 1. _d 0
          ENDIF
         ENDDO
        ENDDO
       ENDDO

       errCode = -1
       CALL SOLVE_TRIDIAGONAL( iMin+1, iMax, jMin, jMax,
     &                         GM_a3d, GM_b3d, GM_c3d,
     &                         GM_PsiX(1-OLx,1-OLy,1,bi,bj),
     &                         errCode, bi, bj, myThid )

       IF ( errCode .GT. 0 ) THEN
        WRITE(msgBuf,'(A)')
     &  'S/R GMREDI_CALC_PSI_BVP: matrix singular for PsiX'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GMREDI_CALC_PSI_BVP'
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C      Reset matrix to identity
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          GM_a3d(i,j,k) = 0. _d 0
          GM_b3d(i,j,k) = 1. _d 0
          GM_c3d(i,j,k) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
C      cut k loop in 2 to prevent bad optimisation with some compilers
       DO k=2,Nr
        km1 = k-1
        half_K = GM_background_K
     &         *(GM_bolFac1d(km1)+GM_bolFac1d(k))*op25
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          sigmaY_W = op5*( sigmaY(i,j,km1)+sigmaY(i,j,k) )
     &                  *maskS(i,j,km1,bi,bj)*maskS(i,j,k,bi,bj)
          dSigmaDrS = op5*( sigmaR(i,j-1,k)+sigmaR(i,j,k) )
     &                  *maskS(i,j,km1,bi,bj)*maskS(i,j,k,bi,bj)

          wkb_cS(i,j) = wkb_cS(i,j)
     &                + SQRT(MAX( -dSigmaDrS, 0. _d 0 ))
     &                 *drC(k)*GM_BVP_rModeNumber*rPI

C      Part of main diagonal coming from zeroth order derivative
          GM_b3d(i,j,k) = MAX( -dSigmaDrS, GM_Small_Number )

C      This is initially the RHS
          GM_PsiY(i,j,k,bi,bj) = half_K*sigmaY_W
     &       *(GM_bolFac2d(i,j-1,bi,bj)+GM_bolFac2d(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDDO

       DO k=2,Nr
        km1 = k-1
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( maskS(i,j,km1,bi,bj).NE.zeroRS .AND.
     &         maskS(i,j, k, bi,bj).NE.zeroRS ) THEN
            c2 = MAX( wkb_cS(i,j)*wkb_cS(i,j), GM_BVP_cHat2Min )
            GM_a3d(i,j,k) = -c2*recip_drC(k)
     &                      *recip_drF(km1)*recip_hFacS(i,j,km1,bi,bj)
            GM_b3d(i,j,k) = GM_b3d(i,j,k)
     &                    + c2*recip_drC(k)
     &                     *(recip_drF(km1)*recip_hFacS(i,j,km1,bi,bj)
     &                      +recip_drF(k)*recip_hFacS(i,j,k,bi,bj) )
            GM_c3d(i,j,k) = -c2*recip_drC(k)
     &                      *recip_drF(k)*recip_hFacS(i,j,k,bi,bj)
          ELSE
            GM_b3d(i,j,k) = 1. _d 0
          ENDIF
         ENDDO
        ENDDO
       ENDDO

       errCode = -1
       CALL SOLVE_TRIDIAGONAL( iMin, iMax, jMin+1, jMax,
     &                         GM_a3d, GM_b3d, GM_c3d,
     &                         GM_PsiY(1-OLx,1-OLy,1,bi,bj),
     &                         errCode, bi, bj, myThid )

       IF ( errCode .GT. 0 ) THEN
        WRITE(msgBuf,'(A)')
     &  'S/R GMREDI_CALC_PSI_BVP: matrix singular for PsiY'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R GMREDI_CALC_PSI_BVP'
       ENDIF

#ifdef ALLOW_DIAGNOSTICS
C     Write some diagnostics
       IF ( useDiagnostics ) THEN
        tmpFac = SQRT(gravity/rhoConst)
        CALL DIAGNOSTICS_SCALE_FILL( wkb_cW, tmpFac, 1, 'GM_BVPcW',
     &                               0, 1, 2, bi, bj, myThid )
        CALL DIAGNOSTICS_SCALE_FILL( wkb_cS, tmpFac, 1, 'GM_BVPcS',
     &                               0, 1, 2, bi, bj, myThid )
       ENDIF
#endif

      ENDIF
#endif /* GM_BOLUS_BVP */
#endif /* GM_BOLUS_ADVEC */

      RETURN
      END
