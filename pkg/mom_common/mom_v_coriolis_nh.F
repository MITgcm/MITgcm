#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_CORIOLIS_NH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_CORIOLIS_NH(
     I               bi, bj, k, wFld,
     O               vCoriolisTerm,
     I               myThid )

C !DESCRIPTION:
C Calculates the 3.D Coriolis term in the meridional momentum equation:
C \begin{equation*}
C - \overline{ f_prime \overline{w}^{k} }^{i}
C \end{equation*}
C consistent with Non-Hydrostatic (or quasi-hydrostatic) formulation

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj               :: tile indices
C  k                    :: vertical level
C  wFld                 :: vertical flow
C  myThid               :: thread number
      INTEGER bi, bj, k
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vCoriolisTerm        :: Coriolis term
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j                 :: loop indices
      INTEGER i, j, kp1
      _RL wMsk
CEOP

      kp1 = MIN(k+1,Nr)
      wMsk = 1.
      IF (k.EQ.Nr) wMsk = 0.

      IF ( select3dCoriScheme.EQ.1 ) THEN
C-    Original discretization of 2*Omega*cos(phi)*w
C     documented as Energy conserving
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         vCoriolisTerm(i,j) = -gravitySign*halfRL
     &       *( fCoriCos(i, j ,bi,bj)*angleSinC(i, j ,bi,bj)*halfRL
     &           *( wFld(i, j , k ,bi,bj)*rVel2wUnit( k )
     &            + wFld(i, j ,kp1,bi,bj)*rVel2wUnit(kp1)*wMsk )
     &        + fCoriCos(i,j-1,bi,bj)*angleSinC(i,j-1,bi,bj)*halfRL
     &           *( wFld(i,j-1, k ,bi,bj)*rVel2wUnit( k )
     &            + wFld(i,j-1,kp1,bi,bj)*rVel2wUnit(kp1)*wMsk )
     &        )
        ENDDO
       ENDDO
      ELSE
C-    Using averaged transport:
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         vCoriolisTerm(i,j) = -gravitySign*halfRL
     &    *( fCoriCos(i, j ,bi,bj)*angleSinC(i, j ,bi,bj)
     &      *( wFld(i, j , k ,bi,bj)*rVel2wUnit( k )*deepFac2F( k )
     &       + wFld(i, j ,kp1,bi,bj)*rVel2wUnit(kp1)*deepFac2F(kp1)*wMsk
     &         )*rA(i, j ,bi,bj)*halfRL
     &     + fCoriCos(i,j-1,bi,bj)*angleSinC(i,j-1,bi,bj)
     &      *( wFld(i,j-1, k ,bi,bj)*rVel2wUnit( k )*deepFac2F( k )
     &       + wFld(i,j-1,kp1,bi,bj)*rVel2wUnit(kp1)*deepFac2F(kp1)*wMsk
     &         )*rA(i,j-1,bi,bj)*halfRL
     &     )*recip_rAs(i,j,bi,bj)*recip_deepFac2C(k)
     &      *recip_hFacS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
