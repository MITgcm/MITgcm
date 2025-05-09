#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_CORIOLIS_NH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_CORIOLIS_NH(
     I               bi, bj, k, wFld,
     O               uCoriolisTerm,
     I               myThid )

C !DESCRIPTION:
C Calculates the 3.D Coriolis term in the zonal momentum equation:
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
C  uCoriolisTerm        :: Coriolis term
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

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
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
         uCoriolisTerm(i,j) = gravitySign*halfRL
     &       *( fCoriCos( i ,j,bi,bj)*angleCosC( i ,j,bi,bj)*halfRL
     &           *( wFld( i ,j, k ,bi,bj)*rVel2wUnit( k )
     &            + wFld( i ,j,kp1,bi,bj)*rVel2wUnit(kp1)*wMsk )
     &        + fCoriCos(i-1,j,bi,bj)*angleCosC(i-1,j,bi,bj)*halfRL
     &           *( wFld(i-1,j, k ,bi,bj)*rVel2wUnit( k )
     &            + wFld(i-1,j,kp1,bi,bj)*rVel2wUnit(kp1)*wMsk )
     &        )
        ENDDO
       ENDDO
      ELSE
C-    Using averaged transport:
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
         uCoriolisTerm(i,j) = gravitySign*halfRL
     &    *( fCoriCos( i ,j,bi,bj)*angleCosC( i ,j,bi,bj)
     &      *( wFld( i ,j, k ,bi,bj)*rVel2wUnit( k )*deepFac2F( k )
     &       + wFld( i ,j,kp1,bi,bj)*rVel2wUnit(kp1)*deepFac2F(kp1)*wMsk
     &       )*rA( i ,j,bi,bj)*halfRL
     &     + fCoriCos(i-1,j,bi,bj)*angleCosC(i-1,j,bi,bj)
     &      *( wFld(i-1,j, k ,bi,bj)*rVel2wUnit( k )*deepFac2F( k )
     &       + wFld(i-1,j,kp1,bi,bj)*rVel2wUnit(kp1)*deepFac2F(kp1)*wMsk
     &       )*rA(i-1,j,bi,bj)*halfRL
     &     )*recip_rAw(i,j,bi,bj)*recip_deepFac2C(k)
     &      *recip_hFacW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
