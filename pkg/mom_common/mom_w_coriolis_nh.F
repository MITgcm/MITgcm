#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_W_CORIOLIS_NH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_W_CORIOLIS_NH(
     I               bi,bj,k,
     I               uFld, vFld,
     U               wCoriolisTerm,
     I               myThid )

C !DESCRIPTION:
C Calculates the Coriolis term in the vertical momentum equation:
C \begin{equation*}
C + f_prime \overline{u}^{ik}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uFld                 :: horizontal flow, u component
C  vFld                 :: horizontal flow, v component
C  myThid               :: my Thread Id number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uCoriolisTerm        :: Coriolis term
      _RL wCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_NONHYDROSTATIC
C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      IF ( k.GT.1 .AND. k.LE.Nr ) THEN

       IF ( selectCoriScheme.GE.6 ) THEN
C-    Using thickness-averaged transport:
C     for now, without dyG*deepFacC weight and without recip_hFacI factor:
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
           wCoriolisTerm(i,j) = -gravitySign
     &      *fCoriCos(i,j,bi,bj)
     &      *( angleCosC(i,j,bi,bj)
     &        *( ( uFld( i ,j,k-1,bi,bj)*hFacW( i ,j,k-1,bi,bj)
     &           + uFld(i+1,j,k-1,bi,bj)*hFacW(i+1,j,k-1,bi,bj)
     &           )*drF(k-1)
c    &            *deepFacC(k-1)
     &         + ( uFld( i ,j, k ,bi,bj)*hFacW( i ,j, k ,bi,bj)
     &           + uFld(i+1,j, k ,bi,bj)*hFacW(i+1,j, k ,bi,bj)
     &           )*drF( k )
c    &            *deepFacC( k )
     &         )*0.25 _d 0
     &        -angleSinC(i,j,bi,bj)
     &        *( ( vFld(i, j ,k-1,bi,bj)*hFacS(i, j ,k-1,bi,bj)
     &           + vFld(i,j+1,k-1,bi,bj)*hFacS(i,j+1,k-1,bi,bj)
     &           )*drF(k-1)
c    &            *deepFacC(k-1)
     &         + ( vFld(i, j , k ,bi,bj)*hFacS(i, j , k ,bi,bj)
     &           + vFld(i,j+1, k ,bi,bj)*hFacS(i,j+1, k ,bi,bj)
     &           )*drF( k )
c    &            *deepFacC( k )
     &         )*0.25 _d 0
     &       )*recip_drC(k)*wUnit2rVel(k)
c    &       )*recip_drC(k)*recip_deepFacF(k)*wUnit2rVel(k)
         ENDDO
        ENDDO
       ELSEIF ( selectCoriScheme.GE.4 ) THEN
C-    Using thickness-averaged transport (but without hFac):
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
           wCoriolisTerm(i,j) = -gravitySign
     &      *fCoriCos(i,j,bi,bj)
     &      *( angleCosC(i,j,bi,bj)
     &         *( ( uFld(i,j,k-1,bi,bj) + uFld(i+1,j,k-1,bi,bj) )
     &            *drF(k-1)
     &          + ( uFld(i,j, k ,bi,bj) + uFld(i+1,j, k ,bi,bj) )
     &            *drF( k )
     &          )*0.25 _d 0
     &        -angleSinC(i,j,bi,bj)
     &         *( ( vFld(i,j,k-1,bi,bj) + vFld(i,j+1,k-1,bi,bj) )
     &            *drF(k-1)
     &          + ( vFld(i,j, k ,bi,bj) + vFld(i,j+1, k ,bi,bj) )
     &            *drF( k )
     &          )*0.25 _d 0
     &       )*recip_drC(k)*wUnit2rVel(k)
         ENDDO
        ENDDO
       ELSE
C-    Energy conserving discretization of 2*Omega*cos(phi)*u_eastward
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
           wCoriolisTerm(i,j) =
     &      -gravitySign*fCoriCos(i,j,bi,bj)*
     &       ( angleCosC(i,j,bi,bj)*(
     &                (uFld(i,j,k-1,bi,bj)+uFld(i+1,j,k-1,bi,bj))
     &              + (uFld(i,j, k ,bi,bj)+uFld(i+1,j, k ,bi,bj))
     &                              )*0.25 _d 0
     &        -angleSinC(i,j,bi,bj)*(
     &                (vFld(i,j,k-1,bi,bj)+vFld(i,j+1,k-1,bi,bj))
     &              + (vFld(i,j, k ,bi,bj)+vFld(i,j+1, k ,bi,bj))
     &                              )*0.25 _d 0
     &       )*wUnit2rVel(k)
         ENDDO
        ENDDO
       ENDIF

      ELSE
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
           wCoriolisTerm(i,j) = 0. _d 0
         ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
