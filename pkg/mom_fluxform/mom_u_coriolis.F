#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_CORIOLIS

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_CORIOLIS(
     I        bi, bj, k, vFld,
     U        uCoriolisTerm,
     I        myThid )

C !DESCRIPTION:
C Calculates the horizontal Coriolis term in the zonal equation:
C \begin{equation*}
C \overline{f}^i \overline{v}^{ij}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj               :: tile indices
C  k                    :: vertical level
C  vFld                 :: meridional flow
C  myThid               :: thread number
      INTEGER bi, bj, k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uCoriolisTerm        :: Coriolis term
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i, j
CEOP

      IF ( selectCoriScheme.LE.1 ) THEN
C-    Original discretization
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         uCoriolisTerm(i,j) = halfRL
     &   *( _fCori( i ,j,bi,bj) + _fCori(i-1,j,bi,bj) )
     &   *0.25 _d 0*( vFld( i ,j) + vFld( i ,j+1)
     &              + vFld(i-1,j) + vFld(i-1,j+1)
     &              )
        ENDDO
       ENDDO
      ELSEIF ( selectCoriScheme.LE.3 ) THEN
C-    Energy conserving discretization
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         uCoriolisTerm(i,j) = halfRL
     &        *( _fCori( i ,j,bi,bj)
     &          *halfRL*( vFld( i ,j) + vFld( i ,j+1) )
     &         + _fCori(i-1,j,bi,bj)
     &          *halfRL*( vFld(i-1,j) + vFld(i-1,j+1) ) )
        ENDDO
       ENDDO
      ELSE
C-    Using averaged transport:
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         uCoriolisTerm(i,j) = halfRL
     &   *( _fCori( i ,j,bi,bj) + _fCori(i-1,j,bi,bj) )
     &   *( vFld( i , j )*dxG( i , j ,bi,bj)*hFacS( i , j ,k,bi,bj)
     &    + vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacS( i ,j+1,k,bi,bj)
     &    + vFld(i-1, j )*dxG(i-1, j ,bi,bj)*hFacS(i-1, j ,k,bi,bj)
     &    + vFld(i-1,j+1)*dxG(i-1,j+1,bi,bj)*hFacS(i-1,j+1,k,bi,bj)
     &    )*0.25 _d 0*recip_dxC(i,j,bi,bj)*recip_hFacW(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      IF ( selectCoriScheme.EQ.1 .OR. selectCoriScheme.EQ.3 ) THEN
C-    Scale term so that only "wet" points are used
C     Due to: Jamart and Ozer, 1986, JGR 91 (C9), 10,621-10,631
C         "Numerical Boundary Layers and Spurious Residual Flows"
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx
         uCoriolisTerm(i,j) = uCoriolisTerm(i,j)
     &   *4. _d 0/MAX( oneRS,
     &                 maskS( i , j ,k,bi,bj)+maskS( i ,j+1,k,bi,bj)
     &                +maskS(i-1, j ,k,bi,bj)+maskS(i-1,j+1,k,bi,bj) )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
