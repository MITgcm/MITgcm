#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_CORIOLIS

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_CORIOLIS(
     I        bi, bj, k, uFld,
     U        vCoriolisTerm,
     I        myThid )

C !DESCRIPTION:
C Calculates the horizontal Coriolis term in the meridional equation:
C \begin{equation*}
C -\overline{f}^j \overline{u}^{ij}
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
C  uFld                 :: zonal flow
C  myThid               :: thread number
      INTEGER bi, bj, k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vCoriolisTerm        :: Coriolis term
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i, j
CEOP

      IF ( selectCoriScheme.LE.1 ) THEN
C-    Original discretization
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         vCoriolisTerm(i,j) = -halfRL
     &   *( _fCori(i, j ,bi,bj) + _fCori(i,j-1,bi,bj) )
     &   *0.25 _d 0*( uFld(i, j ) + uFld(i+1, j )
     &              + uFld(i,j-1) + uFld(i+1,j-1)
     &              )
        ENDDO
       ENDDO
      ELSEIF ( selectCoriScheme.LE.3 ) THEN
C-    Energy conserving discretization
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         vCoriolisTerm(i,j) = -halfRL
     &        *( _fCori(i, j ,bi,bj)
     &          *halfRL*( uFld( i , j ) + uFld(i+1, j ) )
     &         + _fCori(i,j-1,bi,bj)
     &          *halfRL*( uFld( i ,j-1) + uFld(i+1,j-1) ) )
        ENDDO
       ENDDO
      ELSE
C-    Using averaged transport:
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         vCoriolisTerm(i,j) = -halfRL
     &   *( _fCori(i, j ,bi,bj) + _fCori(i,j-1,bi,bj) )
     &   *( uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj)
     &    + uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &    + uFld( i ,j-1)*dyG( i ,j-1,bi,bj)*hFacW( i ,j-1,k,bi,bj)
     &    + uFld(i+1,j-1)*dyG(i+1,j-1,bi,bj)*hFacW(i+1,j-1,k,bi,bj)
     &    )*0.25 _d 0*recip_dyC(i,j,bi,bj)*recip_hFacS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      IF ( selectCoriScheme.EQ.1 .OR. selectCoriScheme.EQ.3 ) THEN
C-    Scale term so that only "wet" points are used
C     Due to: Jamart and Ozer, 1986, JGR 91 (C9), 10,621-10,631
C         "Numerical Boundary Layers and Spurious Residual Flows"
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
         vCoriolisTerm(i,j) = vCoriolisTerm(i,j)
     &   *4. _d 0/MAX( oneRS,
     &                 maskW( i , j ,k,bi,bj)+maskW(i+1, j ,k,bi,bj)
     &                +maskW( i ,j-1,k,bi,bj)+maskW(i+1,j-1,k,bi,bj) )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
