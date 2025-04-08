#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_ADV_WV

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_ADV_WV(
     I        bi, bj, k, deepFacA,
     I        vFld, wFld, rTrans,
     O        advectiveFluxWV,
     I        myThid )

C !DESCRIPTION:
C Calculates the vertical advective flux of meridional momentum:
C \begin{equation*}
C F^r = \overline{W}^j \overline{v}^{k}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj               :: tile indices
C  k                    :: vertical level
C  deepFacA             :: deep-model grid factor at level center
C  vFld                 :: meridional velocity
C  wFld                 :: vertical velocity
C  rTrans               :: vertical transport (above V point)
C  myThid               :: my Thread Id number
      INTEGER bi, bj, k
      _RL deepFacA(Nr)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  advectiveFluxWV      :: advective flux
      _RL advectiveFluxWV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i, j                 :: loop indices
      INTEGER i, j
CEOP

      IF ( k.EQ.Nr+1 .AND.
     &     useRealFreshWaterFlux .AND. usingPCoords ) THEN
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         advectiveFluxWV(i,j) = rTrans(i,j)*vFld(i,j,k-1,bi,bj)
     &                                     *deepFacA(k-1)
        ENDDO
       ENDDO

      ELSEIF ( k.GT.Nr .OR. (k.EQ.1.AND.rigidLid) ) THEN
C     Advective flux = 0  at k=Nr+1 ; = 0 at k=1 if rigid-lid

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         advectiveFluxWV(i,j) = 0.
        ENDDO
       ENDDO

      ELSEIF (k.EQ.1) THEN
C     (linear) Free-surface correction at k=1

       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         advectiveFluxWV(i,j) = rTrans(i,j)*vFld(i,j,k,bi,bj)
     &                                     *deepFacA(k)
        ENDDO
       ENDDO

      ELSE

C     Vertical advection - interior ; assume vFld & wFld are masked
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         advectiveFluxWV(i,j) = rTrans(i,j)*halfRL
#ifdef MOM_BOUNDARY_CONSERVE
     &    *( vFld(i,j, k ,bi,bj)*deepFacA( k )*_maskS(i,j,k-1,bi,bj)
     &     + vFld(i,j,k-1,bi,bj)*deepFacA(k-1)*_maskS(i,j, k ,bi,bj) )
#else
     &    *( vFld(i,j, k ,bi,bj)*deepFacA( k )
     &     + vFld(i,j,k-1,bi,bj)*deepFacA(k-1) )
#endif
        ENDDO
       ENDDO

       IF ( select_rStar.EQ.0 .AND. .NOT.rigidLid ) THEN
c    &                        .AND. usingPCoords ) THEN
C     (linear) Free-surface correction at k>1
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          advectiveFluxWV(i,j) = advectiveFluxWV(i,j)
     &     +0.25 _d 0*(
     &          wFld(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &           *(maskC(i, j ,k,bi,bj)-maskC(i, j ,k-1,bi,bj))
     &         +wFld(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
     &           *(maskC(i,j-1,k,bi,bj)-maskC(i,j-1,k-1,bi,bj))
     &                )*deepFac2F(k)*rhoFacF(k)
     &                 *vFld(i,j,k,bi,bj)*deepFacA(k)
         ENDDO
        ENDDO
C- endif select_rStar=0 and NOT rigidLid
       ENDIF

C- endif special k cases
      ENDIF

      RETURN
      END
