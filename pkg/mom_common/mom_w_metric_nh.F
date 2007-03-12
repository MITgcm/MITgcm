C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_w_metric_nh.F,v 1.3 2007/03/12 23:54:26 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_W_METRIC_NH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_W_METRIC_NH(
     I               bi,bj,k,
     I               uFld, vFld,
     O               wMetricTerms,
     I               myThid )

C !DESCRIPTION:
C Calculates the vertical metric term due to non-hydrostaticity on the sphere:
C \begin{equation}
C + \frac{1}{a} \overline{ \overline{u^2}^{i} + \overline{v^2}^{j} }^{k}
C \end{equation}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal flow
C  vFld                 :: meridional flow
C  myThid               :: my Thread Id number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wMetricTerms         :: metric term
      _RL wMetricTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_NONHYDROSTATIC
C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      IF ( k.GT.1 .AND. k.LE.Nr ) THEN
        DO j=1-Olx,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
           wMetricTerms(i,j) = -gravitySign*wUnit2rVel(k)
     &      *recip_rSphere*recip_deepFacF(k)*(
     &          ( (uFld( i ,j,k-1,bi,bj)*uFld( i ,j,k-1,bi,bj)
     &            +uFld(i+1,j,k-1,bi,bj)*uFld(i+1,j,k-1,bi,bj))
     &          + (vFld(i, j ,k-1,bi,bj)*vFld(i, j ,k-1,bi,bj)
     &            +vFld(i,j+1,k-1,bi,bj)*vFld(i,j+1,k-1,bi,bj))
     &          )
     &        + ( (uFld( i ,j, k ,bi,bj)*uFld( i ,j, k ,bi,bj)
     &            +uFld(i+1,j, k ,bi,bj)*uFld(i+1,j, k ,bi,bj))
     &          + (vFld(i, j , k ,bi,bj)*vFld(i, j , k ,bi,bj)
     &            +vFld(i,j+1, k ,bi,bj)*vFld(i,j+1, k ,bi,bj))
     &          )                            )*0.25 _d 0
         ENDDO
        ENDDO
      ELSE
        DO j=1-Olx,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
           wMetricTerms(i,j) = 0. _d 0
         ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
