C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_biharm_r.F,v 1.2 2014/08/18 12:22:46 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_BIHARM_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_BIHARM_R(
     I           bi, bj, k,
     I           diffKr4, tracer,
     U           d4f,
     I           myThid )

C !DESCRIPTION:
C Calculates the vertical flux due to bi-harmonic diffusion of a tracer.
C \begin{equation*}
C F^r_{diff} = \kappa_4 \partial_r ( \partial^2 / \partial_r^2 \theta )
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
C  diffKr4              :: vertical bi-harmonic diffusivity
C  tracer               :: tracer field
C  myThid               :: my thread Id number
      INTEGER bi, bj, k
      _RL diffKr4(Nr)
      _RL tracer (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  dfy                  :: vertical diffusive flux
      _RL d4f    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j,n                :: loop indices
C  del2T                :: vertical 2nd  derivative of tracer
C  gradR                :: vertical 1rst derivative of tracer (*rkSign)
      INTEGER i, j, n
      INTEGER kl, km, kp
      _RL del2T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:2)
      _RL gradR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:3)
      _RL tmpFac
CEOP

      IF ( k.GE.2 ) THEN

C     Calculate vertical gradient @ interface k-1, k & k+1 (n=1:3)
       DO n=1,3
        km = k+n-3
        kl = k+n-2
        IF ( km.LT.1 .OR. kl.GT.Nr ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gradR(i,j,n) = 0.
          ENDDO
         ENDDO
        ELSE
         tmpFac = recip_drC(kl)*deepFac2F(kl)*rhoFacF(kl)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gradR(i,j,n) = ( tracer(i,j,kl)-tracer(i,j,km) )
     &                *tmpFac*maskC(i,j,kl,bi,bj)*maskC(i,j,km,bi,bj)
          ENDDO
         ENDDO
        ENDIF
       ENDDO

C     Calculate vertical 2nd derivative @ level k-1 & k (n=1:2)
       DO n=1,2
        kl = k+n-2
        kp = k+n-1
        tmpFac = recip_drF(kl)*recip_deepFac2C(kl)*recip_rhoFacC(kl)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           del2T(i,j,n) = ( gradR(i,j,n+1)-gradR(i,j,n) )
     &                  *_recip_hFacC(i,j,kl,bi,bj)
         ENDDO
        ENDDO
       ENDDO

C     Add bi-harmonic flux (gradient of 2nd derivative)
       tmpFac = rkSign*recip_drC(k)*deepFac2F(k)*rhoFacF(k)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          d4f(i,j) = d4f(i,j)
     &             + diffKr4(k)*( del2T(i,j,2)-del2T(i,j,1) )
     &              *tmpFac*_rA(i,j,bi,bj)
        ENDDO
       ENDDO

      ENDIF

      RETURN
      END
