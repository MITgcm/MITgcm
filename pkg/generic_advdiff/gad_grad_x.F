C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_grad_x.F,v 1.10 2011/03/29 15:53:45 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_GRAD_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_GRAD_X(
     I           bi,bj,k,
     I           xA,
     U           tracer,
     O           dTdx,
     I           myThid )

C !DESCRIPTION:
C Calculates the zonal gradient of a tracer: $\partial_x \theta
C = \frac{1}{\Delta x_c} \delta_i \theta$
C and multiplies by the area.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj            :: tile indices
C  k                :: vertical level
C  myThid           :: thread number
      INTEGER bi,bj,k
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !UPDATED PARAMETERS: =================================================
C  tracer           :: tracer field
C    (NOTE: only udate the corner-halo region when using CS-grid)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !OUTPUT PARAMETERS: ==================================================
C  dTdx             :: zonal gradient (units = [Tr_units]*[r_units])
      _RL dTdx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j              :: loop indices
      INTEGER i,j
CEOP

cph-exch2#ifndef ALLOW_AUTODIFF_TAMC
C--   Internal exchange for calculations in X
      IF (useCubedSphereExchange) THEN
        CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                             tracer, bi,bj, myThid )
      ENDIF
cph-exch2#endif

C     o Zonal tracer gradient
      DO j=1-Oly,sNy+Oly
       dTdx(1-Olx,j)=0.
       DO i=1-Olx+1,sNx+Olx
        dTdx(i,j) = xA(i,j)
     &            *_recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &            *(tracer(i,j)-tracer(i-1,j))
#ifdef COSINEMETH_III
     &            *sqCosFacU(j,bi,bj)
#endif
#ifdef ALLOW_OBCS
     &            *maskInW(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
