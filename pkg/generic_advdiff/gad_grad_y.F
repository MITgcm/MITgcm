C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_grad_y.F,v 1.10 2011/03/29 15:53:45 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_GRAD_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_GRAD_Y(
     I           bi,bj,k,
     I           yA,
     U           tracer,
     O           dTdy,
     I           myThid )

C !DESCRIPTION:
C Calculates the meridional gradient of a tracer: $\partial_y \theta
C = \frac{1}{\Delta y_c} \delta_j \theta$
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
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !UPDATED PARAMETERS: =================================================
C  tracer           :: tracer field
C    (NOTE: only udate the corner-halo region when using CS-grid)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !OUTPUT PARAMETERS: ==================================================
C  dTdy             :: meridional gradient (units= [Tr_units]*[r_units])
      _RL dTdy  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j              :: loop indices
      INTEGER i,j
CEOP

cph-exch2#ifndef ALLOW_AUTODIFF_TAMC
C--   Internal exchange for calculations in Y
      IF (useCubedSphereExchange) THEN
        CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                             tracer, bi,bj, myThid )
      ENDIF
cph-exch2#endif

C     o Meridional tracer gradient
      DO i=1-Olx,sNx+Olx
       dTdy(I,1-Oly)=0.
      ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        dTdy(i,j) = yA(i,j)
     &            *_recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &            *(tracer(i,j)-tracer(i,j-1))
#ifdef ISOTROPIC_COS_SCALING
#ifdef COSINEMETH_III
     &            *sqCosFacV(j,bi,bj)
#endif
#endif
#ifdef ALLOW_OBCS
     &            *maskInS(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
