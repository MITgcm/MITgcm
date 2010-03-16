C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_u_coriolis_nh.F,v 1.3 2010/03/16 00:20:14 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_CORIOLIS_NH

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_CORIOLIS_NH(
     I               bi,bj,k,wFld,
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
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  wFld                 :: vertical flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL wFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uCoriolisTerm        :: Coriolis term
      _RL uCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j,kp1
      _RL wMsk
CEOP


      kp1=min(k+1,Nr)
      wMsk=1.
      IF (k.EQ.Nr) wMsk=0.

C Energy conserving discretization of 2*Omega*cos(phi)*w
      DO j=1-Oly,sNy+Oly
       DO i=2-Olx,sNx+Olx
        uCoriolisTerm(i,j) =
     &    0.5*( fCoriCos( i ,j,bi,bj)*angleCosC( i ,j,bi,bj)
     &         *0.5*( wFld( i ,j, k ,bi,bj)*rVel2wUnit( k )
     &               +wFld( i ,j,kp1,bi,bj)*rVel2wUnit(kp1)*wMsk )
     &        + fCoriCos(i-1,j,bi,bj)*angleCosC(i-1,j,bi,bj)
     &         *0.5*( wFld(i-1,j, k ,bi,bj)*rVel2wUnit( k )
     &               +wFld(i-1,j,kp1,bi,bj)*rVel2wUnit(kp1)*wMsk )
     &        )*gravitySign
       ENDDO
      ENDDO

      RETURN
      END
