C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_u_coriolis_nh.F,v 1.1 2002/11/05 19:58:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_CORIOLIS

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_CORIOLIS_NH( 
     I        bi,bj,k,wFld,
     U        uCoriolisTerm,
     I        myThid)

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

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  wFld                 :: vertical flow
C  myThid               :: thread number
      INTEGER bi,bj,K
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
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx
        uCoriolisTerm(i,j) =
     &   -0.5*( fCoriCos( i ,j,bi,bj)
     &         *0.5*( wFld( i ,j,k,bi,bj)+wFld( i ,j,kp1,bi,bj)*wMsk )
     &        + fCoriCos(i-1,j,bi,bj)
     &         *0.5*( wFld(i-1,j,k,bi,bj)+wFld(i-1,j,kp1,bi,bj)*wMsk ) )
     &    *gravitySign*recip_horiVertRatio
       ENDDO
      ENDDO

      RETURN
      END
