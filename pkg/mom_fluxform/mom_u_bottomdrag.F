C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_u_bottomdrag.F,v 1.4 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_BOTTOMDRAG

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_BOTTOMDRAG(
     I        bi,bj,k,
     I        uFld, KE, KappaRU,
     O        uDragTerms,
     I        myThid)

C !DESCRIPTION:
C Calculates the drag due to friction and the no-slip condition at bottom:
C \begin{equation*}
C G^u_{drag} = - ( r_b + C_D |v| + \frac{2}{\Delta r_c} ) u
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
C  uFld                 :: zonal flow
C  KE                   :: Kinetic energy
C  KappaRU              :: vertical viscosity
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uDragTerms           :: drag term
      _RL uDragTerms(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  Kp1                  :: =k+1 for k<Nr, =Nr for k>=Nr
      INTEGER I,J,Kp1
      _RL viscFac
      _RL rdrckp1,maskDown
CEOP

C-  No-slip BCs impose a drag at bottom
      Kp1=min(k+1,Nr)
      rdrckp1=recip_drC(kp1)
      IF (k.EQ.Nr) rdrckp1=recip_drF(k)
      viscFac=1.
      IF (.NOT. no_slip_bottom) viscFac=0.
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        maskDown=_maskW(i,j,kp1,bi,bj)
        IF (k.EQ.Nr) maskDown=0.
        uDragTerms(i,j)=
     &   -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *( 
     &      2.*KappaRU(i,j,kp1)*rkFac*rdrckp1*viscFac
     &     + bottomDragLinear
     &    )*(1.-maskDown)*uFld(i,j)
        IF ( (KE(i,j)+KE(i-1,j)) .NE. 0. ) THEN
         uDragTerms(i,j)=uDragTerms(i,j)
     &    -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &    *bottomDragQuadratic*sqrt(KE(i,j)+KE(i-1,j))
     &    *(1.-maskDown)*uFld(i,j)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
