C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_coriolis.F,v 1.4 2001/09/28 03:25:15 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_CORIOLIS

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_CORIOLIS( 
     I        bi,bj,k,uFld,
     U        vCoriolisTerm,
     I        myThid)

C !DESCRIPTION:
C Calculates the horizontal Coriolis term in the meridional equation:
C \begin{equation*}
C -\overline{f}^j \overline{u}^{ij}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal flow
C  myThid               :: thread number
      INTEGER bi,bj,K
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vCoriolisTerm        :: Coriolis term
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
CEOP

      IF (useEnergyConservingCoriolis) THEN
C Energy conserving discretization
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx,sNx+Olx-1
         vCoriolisTerm(i,j) =
     &     0.5*( _fCori(i, j ,bi,bj)
     &          *0.5*( uFld( i , j )+uFld(i+1, j ) )
     &         + _fCori(i,j-1,bi,bj)
     &          *0.5*( uFld( i ,j-1)+uFld(i+1,j-1) ) )
        ENDDO
       ENDDO
      ELSE
C Original discretization
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx,sNx+Olx-1
        vCoriolisTerm(i,j) =
     &    -0.5*(_fCori(i, j ,bi,bj)+_fCori(i,j-1,bi,bj))
     &   *0.25*( uFld(i, j )+uFld(i+1, j )
     &          +uFld(i,j-1)+uFld(i+1,j-1)
     &         )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
