C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_coriolis.F,v 1.10 2008/11/16 17:25:26 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

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
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vCoriolisTerm        :: Coriolis term
      _RL vCoriolisTerm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      _RS one
      PARAMETER( one = 1. _d 0 )
CEOP

      IF (useEnergyConservingCoriolis) THEN
C Energy conserving discretization
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx,sNx+Olx-1
         vCoriolisTerm(i,j) =
     &    -0.5*( _fCori(i, j ,bi,bj)
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

      IF (useJamartWetPoints) THEN
C Scale term so that only "wet" points are used
C Due to: Jamart and Ozer, 1986, JGR 91 (C9), 10,621-10,631
C         "Numerical Boundary Layers and Spurious Residual Flows"
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx,sNx+Olx-1
         vCoriolisTerm(i,j) = vCoriolisTerm(i,j)
     &   *4. _d 0/MAX( one,
     &                 maskW( i , j ,k,bi,bj)+maskW(i+1, j ,k,bi,bj)
     &                +maskW( i ,j-1,k,bi,bj)+maskW(i+1,j-1,k,bi,bj) )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
