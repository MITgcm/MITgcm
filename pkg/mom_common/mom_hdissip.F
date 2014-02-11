C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_hdissip.F,v 1.7 2014/02/11 20:22:51 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

      SUBROUTINE MOM_HDISSIP(
     I        bi, bj, k,
     I        tension, strain, hFacZ,
     I        viscAh_s, viscAh_t, viscA4_s, viscA4_t,
     I        harmonic, biharmonic, useVariableViscosity,
     O        uDissip, vDissip,
     I        myThid )

      IMPLICIT NONE

C     Calculate horizontal dissipation terms in terms of tension and strain
C       Du = d/dx At Tension + d/dy As Strain
C       Dv = d/dx As Strain  - d/dy At Tension

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_s(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_t(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_s(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_t(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL harmonic, biharmonic, useVariableViscosity
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i, j

C     - Laplacian and bi-harmonic terms
      IF (harmonic) THEN
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1

         uDissip(i,j) =
     &    recip_dyG(i,j,bi,bj)*recip_dyG(i,j,bi,bj)
     &    *recip_dxC(i,j,bi,bj)
     &    *(
     &       dyF( i , j ,bi,bj)*dyF( i , j ,bi,bj)
     &         *viscAh_t( i , j )*tension( i , j )
     &      -dyF(i-1, j ,bi,bj)*dyF(i-1, j ,bi,bj)
     &         *viscAh_t(i-1, j )*tension(i-1, j )
     &     )
     &    +recip_dxC(i,j,bi,bj)*recip_dxC(i,j,bi,bj)
     &    *recip_dyG(i,j,bi,bj)
     &    *(
     &       dxV( i ,j+1,bi,bj)*dxV( i ,j+1,bi,bj)
     &         *viscAh_s(i,j+1)*strain( i ,j+1)
     &      -dxV( i , j ,bi,bj)*dxV( i , j ,bi,bj)
     &         *viscAh_s(i, j )*strain( i , j )
     &     )

         vDissip(i,j) =
     &    recip_dyC(i,j,bi,bj)*recip_dyC(i,j,bi,bj)
     &    *recip_dxG(i,j,bi,bj)
     &    *(
     &       dyU(i+1, j ,bi,bj)*dyU(i+1, j ,bi,bj)
     &         *viscAh_s(i+1,j)*strain(i+1,j)
     &      -dyU( i , j ,bi,bj)*dyU( i , j ,bi,bj)
     &         *viscAh_s( i ,j)*strain( i ,j)
     &     )
     &    -recip_dxG(i,j,bi,bj)*recip_dxG(i,j,bi,bj)
     &    *recip_dyC(i,j,bi,bj)
     &    *(
     &       dxF( i , j ,bi,bj)*dxF( i , j ,bi,bj)
     &         *viscAh_t(i, j )*tension(i, j )
     &      -dxF( i ,j-1,bi,bj)*dxF( i ,j-1,bi,bj)
     &         *viscAh_t(i,j-1)*tension(i,j-1)
     &     )

        ENDDO
       ENDDO
      ENDIF
      IF (biharmonic) THEN
       STOP 'MOM_HDISSIP: BIHARMONIC NOT ALLOWED WITH STRAIN-TENSION'
      ENDIF

      RETURN
      END
