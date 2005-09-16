C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_hdissip.F,v 1.4 2005/09/16 19:32:20 baylor Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

      SUBROUTINE MOM_HDISSIP(
     I        bi,bj,k,
     I        hDiv,vort3,tension,strain,KE,
     I        hFacZ,
     O        uDissip,vDissip,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate horizontal dissipation terms in terms of tension and strain
C
C       Du = d/dx At Tension + d/dy As Strain
C       Dv = d/dx As Strain  - d/dy At Tension

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL viscAh_t(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_s(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_t(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_s(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL harmonic, biharmonic, useVariableViscosity

      CALL MOM_CALC_VISC(
     I        bi,bj,k,
     O        viscAh_s,viscAh_t,viscA4_s,viscA4_t,
     O        harmonic,biharmonic,useVariableViscosity,
     I        hDiv,vort3,tension,strain,KE,
     I        myThid)

C     - Laplacian and bi-harmonic terms
      IF (harmonic) THEN
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1

         uDissip(i,j) = 
     &    recip_dyg(i,j,bi,bj)*recip_dyg(i,j,bi,bj)
     &    *recip_dxc(i,j,bi,bj)
     &    *(
     &       dyf( i , j ,bi,bj)*dyf( i , j ,bi,bj)
     &         *viscAh_t( i , j )*tension( i , j )
     &      -dyf(i-1, j ,bi,bj)*dyf(i-1, j ,bi,bj)
     &         *viscAh_t(i-1, j )*tension(i-1, j )
     &     )
     &    +recip_dxc(i,j,bi,bj)*recip_dxc(i,j,bi,bj)
     &    *recip_dyg(i,j,bi,bj)
     &    *(
     &       dxv( i ,j+1,bi,bj)*dxv( i ,j+1,bi,bj)
     &         *viscAh_s(i,j+1)*strain( i ,j+1)
     &      -dxv( i , j ,bi,bj)*dxv( i , j ,bi,bj)
     &         *viscAh_s(i, j )*strain( i , j )
     &     )

         vDissip(i,j) = 
     &    recip_dyc(i,j,bi,bj)*recip_dyc(i,j,bi,bj)
     &    *recip_dxg(i,j,bi,bj)
     &    *(
     &       dyu(i+1, j ,bi,bj)*dyu(i+1, j ,bi,bj)
     &         *viscAh_s(i+1,j)*strain(i+1,j)
     &      -dyu( i , j ,bi,bj)*dyu( i , j ,bi,bj)
     &         *viscAh_s( i ,j)*strain( i ,j)
     &     )
     &    -recip_dxg(i,j,bi,bj)*recip_dxg(i,j,bi,bj)
     &    *recip_dyc(i,j,bi,bj)
     &    *(
     &       dxf( i , j ,bi,bj)*dxf( i , j ,bi,bj)
     &         *viscAh_t(i, j )*tension(i, j )
     &      -dxf( i ,j-1,bi,bj)*dxf( i ,j-1,bi,bj)
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
