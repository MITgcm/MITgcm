C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_hdissip.F,v 1.1 2001/09/06 14:23:58 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_HDISSIP(
     I        bi,bj,k,
     I        tension,strain,hFacZ,viscAt,viscAs,
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
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAt
      _RL viscAs
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

C     - Laplacian  and bi-harmonic terms
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1

        uDissip(i,j) = 
     &    viscAt
     &   *recip_dyg(i,j,bi,bj)*recip_dyg(i,j,bi,bj)
     &   *recip_dxc(i,j,bi,bj)
     &   *(
     &      dyf( i , j ,bi,bj)*dyf( i , j ,bi,bj)*tension( i , j )
     &     -dyf(i-1, j ,bi,bj)*dyf(i-1, j ,bi,bj)*tension(i-1, j )
     &    )
     &   +viscAs
     &   *recip_dxc(i,j,bi,bj)*recip_dxc(i,j,bi,bj)
     &   *recip_dyg(i,j,bi,bj)
     &   *(
     &      dxv( i ,j+1,bi,bj)*dxv( i ,j+1,bi,bj)*strain( i ,j+1)
     &     -dxv( i , j ,bi,bj)*dxv( i , j ,bi,bj)*strain( i , j )
     &    )

        vDissip(i,j) = 
     &    viscAs
     &   *recip_dyc(i,j,bi,bj)*recip_dyc(i,j,bi,bj)
     &   *recip_dxg(i,j,bi,bj)
     &   *(
     &      dyu(i+1, j ,bi,bj)*dyu(i+1, j ,bi,bj)*strain(i+1, j )
     &     -dyu( i , j ,bi,bj)*dyu( i , j ,bi,bj)*strain( i , j )
     &    )
     &   -viscAt
     &   *recip_dxg(i,j,bi,bj)*recip_dxg(i,j,bi,bj)
     &   *recip_dyc(i,j,bi,bj)
     &   *(
     &      dxf( i , j ,bi,bj)*dxf( i , j ,bi,bj)*tension( i , j )
     &     -dxf( i ,j-1,bi,bj)*dxf( i ,j-1,bi,bj)*tension( i ,j-1)
     &    )

       ENDDO
      ENDDO

      RETURN
      END
