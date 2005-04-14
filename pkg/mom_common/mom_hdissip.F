C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_hdissip.F,v 1.3 2005/04/14 14:22:51 baylor Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

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
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

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
      _RL viscA_t(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA_s(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ASmag, smagfac
      _RL vg2Min, vg2Max, AlinMax, AlinMin
      _RL lenA2, lenAz2

      IF (deltaTmom.NE.0.) THEN
       vg2Min=viscAhGridMin/deltaTmom
       vg2Max=viscAhGridMax/deltaTmom
      ELSE
       vg2Min=0.
       vg2Max=0.
      ENDIF

C     - Calculate Smagorinsky Coefficients
      smagfac=(viscC2smag/pi)**2
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
        IF (viscC2smag.NE.0.) THEN
C    Geometric Mean is used as length scale
         lenA2=(2*rA(i,j,bi,bj)/
     &	    (dxF(I,J,bi,bj)+dyF(I,J,bi,bj)))**2

         Asmag=smagfac*lenA2
     &    *sqrt(tension(i,j)**2
     &          +0.25*(strain(i+1, j )**2+strain( i ,j+1)**2
     &                +strain(i-1, j )**2+strain( i ,j-1)**2))
         viscA_t(i,j)=min(viscAhMax,max(viscAt,Asmag))

         IF (vg2Max.GT.0.) THEN
            AlinMax=vg2Max*lenA2
            viscA_t(i,j)=min(AlinMax,viscA_t(i,j))
         ENDIF
         AlinMin=vg2Min*lenA2
         viscA_t(i,j)=max(AlinMin,viscA_t(i,j))

C    Geometric Mean is used as length scale
         lenAz2=(2*rAz(i,j,bi,bj)/
     &       (dxV(I,J,bi,bj)+dyU(I,J,bi,bj)))**2
         Asmag=smagfac*lenAz2
     &    *sqrt(strain(i,j)**2
     &          +0.25*(tension( i , j )**2+tension( i ,j+1)**2
     &                +tension(i+1, j )**2+tension(i+1,j+1)**2))
         viscA_s(i,j)=min(viscAhMax,max(viscAs,Asmag))

         IF (vg2Max.GT.0.) THEN
            AlinMax=vg2Max*lenAz2
            viscA_s(i,j)=min(AlinMax,viscA_s(i,j))
         ENDIF
         AlinMin=vg2Min*lenAz2
         viscA_s(i,j)=max(AlinMin,viscA_s(i,j))
 
        ELSE
         viscA_t(i,j)=viscAt
         viscA_s(i,j)=viscAs
        ENDIF
       ENDDO
      ENDDO

C     - Laplacian  and bi-harmonic terms
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1

        uDissip(i,j) = 
     &   recip_dyg(i,j,bi,bj)*recip_dyg(i,j,bi,bj)
     &   *recip_dxc(i,j,bi,bj)
     &   *(
     &      dyf( i , j ,bi,bj)*dyf( i , j ,bi,bj)
     &        *viscA_t( i , j )*tension( i , j )
     &     -dyf(i-1, j ,bi,bj)*dyf(i-1, j ,bi,bj)
     &        *viscA_t(i-1, j )*tension(i-1, j )
     &    )
     &   +recip_dxc(i,j,bi,bj)*recip_dxc(i,j,bi,bj)
     &   *recip_dyg(i,j,bi,bj)
     &   *(
     &      dxv( i ,j+1,bi,bj)*dxv( i ,j+1,bi,bj)
     &        *viscA_s(i,j+1)*strain( i ,j+1)
     &     -dxv( i , j ,bi,bj)*dxv( i , j ,bi,bj)
     &        *viscA_s(i, j )*strain( i , j )
     &    )

        vDissip(i,j) = 
     &   recip_dyc(i,j,bi,bj)*recip_dyc(i,j,bi,bj)
     &   *recip_dxg(i,j,bi,bj)
     &   *(
     &      dyu(i+1, j ,bi,bj)*dyu(i+1, j ,bi,bj)
     &        *viscA_s(i+1,j)*strain(i+1,j)
     &     -dyu( i , j ,bi,bj)*dyu( i , j ,bi,bj)
     &        *viscA_s( i ,j)*strain( i ,j)
     &    )
     &   -recip_dxg(i,j,bi,bj)*recip_dxg(i,j,bi,bj)
     &   *recip_dyc(i,j,bi,bj)
     &   *(
     &      dxf( i , j ,bi,bj)*dxf( i , j ,bi,bj)
     &        *viscA_t(i, j )*tension(i, j )
     &     -dxf( i ,j-1,bi,bj)*dxf( i ,j-1,bi,bj)
     &        *viscA_t(i,j-1)*tension(i,j-1)
     &    )

       ENDDO
      ENDDO

      RETURN
      END
