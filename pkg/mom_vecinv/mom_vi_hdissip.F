C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_hdissip.F,v 1.6 2004/05/26 14:50:10 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_HDISSIP(
     I        bi,bj,k,
     I        hDiv,vort3,hFacZ,dStar,zStar,
c    I        viscAh_Z,viscAh_D,viscA4_Z,viscA4_D,
     O        uDissip,vDissip,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate horizontal dissipation terms
C     [del^2 - del^4] (u,v)
C

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zStar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscA4_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL Zip,Zij,Zpj,Dim,Dij,Dmj,uD2,vD2,uD4,vD4
      _RL Alin,Alth,grdVrt,vg2,vg4
      LOGICAL useVariableViscosity

      useVariableViscosity=
     &      (viscAhGrid*deltaTmom.NE.0.)
     &  .OR.(viscA4Grid*deltaTmom.NE.0.)
     &  .OR.(viscC2leith.NE.0.)
     &  .OR.(viscC4leith.NE.0.)

      IF (deltaTmom.NE.0.) THEN
       vg2=viscAhGrid/deltaTmom
       vg4=viscA4Grid/deltaTmom
      ELSE
       vg2=0.
       vg4=0.
      ENDIF

C     - Viscosity
      IF (useVariableViscosity) THEN
       DO j=2-Oly,sNy+Oly-1
        DO i=2-Olx,sNx+Olx-1
C This is the vector magnitude of the vorticity gradient
c        grdVrt=sqrt(0.25*(
c    &      ((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))**2
c    &     +((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj))**2
c    &     +((vort3(i+1,j+1)-vort3(i,j+1))*recip_DXG(i,j+1,bi,bj))**2
c    &     +((vort3(i+1,j+1)-vort3(i+1,j))*recip_DYG(i+1,j,bi,bj))**2 ))
C but this approximation will work on cube (and differs by as much as 4X)
         grdVrt=abs((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))
         grdVrt=max(grdVrt,
     &        abs((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj)))
         grdVrt=max(grdVrt,
     &        abs((vort3(i+1,j+1)-vort3(i,j+1))*recip_DXG(i,j+1,bi,bj)))
         grdVrt=max(grdVrt,
     &        abs((vort3(i+1,j+1)-vort3(i+1,j))*recip_DYG(i+1,j,bi,bj)))
         Alth=viscC2leith*grdVrt*(rA(i,j,bi,bj)**1.5)
         Alin=viscAh+vg2*rA ( i , j ,bi,bj)
         viscAh_D(i,j)=min(viscAhMax,Alin+Alth)

         Alth=viscC4leith*grdVrt*0.125*(rA(i,j,bi,bj)**2.5)
         Alin=viscA4+vg4*(rA ( i , j ,bi,bj)**2)
         viscA4_D(i,j)=min(viscA4Max,Alin+Alth)

C This is the vector magnitude of the vorticity gradient
c        grdVrt=sqrt(0.25*(
c    &      ((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))**2
c    &     +((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj))**2
c    &     +((vort3(i-1,j)-vort3(i,j))*recip_DXG(i-1,j,bi,bj))**2
c    &     +((vort3(i,j-1)-vort3(i,j))*recip_DYG(i,j-1,bi,bj))**2 ))
C but this approximation will work on cube (and differs by as much as 4X)
         grdVrt=abs((vort3(i+1,j)-vort3(i,j))*recip_DXG(i,j,bi,bj))
         grdVrt=max(grdVrt,
     &          abs((vort3(i,j+1)-vort3(i,j))*recip_DYG(i,j,bi,bj)))
         grdVrt=max(grdVrt,
     &          abs((vort3(i-1,j)-vort3(i,j))*recip_DXG(i-1,j,bi,bj)))
         grdVrt=max(grdVrt,
     &          abs((vort3(i,j-1)-vort3(i,j))*recip_DYG(i,j-1,bi,bj)))
         Alth=viscC2leith*grdVrt*(rAz(i,j,bi,bj)**1.5)
         Alin=viscAh+vg2*rAz( i , j ,bi,bj)
         viscAh_Z(i,j)=min(viscAhMax,Alin+Alth)

         Alth=viscC4leith*grdVrt*0.125*(rAz(i,j,bi,bj)**2.5)
         Alin=viscA4+vg4*(rAz( i , j ,bi,bj)**2)
         viscA4_Z(i,j)=min(viscA4Max,Alin+Alth)
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         viscAh_D(i,j)=viscAh
         viscAh_Z(i,j)=viscAh
         viscA4_D(i,j)=viscA4
         viscA4_Z(i,j)=viscA4
        ENDDO
       ENDDO
      ENDIF

C     - Laplacian  and bi-harmonic terms
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1

        Dim=hDiv( i ,j-1)
        Dij=hDiv( i , j )
        Dmj=hDiv(i-1, j )
        Zip=hFacZ( i ,j+1)*vort3( i ,j+1)
        Zij=hFacZ( i , j )*vort3( i , j )
        Zpj=hFacZ(i+1, j )*vort3(i+1, j )

C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01 
        if (useVariableViscosity) then
          Dij=Dij*viscAh_D(i,j)
          Dim=Dim*viscAh_D(i,j-1)
          Dmj=Dmj*viscAh_D(i-1,j)
          Zij=Zij*viscAh_Z(i,j)
          Zip=Zip*viscAh_Z(i,j+1)
          Zpj=Zpj*viscAh_Z(i+1,j)
          uD2 = (
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &      -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
          vD2 = (
     &       recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
        else
          uD2 = viscAh*(
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &      -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
          vD2 = viscAh*(
     &       recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
        endif

        Dim=dyF( i ,j-1,bi,bj)*dStar( i ,j-1)
        Dij=dyF( i , j ,bi,bj)*dStar( i , j )
        Dmj=dyF(i-1, j ,bi,bj)*dStar(i-1, j )

        Zip=dxV( i ,j+1,bi,bj)*hFacZ( i ,j+1)*zStar( i ,j+1)
        Zij=dxV( i , j ,bi,bj)*hFacZ( i , j )*zStar( i , j )
        Zpj=dxV(i+1, j ,bi,bj)*hFacZ(i+1, j )*zStar(i+1, j )

C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01 
        if (useVariableViscosity) then
          Dij=Dij*viscA4_D(i,j)
          Dim=Dim*viscA4_D(i,j-1)
          Dmj=Dmj*viscA4_D(i-1,j)
          Zij=Zij*viscA4_Z(i,j)
          Zip=Zip*viscA4_Z(i,j+1)
          Zpj=Zpj*viscA4_Z(i+1,j)
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             ( (Dij-Dmj)*cosFacU(j,bi,bj) )
     &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij ) )
          vD4 = recip_rAs(i,j,bi,bj)*(
     &    recip_hFacS(i,j,k,bi,bj)*( (Zpj-Zij)*cosFacV(j,bi,bj) )
     &   +                         ( Dij-Dim ) )
        else
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             viscA4*( (Dij-Dmj)*cosFacU(j,bi,bj) )
     &   -recip_hFacW(i,j,k,bi,bj)*viscA4*( Zip-Zij ) )
          vD4 = recip_rAs(i,j,bi,bj)*(
     &    recip_hFacS(i,j,k,bi,bj)*viscA4*( (Zpj-Zij)*cosFacV(j,bi,bj) )
     &   +                         viscA4*( Dij-Dim ) )
        endif

        uDissip(i,j) = uD2 - uD4
        vDissip(i,j) = vD2 - vD4

       ENDDO
      ENDDO

      RETURN
      END
