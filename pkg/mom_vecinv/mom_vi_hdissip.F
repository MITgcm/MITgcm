C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_hdissip.F,v 1.5 2004/02/07 23:15:47 dimitri Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_HDISSIP(
     I        bi,bj,k,
     I        hDiv,vort3,hFacZ,dStar,zStar,
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
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
      _RL Zip,Zij,Zpj,Dim,Dij,Dmj,uD2,vD2,uD4,vD4

C     - Laplacian  and bi-harmonic terms
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1

c       Dim=dyF( i ,j-1,bi,bj)*hFacC( i ,j-1,k,bi,bj)*hDiv( i ,j-1)
c       Dij=dyF( i , j ,bi,bj)*hFacC( i , j ,k,bi,bj)*hDiv( i , j )
c       Dmj=dyF(i-1, j ,bi,bj)*hFacC(i-1, j ,k,bi,bj)*hDiv(i-1, j )
c       Dim=dyF( i ,j-1,bi,bj)*                       hDiv( i ,j-1)
c       Dij=dyF( i , j ,bi,bj)*                       hDiv( i , j )
c       Dmj=dyF(i-1, j ,bi,bj)*                       hDiv(i-1, j )
        Dim=                                          hDiv( i ,j-1)
        Dij=                                          hDiv( i , j )
        Dmj=                                          hDiv(i-1, j )

c       Zip=dxV( i ,j+1,bi,bj)*hFacZ( i ,j+1)*vort3( i ,j+1)
c       Zij=dxV( i , j ,bi,bj)*hFacZ( i , j )*vort3( i , j )
c       Zpj=dxV(i+1, j ,bi,bj)*hFacZ(i+1, j )*vort3(i+1, j )
        Zip=                   hFacZ( i ,j+1)*vort3( i ,j+1)
        Zij=                   hFacZ( i , j )*vort3( i , j )
        Zpj=                   hFacZ(i+1, j )*vort3(i+1, j )

C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01 
        if (viscAhGrid*deltaTmom.NE.0.) then
          Dij=Dij*
     &     min(viscAh+viscAhGrid*rA ( i , j ,bi,bj)/deltaTmom,viscAhMax)
          Dim=Dim*
     &     min(viscAh+viscAhGrid*rA ( i ,j-1,bi,bj)/deltaTmom,viscAhMax)
          Dmj=Dmj*
     &     min(viscAh+viscAhGrid*rA (i-1, j ,bi,bj)/deltaTmom,viscAhMax)
          Zij=Zij*
     &     min(viscAh+viscAhGrid*rAz( i , j ,bi,bj)/deltaTmom,viscAhMax)
          Zip=Zip*
     &     min(viscAh+viscAhGrid*rAz( i ,j+1,bi,bj)/deltaTmom,viscAhMax)
          Zpj=Zpj*
     &     min(viscAh+viscAhGrid*rAz(i+1, j ,bi,bj)/deltaTmom,viscAhMax)
          uD2 = (
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &      -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
          vD2 = (
     &       recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
        else
c       uD2 = recip_rAw(i,j,bi,bj)*(
c    &    recip_hFacW(i,j,k,bi,bj)*viscAh*( (Dij-Dmj)*cosFacU(j,bi,bj) )
c    &   -recip_hFacW(i,j,k,bi,bj)*viscAh*( Zip-Zij ) )
c       uD2 = recip_rAw(i,j,bi,bj)*(
c    &                             viscAh*( (Dij-Dmj)*cosFacU(j,bi,bj) )
c    &   -recip_hFacW(i,j,k,bi,bj)*viscAh*( Zip-Zij ) )
          uD2 = viscAh*(
     &               cosFacU(j,bi,bj)*( Dij-Dmj )*recip_DXC(i,j,bi,bj)
     &      -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj) )
c       vD2 = recip_rAs(i,j,bi,bj)*(
c    &    recip_hFacS(i,j,k,bi,bj)*viscAh*( (Zpj-Zij)*cosFacV(j,bi,bj) )
c    &   +recip_hFacS(i,j,k,bi,bj)*viscAh*( Dij-Dim ) )
c       vD2 = recip_rAs(i,j,bi,bj)*(
c    &    recip_hFacS(i,j,k,bi,bj)*viscAh*( (Zpj-Zij)*cosFacV(j,bi,bj) )
c    &   +                         viscAh*( Dij-Dim ) )
          vD2 = viscAh*(
     &       recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &                                           *cosFacV(j,bi,bj)
     &                               +( Dij-Dim )*recip_DYC(i,j,bi,bj) )
        endif

c       Dim=dyF( i ,j-1,bi,bj)*hFacC( i ,j-1,k,bi,bj)*dStar( i ,j-1)
c       Dij=dyF( i , j ,bi,bj)*hFacC( i , j ,k,bi,bj)*dStar( i , j )
c       Dmj=dyF(i-1, j ,bi,bj)*hFacC(i-1, j ,k,bi,bj)*dStar(i-1, j )
        Dim=dyF( i ,j-1,bi,bj)*                       dStar( i ,j-1)
        Dij=dyF( i , j ,bi,bj)*                       dStar( i , j )
        Dmj=dyF(i-1, j ,bi,bj)*                       dStar(i-1, j )

        Zip=dxV( i ,j+1,bi,bj)*hFacZ( i ,j+1)*zStar( i ,j+1)
        Zij=dxV( i , j ,bi,bj)*hFacZ( i , j )*zStar( i , j )
        Zpj=dxV(i+1, j ,bi,bj)*hFacZ(i+1, j )*zStar(i+1, j )

C This bit scales the harmonic dissipation operator to be proportional
C to the grid-cell area over the time-step. viscAh is then non-dimensional
C and should be less than 1/8, for example viscAh=0.01 
        if (viscA4Grid*deltaTmom.NE.0.) then
          Dij = Dij * min(
     &         viscA4+viscA4Grid*(rA ( i , j ,bi,bj)**2)/deltaTmom,
     &         viscA4Max)
          Dim = Dim * min(
     &         viscA4+viscA4Grid*(rA ( i ,j-1,bi,bj)**2)/deltaTmom,
     &         viscA4Max)
          Dmj = Dmj * min(
     &         viscA4+viscA4Grid*(rA (i-1, j ,bi,bj)**2)/deltaTmom,
     &         viscA4Max)
          Zij = Zij * min(
     &         viscA4+viscA4Grid*(rAz( i , j ,bi,bj)**2)/deltaTmom,
     &         viscA4Max)
          Zip = Zip * min(
     &         viscA4+viscA4Grid*(rAz( i ,j+1,bi,bj)**2)/deltaTmom,
     &         viscA4Max)
          Zpj = Zpj * min(
     &         viscA4+viscA4Grid*(rAz(i+1, j ,bi,bj)**2)/deltaTmom,
     &         viscA4Max)
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             ( (Dij-Dmj)*cosFacU(j,bi,bj) )
     &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij ) )
          vD4 = recip_rAs(i,j,bi,bj)*(
     &    recip_hFacS(i,j,k,bi,bj)*( (Zpj-Zij)*cosFacV(j,bi,bj) )
     &   +                         ( Dij-Dim ) )
        else
c       uD4 = recip_rAw(i,j,bi,bj)*(
c    &    recip_hFacW(i,j,k,bi,bj)*viscA4*( (Dij-Dmj)*cosFacU(j,bi,bj) )
c    &   -recip_hFacW(i,j,k,bi,bj)*viscA4*( Zip-Zij ) )
          uD4 = recip_rAw(i,j,bi,bj)*(
     &                             viscA4*( (Dij-Dmj)*cosFacU(j,bi,bj) )
     &   -recip_hFacW(i,j,k,bi,bj)*viscA4*( Zip-Zij ) )

c       vD4 = recip_rAs(i,j,bi,bj)*(
c    &    recip_hFacS(i,j,k,bi,bj)*viscA4*( (Zpj-Zij)*cosFacV(j,bi,bj) )
c    &   +recip_hFacS(i,j,k,bi,bj)*viscA4*( Dij-Dim ) )
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
