C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_del2uv.F,v 1.16 2011/05/03 19:36:11 jmc Exp $
C $Name:  $

#include "MOM_VECINV_OPTIONS.h"

      SUBROUTINE MOM_VI_DEL2UV(
     I        bi,bj,k,
     I        hDiv,vort3,hFacZ,
     O        del2u,del2v,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate del^2 of (u,v) in terms of hDiv and vort3
C

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
c     _RL Zip,Zij,Zpj,Dim,Dij,Dmj,uDij

C     - bi-harmonic viscosity :

c     DO j=2-Oly,sNy+Oly-1
c      DO i=2-Olx,sNx+Olx-1

c       Dim=dyF( i ,j-1,bi,bj)*hFacC( i ,j-1,k,bi,bj)*hDiv( i ,j-1)
c       Dij=dyF( i , j ,bi,bj)*hFacC( i , j ,k,bi,bj)*hDiv( i , j )
c       Dmj=dyF(i-1, j ,bi,bj)*hFacC(i-1, j ,k,bi,bj)*hDiv(i-1, j )
c       Dim=dyF( i ,j-1,bi,bj)*                       hDiv( i ,j-1)
c       Dij=dyF( i , j ,bi,bj)*                       hDiv( i , j )
c       Dmj=dyF(i-1, j ,bi,bj)*                       hDiv(i-1, j )
c       Dim=                                          hDiv( i ,j-1)
c       Dij=                                          hDiv( i , j )
c       Dmj=                                          hDiv(i-1, j )

c       Zip=dxV( i ,j+1,bi,bj)*hFacZ( i ,j+1)*vort3( i ,j+1)
c       Zij=dxV( i , j ,bi,bj)*hFacZ( i , j )*vort3( i , j )
c       Zpj=dxV(i+1, j ,bi,bj)*hFacZ(i+1, j )*vort3(i+1, j )
c       Zip=                   hFacZ( i ,j+1)*vort3( i ,j+1)
c       Zij=                   hFacZ( i , j )*vort3( i , j )
c       Zpj=                   hFacZ(i+1, j )*vort3(i+1, j )


c       del2u(i,j) = recip_rAw(i,j,bi,bj)*(
c    &   +recip_hFacW(i,j,k,bi,bj)*( Dij-Dmj )
c    &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij ) )
c       del2u(i,j) = recip_rAw(i,j,bi,bj)*(
c    &   +                         ( Dij-Dmj )
c    &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij ) )
c       del2u(i,j) =
c    &   +                         ( Dij-Dmj )*recip_DXC(i,j,bi,bj)
c    &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj)

c       del2v(i,j) = recip_rAs(i,j,bi,bj)*(
c    &    recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )
c    &   +recip_hFacS(i,j,k,bi,bj)*( Dij-Dim ) )
c       del2v(i,j) = recip_rAs(i,j,bi,bj)*(
c    &    recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )
c    &   +                         ( Dij-Dim ) )
c       del2v(i,j) =
c    &    recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
c    &   +                         ( Dij-Dim )*recip_DYC(i,j,bi,bj)

c      ENDDO
c     ENDDO

C     - bi-harmonic viscosity : del^2(U_component)

cph-exch2#ifndef ALLOW_AUTODIFF_TAMC
      IF (useCubedSphereExchange) THEN
C     to compute d/dx(hDiv), fill corners with appropriate values:
        CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                             hDiv, bi,bj, myThid )
      ENDIF
cph-exch2#endif
c     DO j=1,sNy
c      DO i=1,sNx+1
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
        del2u(i,j) =
     &      (  ( hDiv(i,j) - hDiv(i-1,j) )*recip_dxC(i,j,bi,bj)
     &        -_recip_hFacW(i,j,k,bi,bj)*
     &         ( hFacZ(i,j+1)*vort3(i,j+1) - hFacZ(i,j)*vort3(i,j) )
     &                                    *recip_dyG(i,j,bi,bj)
     &      )*maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &       *maskInW(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

C     - bi-harmonic viscosity : del^2(V_component)

cph-exch2#ifndef ALLOW_AUTODIFF_TAMC
      IF (useCubedSphereExchange) THEN
C     to compute d/dy(hDiv), fill corners with appropriate values:
        CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                             hDiv, bi,bj, myThid )
      ENDIF
cph-exch2#endif
c     DO j=1,sNy+1
c      DO i=1,sNx
      DO j=2-Oly,sNy+Oly-1
       DO i=2-Olx,sNx+Olx-1
        del2v(i,j) =
     &      (  ( hDiv(i,j) - hDiv(i,j-1) )*recip_dyC(i,j,bi,bj)
     &        +_recip_hFacS(i,j,k,bi,bj)*
     &         ( hFacZ(i+1,j)*vort3(i+1,j) - hFacZ(i,j)*vort3(i,j) )
     &                                    *recip_dxG(i,j,bi,bj)
     &      )*maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &       *maskInS(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

      RETURN
      END
