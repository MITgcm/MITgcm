C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/mom_vi_del2uv.F,v 1.4 2004/01/22 17:17:19 afe Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
#ifdef USE_W2
#include "EEPARAMS.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* USE_W2 */

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J
#ifndef USE_W2
      _RL Zip,Zij,Zpj,Dim,Dij,Dmj
#else /* USE_W2 */
      _RL Zip,Zij,Zpj,Dim,Dij,Dmj,uDij
      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
      INTEGER myTile

C     Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
         southWestCorner = .FALSE.
         southEastCorner = .FALSE.
         northWestCorner = .FALSE.
         northEastCorner = .FALSE.
         myTile = W2_myTileList(bi)
         IF ( exch2_isWedge(myTile) .EQ. 1 .AND.
     &        exch2_isSedge(myTile) .EQ. 1 ) THEN
            southWestCorner = .TRUE.
         ENDIF
         IF ( exch2_isEedge(myTile) .EQ. 1 .AND.
     &        exch2_isSedge(myTile) .EQ. 1 ) THEN
            southEastCorner = .TRUE.
         ENDIF
         IF ( exch2_isEedge(myTile) .EQ. 1 .AND.
     &        exch2_isNedge(myTile) .EQ. 1 ) THEN
            northEastCorner = .TRUE.
         ENDIF
         IF ( exch2_isWedge(myTile) .EQ. 1 .AND.
     &        exch2_isNedge(myTile) .EQ. 1 ) THEN
            northWestCorner = .TRUE.
         ENDIF
      ENDIF
#endif /* USE_W2 */

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

#ifdef USE_W2
C     Special stuff for Cubed Sphere
        uDij=Dij
        IF (useCubedSphereExchange) THEN
C               U(0,1)     D(0,1)      U(1,1)     TILE
C                |                      |
C   V(-1,1) --- Z(0,1) --- V(0,1) ---  Z(1,1) --- V(1,1) ---
C                |                      |
C               U(0,0)     D(0,0)      U(1,0)     D(1,0)
C                |                      |
C                      --- V(0,0) ---  Z(1,0) --- V(1,0) ---
C                                       |
C                                      U(1,-1)
         if(southWestCorner.and.i.eq.1.and.j.eq.0) Dmj=hDiv(0,1)
         if(southWestCorner.and.i.eq.0.and.j.eq.1) Dim=hDiv(1,0)
C                                            U(1,N+2)
C                                             |
C                          --- V(0,N+1) ---  Z(1,N+2) --- V(1,N+2) ---
C                  |                          |
C                 U(0,N+1)     D(0,N+1)      U(1,N+1)     D(1,N+1)
C                  |                          |
C   V(-1,N+1) --- Z(0,N+1) --- V(0,N+1) ---  Z(1,N+1) --- V(1,N+1) ---
C                  |                          |
C                 U(0,N)       D(0,N)        U(1,N)       TILE
         if(northWestCorner.and.i.eq.1.and.j.eq.sNy+1) Dmj=hDiv(0,sNy)
         if(northWestCorner.and.i.eq.0.and.j.eq.sNy+1) Dij=hDiv(1,sNy+1)
C   TILE       U(N+1,1)     D(N+1,1)      U(N+2,1)
C               |                          |
C   V(N,1) --- Z(N+1,1) --- V(N+1,1) ---  Z(N+2,1) --- V(N+3,1) ---
C               |                          |
C   D(N,0)     U(N+1,0)     D(N+1,0)      U(N+2,0)
C               |                          |
C   V(N,0) --- Z(N+1,0) --- V(N+1,0) ---
C               |
C              U(N+1,-1)
         if(southEastCorner.and.i.eq.sNx+1.and.j.eq.0) Dij=hDiv(sNx+1,1)
         if(southEastCorner.and.i.eq.sNx+1.and.j.eq.1) Dim=hDiv(sNx,0)
C                U(N+1,N+2)
C                 | 
C   V(N,N+2) --- Z(N+1,N+2) --- V(N+1,N+2) ---
C                 |                              |
C   D(N,N+1)     U(N+1,N+1)     D(N+1,N+1)      U(N+2,N+1)
C                 |                              |
C   V(N,N+1) --- Z(N+1,N+1) --- V(N+1,N+1) ---  Z(N+2,N+1) --- V(N+3,N+1) ---
C                 |                              |
C   TILE         U(N+1,N)       D(N+1,N)        U(N+2,N)
         if (northEastCorner.and.i.eq.sNx+1 .and. j.eq.sNy+1) then
          uDij=hDiv(sNx+1,sNy)
          Dij=hDiv(sNx,sNy+1)
         endif
        ENDIF

#endif /* USE_W2 */
c       del2u(i,j) = recip_rAw(i,j,bi,bj)*(
c    &   +recip_hFacW(i,j,k,bi,bj)*( Dij-Dmj )
c    &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij ) )
c       del2u(i,j) = recip_rAw(i,j,bi,bj)*(
c    &   +                         ( Dij-Dmj )
c    &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij ) )
        del2u(i,j) =
#ifndef USE_W2
     &   +                         ( Dij-Dmj )*recip_DXC(i,j,bi,bj)
#else /* USE_W2 */
     &   +                         ( uDij-Dmj )*recip_DXC(i,j,bi,bj)
#endif /* USE_W2 */
     &   -recip_hFacW(i,j,k,bi,bj)*( Zip-Zij )*recip_DYG(i,j,bi,bj)

c       del2v(i,j) = recip_rAs(i,j,bi,bj)*(
c    &    recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )
c    &   +recip_hFacS(i,j,k,bi,bj)*( Dij-Dim ) )
c       del2v(i,j) = recip_rAs(i,j,bi,bj)*(
c    &    recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )
c    &   +                         ( Dij-Dim ) )
        del2v(i,j) =
     &    recip_hFacS(i,j,k,bi,bj)*( Zpj-Zij )*recip_DXG(i,j,bi,bj)
     &   +                         ( Dij-Dim )*recip_DYC(i,j,bi,bj)

       ENDDO
      ENDDO

      RETURN
      END
