#ifndef USE_W2
C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_calc_relvort3.F,v 1.5 2004/01/22 17:17:19 afe Exp $
#else /* USE_W2 */
C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_calc_relvort3.F,v 1.5 2004/01/22 17:17:19 afe Exp $
#endif /* USE_W2 */
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_CALC_RELVORT3( 
     I        bi,bj,k,
     I        uFld, vFld, hFacZ,
     O        vort3,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_CALC_RELVORT3                                    |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef USE_W2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* USE_W2 */
C     == Routine arguments ==
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j
#ifdef USE_W2
      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
      INTEGER myTile
#endif /* USE_W2 */

#ifdef ALLOW_AUTODIFF_TAMC
      DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
        vort3(I,J) = 0. _d 0
       ENDDO
      ENDDO
#endif

      DO J=2-Oly,sNy+Oly
       DO I=2-Olx,sNx+Olx

C       Horizontal curl of flow field - ignoring lopping factors
        vort3(I,J)=
     &      recip_rAz(I,J,bi,bj)*(
     &      vFld(I,J)*dyc(I,J,bi,bj)
     &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &                           )

C       Horizontal curl of flow field - including lopping factors
        IF (hFacZ(i,j).NE.0.) THEN
c        vort3(I,J)=
c    &      recip_rAz(I,J,bi,bj)*(
c    &      vFld(I,J)*dyc(I,J,bi,bj)*_hFacW(i,j,k,bi,bj)
c    &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)*_hFacW(i-1,j,k,bi,bj)
c    &     -uFld(I,J)*dxc(I,J,bi,bj)*_hFacS(i,j,k,bi,bj)
c    &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)*_hFacS(i,j-1,k,bi,bj)
c    &                           )
c    &                            /hFacZ(i,j)
        ELSE
         vort3(I,J)=0.
        ENDIF

       ENDDO
      ENDDO
#ifndef USE_W2
        
#else /* USE_W2 */

#endif /* USE_W2 */
C     Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
#ifdef USE_W2
       southWestCorner = .FALSE.
       southEastCorner = .FALSE.
       northWestCorner = .FALSE.
       northEastCorner = .FALSE.
       myTile = W2_myTileList(bi)
       IF ( exch2_isWedge(myTile) .EQ. 1 .AND.
     &      exch2_isSedge(myTile) .EQ. 1 ) THEN
        southWestCorner = .TRUE.
       ENDIF
       IF ( exch2_isEedge(myTile) .EQ. 1 .AND.
     &      exch2_isSedge(myTile) .EQ. 1 ) THEN
        southEastCorner = .TRUE.
       ENDIF
       IF ( exch2_isEedge(myTile) .EQ. 1 .AND.
     &      exch2_isNedge(myTile) .EQ. 1 ) THEN
        northEastCorner = .TRUE.
       ENDIF
       IF ( exch2_isWedge(myTile) .EQ. 1 .AND.
     &      exch2_isNedge(myTile) .EQ. 1 ) THEN
        northWestCorner = .TRUE.
       ENDIF
C
       IF ( southWestCorner ) THEN
C               U(0,1)     D(0,1)      U(1,1)     TILE
C                |                      |
C   V(-1,1) --- Z(0,1) --- V(0,1) ---  Z(1,1) --- V(1,1) ---
C                |                      |
C               U(0,0)     D(0,0)      U(1,0)     D(1,0)
C                |                      |
C                      --- V(0,0) ---  Z(1,0) --- V(1,0) ---
C                                       |
C                                      U(1,-1)
#endif /* USE_W2 */
         I=1
         J=1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      vFld(I,J)*dyc(I,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
#ifdef USE_W2
        vort3(I-1,J)=
     &      recip_rAz(I-1,J,bi,bj)*(
     &      vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -vFld(I-2,J)*dyc(I-2,J,bi,bj)
     &     -uFld(I-1,J)*dxc(I-1,J,bi,bj)
c    &     +uFld(I-1,J-1)*dxc(I-1,J-1,bi,bj)
     &     +vFld(I+0,J-1)*dyc(I+0,J-1,bi,bj)
     &                             )
        vort3(I,J-1)=vort3(I-1,J)
       ENDIF
C
       IF ( southEastCorner ) THEN
C   TILE       U(N+1,1)     D(N+1,1)      U(N+2,1)
C               |                          |
C   V(N,1) --- Z(N+1,1) --- V(N+1,1) ---  Z(N+2,1) --- V(N+3,1) ---
C               |                          |
C   D(N,0)     U(N+1,0)     D(N+1,0)      U(N+2,0)
C               |                          |
C   V(N,0) --- Z(N+1,0) --- V(N+1,0) ---
C               |                          |
C              U(N+1,-1)
#endif /* USE_W2 */
         I=sNx+1
         J=1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &      vFld(I,J)*dyc(I,J,bi,bj)
#ifdef USE_W2
        vort3(I+1,J)=
     &      recip_rAz(I+1,J,bi,bj)*(
     &      vFld(I+1,J)*dyc(I+1,J,bi,bj)
     &     -vFld(I-0,J)*dyc(I-0,J,bi,bj)
     &     -uFld(I+1,J)*dxc(I+1,J,bi,bj)
c    &     +uFld(I+1,J-1)*dxc(I+1,J-1,bi,bj)
     &     -vFld(I-1,J-1)*dyc(I-1,J-1,bi,bj)
     &                           )
        vort3(I,J-1)=vort3(I+1,J)
       ENDIF
C
       IF ( northWestCorner ) THEN
C                                            U(1,N+2)
C                                             |
C                          --- V(0,N+1) ---  Z(1,N+2) --- V(1,N+2) ---
C                  |                          |
C                 U(0,N+1)     D(0,N+1)      U(1,N+1)     D(1,N+1)
C                  |                          |
C   V(-1,N+1) --- Z(0,N+1) --- V(0,N+1) ---  Z(1,N+1) --- V(1,N+1) ---
C                  |                          |
C                 U(0,N)       D(0,N)        U(1,N)       TILE
#endif /* USE_W2 */
         I=1
         J=sNy+1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      vFld(I,J)*dyc(I,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
#ifdef USE_W2
        vort3(I-1,J)=
     &      recip_rAz(I-1,J,bi,bj)*(
     &      vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -vFld(I-2,J)*dyc(I-2,J,bi,bj)
c    &     -uFld(I-1,J)*dxc(I-1,J,bi,bj)
     &     +vFld(I-0,J+1)*dyc(I-0,J+1,bi,bj)
     &     +uFld(I-1,J-1)*dxc(I-1,J-1,bi,bj)
     &                           )
        vort3(I,J+1)=vort3(I-1,J)
        ENDIF
C
        IF ( northEastCorner ) THEN
C                U(N+1,N+2)
C                 |                              |
C   V(N,N+2) --- Z(N+1,N+2) --- V(N+1,N+2) ---
C                 |                              |
C   D(N,N+1)     U(N+1,N+1)     D(N+1,N+1)      U(N+2,N+1)
C                 |                              |
C   V(N,N+1) --- Z(N+1,N+1) --- V(N+1,N+1) ---  Z(N+2,N+1) --- V(N+3,N+1) ---
C                 |                              |
C   TILE         U(N+1,N)       D(N+1,N)        U(N+2,N)
#endif /* USE_W2 */
         I=sNx+1
         J=sNy+1
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)
     &     -uFld(I,J)*dxc(I,J,bi,bj)
     &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)
     &     )
cph    &      vFld(I,J)*dyc(I,J,bi,bj)
#ifdef USE_W2
        vort3(I+1,J)=
     &      recip_rAz(I+1,J,bi,bj)*(
     &      vFld(I+1,J)*dyc(I+1,J,bi,bj)
     &     -vFld(I-0,J)*dyc(I-0,J,bi,bj)
c    &     -uFld(I+1,J)*dxc(I+1,J,bi,bj)
     &     -vFld(I-1,J+1)*dyc(I-1,J+1,bi,bj)
     &     +uFld(I+1,J-1)*dxc(I+1,J-1,bi,bj)
     &                           )
        vort3(I,J+1)=vort3(I+1,J)
        ENDIF
#endif /* USE_W2 */
       ENDIF

      RETURN
      END
