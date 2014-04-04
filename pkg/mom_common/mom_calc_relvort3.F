C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_relvort3.F,v 1.10 2014/04/04 20:08:11 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"
#undef CALC_CS_CORNER_EXTENDED

      SUBROUTINE MOM_CALC_RELVORT3(
     I        bi,bj,k,
     I        uFld, vFld, hFacZ,
     O        vort3,
     I        myThid)
      IMPLICIT NONE
C     *==========================================================*
C     | S/R MOM_CALC_RELVORT3
C     *==========================================================*
C     *==========================================================*

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */
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
      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
      INTEGER myFace
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif /* ALLOW_EXCH2 */

#ifdef ALLOW_AUTODIFF
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        vort3(I,J) = 0. _d 0
       ENDDO
      ENDDO
#endif

      DO J=2-OLy,sNy+OLy
       DO I=2-OLx,sNx+OLx

C       Horizontal curl of flow field - ignoring lopping factors
        vort3(I,J)=
     &      recip_rAz(I,J,bi,bj)*(
     &      ( vFld(I,J)*dyC(I,J,bi,bj)
     &       -vFld(I-1,J)*dyC(I-1,J,bi,bj) )
     &     -( uFld(I,J)*dxC(I,J,bi,bj)
     &       -uFld(I,J-1)*dxC(I,J-1,bi,bj) )
     &                           )

C       Horizontal curl of flow field - including lopping factors
c       IF (hFacZ(i,j).NE.0.) THEN
c        vort3(I,J)=
c    &      recip_rAz(I,J,bi,bj)*(
c    &      vFld(I,J)*dyc(I,J,bi,bj)*_hFacW(i,j,k,bi,bj)
c    &     -vFld(I-1,J)*dyc(I-1,J,bi,bj)*_hFacW(i-1,j,k,bi,bj)
c    &     -uFld(I,J)*dxc(I,J,bi,bj)*_hFacS(i,j,k,bi,bj)
c    &     +uFld(I,J-1)*dxc(I,J-1,bi,bj)*_hFacS(i,j-1,k,bi,bj)
c    &                           )
c    &                            /hFacZ(i,j)
c       ELSE
c        vort3(I,J)=0.
c       ENDIF

       ENDDO
      ENDDO

C     Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
#ifdef ALLOW_EXCH2
       myTile = W2_myTileList(bi,bj)
       myFace = exch2_myFace(myTile)
       southWestCorner = exch2_isWedge(myTile).EQ.1
     &             .AND. exch2_isSedge(myTile).EQ.1
       southEastCorner = exch2_isEedge(myTile).EQ.1
     &             .AND. exch2_isSedge(myTile).EQ.1
       northEastCorner = exch2_isEedge(myTile).EQ.1
     &             .AND. exch2_isNedge(myTile).EQ.1
       northWestCorner = exch2_isWedge(myTile).EQ.1
     &             .AND. exch2_isNedge(myTile).EQ.1
#else
       myFace = bi
       southWestCorner = .TRUE.
       southEastCorner = .TRUE.
       northWestCorner = .TRUE.
       northEastCorner = .TRUE.
#endif /* ALLOW_EXCH2 */

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
         I=1
         J=1
C-    to get the same truncation, independent from the face Nb,
C     do (1+2)+3, and always in the same order (exch3 convention order):
         vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      ( vFld(I,J)*dyC(I,J,bi,bj)
     &       -uFld(I,J)*dxC(I,J,bi,bj) )
     &      + uFld(I,J-1)*dxC(I,J-1,bi,bj)
     &     )
C-    the quick way, but do not get the same truncation on the 3 faces:
c        vort3(I,J)=
c    &     +recip_rAz(I,J,bi,bj)*(
c    &      vFld(I,J)*dyC(I,J,bi,bj)
c    &     -uFld(I,J)*dxC(I,J,bi,bj)
c    &     +uFld(I,J-1)*dxC(I,J-1,bi,bj)
c    &     )
#ifdef CALC_CS_CORNER_EXTENDED
         vort3(I-1,J)=
     &      recip_rAz(I-1,J,bi,bj)*(
     &      vFld(I-1,J)*dyC(I-1,J,bi,bj)
     &     -vFld(I-2,J)*dyC(I-2,J,bi,bj)
     &     -uFld(I-1,J)*dxC(I-1,J,bi,bj)
     &     +vFld(I+0,J-1)*dyC(I+0,J-1,bi,bj)
     &                             )
     &     *maskS(i-1,j,k,bi,bj)*maskS(i-2,j,k,bi,bj)
     &     *maskW(i-1,j,k,bi,bj)*maskS(i,j-1,k,bi,bj)
         vort3(I,J-1)=vort3(I-1,J)
#endif
       ENDIF

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
         I=sNx+1
         J=1
C-    to get the same truncation, independent from the face Nb,
C      (exch3 convention order):
         IF ( myFace.EQ.2 ) THEN
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (-uFld(I,J)*dxC(I,J,bi,bj)
     &       -vFld(I-1,J)*dyC(I-1,J,bi,bj) )
     &      + uFld(I,J-1)*dxC(I,J-1,bi,bj)
     &     )
         ELSEIF ( myFace.EQ.4 ) THEN
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (-vFld(I-1,J)*dyC(I-1,J,bi,bj)
     &       +uFld(I,J-1)*dxC(I,J-1,bi,bj) )
     &      - uFld(I,J)*dxC(I,J,bi,bj)
     &     )
         ELSE
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (+uFld(I,J-1)*dxC(I,J-1,bi,bj)
     &       -uFld(I,J)*dxC(I,J,bi,bj)     )
     &      - vFld(I-1,J)*dyC(I-1,J,bi,bj)
     &     )
         ENDIF
C-    the quick way, but do not get the same truncation on the 3 faces:
c        vort3(I,J)=
c    &     +recip_rAz(I,J,bi,bj)*(
c    &     -vFld(I-1,J)*dyC(I-1,J,bi,bj)
c    &     -uFld(I,J)*dxC(I,J,bi,bj)
c    &     +uFld(I,J-1)*dxC(I,J-1,bi,bj)
c    &     )
#ifdef CALC_CS_CORNER_EXTENDED
         vort3(I+1,J)=
     &      recip_rAz(I+1,J,bi,bj)*(
     &      vFld(I+1,J)*dyC(I+1,J,bi,bj)
     &     -vFld(I-0,J)*dyC(I-0,J,bi,bj)
     &     -uFld(I+1,J)*dxC(I+1,J,bi,bj)
     &     -vFld(I-1,J-1)*dyC(I-1,J-1,bi,bj)
     &                           )
     &     *maskS(i+1,j,k,bi,bj)*maskS(i-0,j,k,bi,bj)
     &     *maskW(i+1,j,k,bi,bj)*maskS(i-1,j-1,k,bi,bj)
         vort3(I,J-1)=vort3(I+1,J)
#endif
       ENDIF

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
         I=1
         J=sNy+1
C-    to get the same truncation, independent from the face Nb,
C      (exch3 convention order):
         IF ( myFace.EQ.1 ) THEN
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (+uFld(I,J-1)*dxC(I,J-1,bi,bj)
     &       +vFld(I,J)*dyC(I,J,bi,bj)     )
     &       -uFld(I,J)*dxC(I,J,bi,bj)
     &     )
         ELSEIF ( myFace.EQ.3 ) THEN
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (-uFld(I,J)*dxC(I,J,bi,bj)
     &       +uFld(I,J-1)*dxC(I,J-1,bi,bj) )
     &      + vFld(I,J)*dyC(I,J,bi,bj)
     &     )
         ELSE
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (+vFld(I,J)*dyC(I,J,bi,bj)
     &       -uFld(I,J)*dxC(I,J,bi,bj)     )
     &      + uFld(I,J-1)*dxC(I,J-1,bi,bj)
     &     )
         ENDIF
C-    the quick way, but do not get the same truncation on the 3 faces:
c        vort3(I,J)=
c    &     +recip_rAz(I,J,bi,bj)*(
c    &      vFld(I,J)*dyC(I,J,bi,bj)
c    &     -uFld(I,J)*dxC(I,J,bi,bj)
c    &     +uFld(I,J-1)*dxC(I,J-1,bi,bj)
c    &     )
#ifdef CALC_CS_CORNER_EXTENDED
         vort3(I-1,J)=
     &      recip_rAz(I-1,J,bi,bj)*(
     &      vFld(I-1,J)*dyC(I-1,J,bi,bj)
     &     -vFld(I-2,J)*dyC(I-2,J,bi,bj)
     &     +vFld(I-0,J+1)*dyC(I-0,J+1,bi,bj)
     &     +uFld(I-1,J-1)*dxC(I-1,J-1,bi,bj)
     &                           )
     &     *maskS(i-1,j,k,bi,bj)*maskS(i-2,j,k,bi,bj)
     &     *maskS(i,j+1,k,bi,bj)*maskW(i-1,j-1,k,bi,bj)
         vort3(I,J+1)=vort3(I-1,J)
#endif
       ENDIF

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
         I=sNx+1
         J=sNy+1
C-    to get the same truncation, independent from the face Nb:
C      (exch3 convention order):
         IF ( MOD(myFace,2).EQ.1 ) THEN
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (-uFld(I,J)*dxC(I,J,bi,bj)
     &       -vFld(I-1,J)*dyC(I-1,J,bi,bj) )
     &      + uFld(I,J-1)*dxC(I,J-1,bi,bj)
     &     )
         ELSE
          vort3(I,J)=
     &     +recip_rAz(I,J,bi,bj)*(
     &      (+uFld(I,J-1)*dxC(I,J-1,bi,bj)
     &       -uFld(I,J)*dxC(I,J,bi,bj)     )
     &      - vFld(I-1,J)*dyC(I-1,J,bi,bj)
     &     )
         ENDIF
C-    the quick way, but do not get the same truncation on the 3 faces:
c        vort3(I,J)=
c    &     +recip_rAz(I,J,bi,bj)*(
c    &     -vFld(I-1,J)*dyC(I-1,J,bi,bj)
c    &     -uFld(I,J)*dxC(I,J,bi,bj)
c    &     +uFld(I,J-1)*dxC(I,J-1,bi,bj)
c    &     )
#ifdef CALC_CS_CORNER_EXTENDED
         vort3(I+1,J)=
     &      recip_rAz(I+1,J,bi,bj)*(
     &      vFld(I+1,J)*dyC(I+1,J,bi,bj)
     &     -vFld(I-0,J)*dyC(I-0,J,bi,bj)
     &     -vFld(I-1,J+1)*dyC(I-1,J+1,bi,bj)
     &     +uFld(I+1,J-1)*dxC(I+1,J-1,bi,bj)
     &                           )
     &     *maskS(i+1,j,k,bi,bj)*maskS(i-0,j,k,bi,bj)
     &     *maskS(i-1,j+1,k,bi,bj)*maskW(i+1,j-1,k,bi,bj)
         vort3(I,J+1)=vort3(I+1,J)
#endif
       ENDIF
      ENDIF

      RETURN
      END
