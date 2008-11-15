C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_hfacz.F,v 1.4 2008/11/15 18:46:03 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_CALC_HFACZ

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_CALC_HFACZ(
     I        bi,bj,k,
     O        hFacZ,r_hFacZ,
     I        myThid)

C !DESCRIPTION:
C Calculates the fractional thickness at vorticity points

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

#ifdef ALLOW_AUTODIFF_TAMC
c # include "EEPARAMS.h"
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  myThid               :: thread number
      INTEGER bi,bj,k
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  hFacZ                :: fractional thickness at vorticity points
C  r_hFacZ              :: reciprocal
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  hZoption   :: forward mode option to select the way hFacZ is computed:
C                0 : = minimum of 4 hFacW,hFacS arround (consistent with
C                    definition of partial cell & mask near topography)
C                1 : = minimum of 2 average (hFacW)_j,(hFacS)_i
C                2 : = average of 4 hFacW,hFacS arround (consistent with
C                    how free surface affects hFacW,hFacS it using r* and
C                    without topography)
      INTEGER I,J
#ifdef ALLOW_DEPTH_CONTROL
      _RL hFacZOpen(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacZOpenI(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacZOpenJ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# ifdef USE_SMOOTH_MIN
      _RS      smoothMin_R4
      EXTERNAL smoothMin_R4
# endif /* USE_SMOOTH_MIN */
#else
      _RS     hFacZOpen
      INTEGER hZoption
      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
      INTEGER myFace
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif /* ALLOW_EXCH2 */
CEOP
      PARAMETER ( hZoption = 0 )
#endif /* ALLOW_DEPTH_CONTROL */

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
          kkey = (ikey-1)*Nr + k
#endif /* ALLOW_DEPTH_CONTROL */
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Calculate open water fraction at vorticity points

#ifdef ALLOW_DEPTH_CONTROL
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        hFacZ(i,j)     =0.
        r_hFacZ(i,j)   =0.
        hFacZOpen(i,j) =0.
        hFacZOpenJ(i,j)=0.
        hFacZOpenJ(i,j)=0.
       ENDDO
      ENDDO

#ifdef    ALLOW_AUTODIFF_TAMC
CADJ STORE      hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
CADJ STORE    r_hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif    /* ALLOW_AUTODIFF_TAMC */
      DO j=2-Oly,sNy+Oly
       DO i=2-Olx,sNx+Olx
        hFacZOpenJ(i,j)=
#ifdef USE_SMOOTH_MIN
     &       smoothMin_R4(_hFacW(i  ,j  ,k,bi,bj),
#else
     &                MIN(_hFacW(i  ,j  ,k,bi,bj),
#endif /* USE_SMOOTH_MIN */
     &                    _hFacW(i  ,j-1,k,bi,bj))
     &       *maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)
        hFacZOpenI(i,j)=
#ifdef USE_SMOOTH_MIN
     &       smoothMin_R4(_hFacS(i  ,j  ,k,bi,bj),
#else
     &                MIN(_hFacS(i  ,j  ,k,bi,bj),
#endif /* USE_SMOOTH_MIN */
     &                    _hFacS(i-1,j  ,k,bi,bj))
     &         *maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)
       ENDDO
      ENDDO
#ifdef    ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacZOpenI(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
CADJ STORE hFacZOpenJ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif /* ALLOW_DEPTH_CONTROL */
#endif    /* ALLOW_AUTODIFF_TAMC */
      DO j=2-Oly,sNy+Oly
       DO i=2-Olx,sNx+Olx
        hFacZ(i,j) =
#ifdef USE_SMOOTH_MIN
     &       smoothMin_R4(hFacZOpenI(i,j),hFacZOpenJ(i,j))
#else
     &                MIN(hFacZOpenI(i,j),hFacZOpenJ(i,j))
#endif /* USE_SMOOTH_MIN */
     &         *maskW(i,j,k,bi,bj)*maskW(i,j-1,k,bi,bj)
     &         *maskS(i,j,k,bi,bj)*maskS(i-1,j,k,bi,bj)
       ENDDO
      ENDDO
#ifdef    ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif /* ALLOW_DEPTH_CONTROL */
#endif    /* ALLOW_AUTODIFF_TAMC */
      DO j=2-Oly,sNy+Oly
       DO i=2-Olx,sNx+Olx
        IF (hFacZ(i,j).EQ.0.) THEN
         r_hFacZ(i,j)=0.
        ELSE
         r_hFacZ(i,j)=1./hFacZ(i,j)
        ENDIF
       ENDDO
      ENDDO
#ifdef    ALLOW_AUTODIFF_TAMC
#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE    r_hFacZ(:,:) = comlev1_bibj_k , key=kkey, byte=isbyte
#endif /* ALLOW_DEPTH_CONTROL */
#endif    /* ALLOW_AUTODIFF_TAMC */

#else /* not ALLOW_DEPTH_CONTROL */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialize hFacZ:
c     DO j=1-Oly,sNy+Oly
c      DO i=1-Olx,sNx+Olx
c        hFacZ(i,j)=0.
c      ENDDO
c     ENDDO

C--   1rst row & column are not computed: fill with zero
      DO i=1-Olx,sNx+Olx
        hFacZ(i,1-Oly)=0.
      ENDDO
      DO j=2-Oly,sNy+Oly
        hFacZ(1-Olx,j)=0.
      ENDDO

C--   Calculate open water fraction at vorticity points

      IF ( hZoption.EQ.2 ) THEN
        DO j=2-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx
c         hFacZOpen=
c    &               ( _hFacW(i, j ,k,bi,bj)*rAw(i, j ,bi,bj)
c    &                +_hFacW(i,j-1,k,bi,bj)*rAw(i,j-1,bi,bj) )
c    &             + ( _hFacS( i ,j,k,bi,bj)*rAs( i ,j,bi,bj)
c    &                +_hFacS(i-1,j,k,bi,bj)*rAs(i-1,j,bi,bj) )
c         hFacZ(i,j) = 0.25 _d 0 * hFacZOpen*recip_rAz(i,j,bi,bj)
          hFacZOpen=
     &               ( _hFacW(i, j ,k,bi,bj)
     &                +_hFacW(i,j-1,k,bi,bj) )
     &             + ( _hFacS( i ,j,k,bi,bj)
     &                +_hFacS(i-1,j,k,bi,bj) )
          hFacZ(i,j) = 0.25 _d 0 * hFacZOpen
         ENDDO
        ENDDO
      ELSEIF ( hZoption.EQ.1 ) THEN
        DO j=2-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx
c         hFacZOpen=MIN(
c    &                  _hFacW(i, j ,k,bi,bj)*rAw(i, j ,bi,bj)
c    &                + _hFacW(i,j-1,k,bi,bj)*rAw(i,j-1,bi,bj)
c    &                , _hFacS( i ,j,k,bi,bj)*rAs( i ,j,bi,bj)
c    &                + _hFacS(i-1,j,k,bi,bj)*rAs(i-1,j,bi,bj)
c    &                 )
c         hFacZ(i,j) = 0.5 _d 0 * hFacZOpen*recip_rAz(i,j,bi,bj)
          hFacZOpen=MIN(
     &                  _hFacW(i, j ,k,bi,bj)
     &                + _hFacW(i,j-1,k,bi,bj)
     &                , _hFacS( i ,j,k,bi,bj)
     &                + _hFacS(i-1,j,k,bi,bj)
     &                 )
          hFacZ(i,j) = 0.5 _d 0 * hFacZOpen
         ENDDO
        ENDDO
      ELSE
        DO j=2-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx
          hFacZOpen=MIN(_hFacW(i,j,k,bi,bj),
     &                  _hFacW(i,j-1,k,bi,bj))
          hFacZOpen=MIN(_hFacS(i,j,k,bi,bj),hFacZOpen)
          hFacZOpen=MIN(_hFacS(i-1,j,k,bi,bj),hFacZOpen)
          hFacZ(i,j)=hFacZOpen
         ENDDO
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4
C     Special stuff for Cubed Sphere
      IF ( useCubedSphereExchange .AND. hZoption.GE.1 ) THEN

#ifdef ALLOW_EXCH2
        myTile = W2_myTileList(bi)
        myFace = exch2_myFace(myTile)
        southWestCorner = exch2_isWedge(myTile).EQ.1
     &               .AND. exch2_isSedge(myTile).EQ.1
        southEastCorner = exch2_isEedge(myTile).EQ.1
     &              .AND. exch2_isSedge(myTile).EQ.1
        northEastCorner = exch2_isEedge(myTile).EQ.1
     &              .AND. exch2_isNedge(myTile).EQ.1
        northWestCorner = exch2_isWedge(myTile).EQ.1
     &              .AND. exch2_isNedge(myTile).EQ.1
#else
        myFace = bi
        southWestCorner = .TRUE.
        southEastCorner = .TRUE.
        northWestCorner = .TRUE.
        northEastCorner = .TRUE.
#endif /* ALLOW_EXCH2 */


        IF ( southWestCorner ) THEN
         i=1
         j=1
         IF ( hZoption.EQ.1 ) THEN
          hFacZOpen=MIN(_hFacW(i,j,k,bi,bj),
     &                  _hFacW(i,j-1,k,bi,bj))
          hFacZOpen=MIN(_hFacS(i,j,k,bi,bj),hFacZOpen)
          hFacZ(i,j)=hFacZOpen
         ELSE
          IF ( MOD(myFace,2).EQ.1 ) THEN
            hFacZOpen=
     &               ( _hFacW(i,j-1,k,bi,bj)
     &                +_hFacS( i ,j,k,bi,bj) )
     &               + _hFacW(i, j ,k,bi,bj)
          ELSE
            hFacZOpen=
     &               ( _hFacW(i, j ,k,bi,bj)
     &                +_hFacW(i,j-1,k,bi,bj) )
     &               + _hFacS( i ,j,k,bi,bj)
          ENDIF
          hFacZ(i,j) = hFacZOpen / 3. _d 0
         ENDIF
        ENDIF

        IF ( southEastCorner ) THEN
         I=sNx+1
         J=1
C-    to get the same truncation, independent from the face Nb:
         IF ( hZoption.EQ.1 ) THEN
          hFacZOpen=MIN(_hFacW(i,j,k,bi,bj),
     &                  _hFacW(i,j-1,k,bi,bj))
          hFacZOpen=MIN(_hFacS(i-1,j,k,bi,bj),hFacZOpen)
          hFacZ(i,j)=hFacZOpen
         ELSE
          IF ( myFace.EQ.4 ) THEN
            hFacZOpen=
     &               ( _hFacS(i-1,j,k,bi,bj)
     &                +_hFacW(i,j-1,k,bi,bj) )
     &               + _hFacW(i, j ,k,bi,bj)
          ELSEIF ( myFace.EQ.6 ) THEN
            hFacZOpen=
     &               ( _hFacW(i,j-1,k,bi,bj)
     &                +_hFacW(i, j ,k,bi,bj) )
     &               + _hFacS(i-1,j,k,bi,bj)
          ELSE
            hFacZOpen=
     &               ( _hFacW(i, j ,k,bi,bj)
     &                +_hFacS(i-1,j,k,bi,bj) )
     &               + _hFacW(i,j-1,k,bi,bj)
          ENDIF
          hFacZ(i,j) = hFacZOpen / 3. _d 0
         ENDIF
        ENDIF

        IF ( northWestCorner ) THEN
         i=1
         j=sNy+1
C-    to get the same truncation, independent from the face Nb:
         IF ( hZoption.EQ.1 ) THEN
          hFacZOpen=MIN(_hFacW(i,j,k,bi,bj),
     &                  _hFacW(i,j-1,k,bi,bj))
          hFacZOpen=MIN(_hFacS(i,j,k,bi,bj),hFacZOpen)
          hFacZ(i,j)=hFacZOpen
         ELSE
          IF ( myFace.EQ.1 ) THEN
            hFacZOpen=
     &               ( _hFacS( i ,j,k,bi,bj)
     &                +_hFacW(i, j ,k,bi,bj) )
     &               + _hFacW(i,j-1,k,bi,bj)
          ELSEIF ( myFace.EQ.5 ) THEN
            hFacZOpen=
     &               ( _hFacW(i, j ,k,bi,bj)
     &                +_hFacW(i,j-1,k,bi,bj) )
     &               + _hFacS( i ,j,k,bi,bj)
          ELSE
            hFacZOpen=
     &               ( _hFacW(i,j-1,k,bi,bj)
     &                +_hFacS( i ,j,k,bi,bj) )
     &               + _hFacW(i, j ,k,bi,bj)
          ENDIF
          hFacZ(i,j) = hFacZOpen / 3. _d 0
         ENDIF
        ENDIF

        IF ( northEastCorner ) THEN
         i=sNx+1
         j=sNy+1
         IF ( hZoption.EQ.1 ) THEN
          hFacZOpen=MIN(_hFacW(i,j,k,bi,bj),
     &                  _hFacW(i,j-1,k,bi,bj))
          hFacZOpen=MIN(_hFacS(i-1,j,k,bi,bj),hFacZOpen)
          hFacZ(i,j)=hFacZOpen
         ELSE
          IF ( MOD(myFace,2).EQ.1 ) THEN
            hFacZOpen=
     &               ( _hFacW(i,j-1,k,bi,bj)
     &                +_hFacW(i, j ,k,bi,bj) )
     &               + _hFacS(i-1,j,k,bi,bj)
          ELSE
            hFacZOpen=
     &               ( _hFacW(i, j ,k,bi,bj)
     &                +_hFacS(i-1,j,k,bi,bj) )
     &               + _hFacW(i,j-1,k,bi,bj)
          ENDIF
          hFacZ(i,j) = hFacZOpen / 3. _d 0
         ENDIF
        ENDIF

      ENDIF
C---+----1----+----2----+----3----+----4

C--   Calculate reciprol:
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        IF (hFacZ(i,j).EQ.0.) THEN
         r_hFacZ(i,j) = 0.
        ELSE
         r_hFacZ(i,j) = 1. _d 0/hFacZ(i,j)
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_DEPTH_CONTROL */

      RETURN
      END
