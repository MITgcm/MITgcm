C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_computvort.F,v 1.5 2014/04/04 19:38:23 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_COMPUTVORT
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_COMPUTVORT(
     I           uFld, vFld,
     O           vort,
     I           k, bi,bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_COMPUTVORT
C     | o Calculate delta_i[vFld]-delta_j[uFld]
C     *==========================================================*
C     | o used in computational-mode filter to replace relative
C     |   vorticity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments
C     uFld :: velocity field (U component) on which filter applies
C     vFld :: velocity field (V component) on which filter applies
C     myThid :: Thread number for this instance of SHAP_FILT_UV_S2
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi,bj
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RS maskZ
      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
      INTEGER myFace
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif /* ALLOW_EXCH2 */
CEOP

#ifdef ALLOW_AUTODIFF
C-    Initialisation :
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          vort(i,j)= 0.
        ENDDO
      ENDDO
#endif

C-    replace Physical calc Div & Vort by computational one :
      DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
          vort(i,j) = ( vFld(i,j)-vFld(i-1,j) )
     &              - ( uFld(i,j)-uFld(i,j-1) )
          maskZ = (maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj))
     &           *(maskS(i,j,k,bi,bj)+maskS(i-1,j,k,bi,bj))
          IF (maskZ.LT.1.) vort(i,j)=0.
        ENDDO
      ENDDO

C-    Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
#ifdef ALLOW_EXCH2
         myTile = W2_myTileList(bi,bj)
         myFace = exch2_myFace(myTile)
         southWestCorner = exch2_isWedge(myTile).EQ.1
     &               .AND. exch2_isSedge(myTile).EQ.1
         southEastCorner = exch2_isEedge(myTile).EQ.1
     &               .AND. exch2_isSedge(myTile).EQ.1
         northEastCorner = exch2_isEedge(myTile).EQ.1
     &               .AND. exch2_isNedge(myTile).EQ.1
         northWestCorner = exch2_isWedge(myTile).EQ.1
     &               .AND. exch2_isNedge(myTile).EQ.1
#else
         myFace = bi
         southWestCorner = .TRUE.
         southEastCorner = .TRUE.
         northWestCorner = .TRUE.
         northEastCorner = .TRUE.
#endif /* ALLOW_EXCH2 */
C---
         IF ( southWestCorner ) THEN
           i=1
           j=1
           maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &            +maskS(i,j,k,bi,bj)
           IF (maskZ.GE.2.) THEN
             vort(i,j)=
     &          (+vFld(i,j) -uFld(i,j) ) +uFld(i,j-1)
             vort(i,j)=vort(i,j)*4. _d 0 / 3. _d 0
           ELSE
             vort(i,j)=0.
           ENDIF
         ENDIF
C---
         IF ( southEastCorner ) THEN
           i=sNx+1
           j=1
           maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &                               +maskS(i-1,j,k,bi,bj)
           IF (maskZ.GE.2.) THEN
             IF ( myFace.EQ.2 ) THEN
              vort(i,j)=
     &          (-uFld(i,j) -vFld(i-1,j) ) +uFld(i,j-1)
             ELSEIF ( myFace.EQ.4 ) THEN
              vort(i,j)=
     &          (-vFld(i-1,j) +uFld(i,j-1) ) -uFld(i,j)
             ELSE
              vort(i,j)=
     &          (+uFld(i,j-1) -uFld(i,j) ) -vFld(i-1,j)
             ENDIF
             vort(i,j)=vort(i,j)*4. _d 0 / 3. _d 0
           ELSE
             vort(i,j)=0.
           ENDIF
         ENDIF
C---
         IF ( northWestCorner ) THEN
           i=1
           j=sNy+1
           maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &            +maskS(i,j,k,bi,bj)
           IF (maskZ.GE.2.) THEN
             IF ( myFace.EQ.1 ) THEN
              vort(i,j)=
     &          (+uFld(i,j-1) +vFld(i,j) ) -uFld(i,j)
             ELSEIF ( myFace.EQ.3 ) THEN
              vort(i,j)=
     &          (-uFld(i,j) +uFld(i,j-1) ) +vFld(i,j)
             ELSE
              vort(i,j)=
     &          (+vFld(i,j) -uFld(i,j) ) +uFld(i,j-1)
             ENDIF
             vort(i,j)=vort(i,j)*4. _d 0 / 3. _d 0
           ELSE
             vort(i,j)=0.
           ENDIF
         ENDIF
C---
         IF ( northEastCorner ) THEN
           i=sNx+1
           j=sNy+1
           maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &                               +maskS(i-1,j,k,bi,bj)
           IF (maskZ.GE.2.) THEN
             IF ( MOD(myFace,2).EQ.1 ) THEN
              vort(i,j)=
     &          (-uFld(i,j) -vFld(i-1,j) ) +uFld(i,j-1)
             ELSE
              vort(i,j)=
     &          (+uFld(i,j-1) -uFld(i,j) ) -vFld(i-1,j)
             ENDIF
             vort(i,j)=vort(i,j)*4. _d 0 / 3. _d 0
           ELSE
             vort(i,j)=0.
           ENDIF
         ENDIF
C---  end if useCubedSphereExchange:
      ENDIF

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_SHAP_FILT */

      RETURN
      END
