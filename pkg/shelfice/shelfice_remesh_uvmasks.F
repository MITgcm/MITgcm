#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: UPD_MASKS_REMESH
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_UVMASKS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_UVMASKS
C     | o Update masks and geom factors at U and V points
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef NONLIN_FRSURF
# include "SURFACE.h"
#endif /* NONLIN_FRSURF */

C     !INPUT/OUTPUT PARAMETERS:
C     myThid    ::  my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi, bj    :: tile indices
C     i, j, k   :: Loop counters
C     tmpVar*   :: Temporary array used to integrate column thickness
      INTEGER bi, bj
      INTEGER i, j, k
      _RL tmpVar1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpVar2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacMnSz, hFacCtmp
      _RL hFac1tmp, hFac2tmp
      _RS rEmpty
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_SHELFICE
#ifdef ALLOW_SHELFICE_REMESHING

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( selectSigmaCoord.EQ.0 ) THEN
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C- Method-1 (useMin4hFacEdges = T):
C    compute hFacW,hFacS as minimum of adjacent hFacC factor
C- Method-2 (useMin4hFacEdges = F):
C    compute hFacW,hFacS from rSurfW,S and rLowW,S by applying
C    same rules as for hFacC
C Note: Currently, no difference between methods except when useShelfIce=T and
C       if, in adjacent columns, ice-draft and bathy are within the same level k

        IF ( useMin4hFacEdges ) THEN
C--   hFacW and hFacS (at U and V points):
C-    Method-1: use simply minimum of adjacent hFacC factor

         DO k=1, Nr
          DO j=1-OLy,sNy+OLy
           hFacW(1-OLx,j,k,bi,bj) = 0.
           DO i=2-OLx,sNx+OLx
            hFacW(i,j,k,bi,bj) =
     &        MIN( hFacC(i,j,k,bi,bj), hFacC(i-1,j,k,bi,bj) )
           ENDDO
          ENDDO
          DO i=1-OLx,sNx+OLx
            hFacS(i,1-OLy,k,bi,bj) = 0.
          ENDDO
          DO j=2-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            hFacS(i,j,k,bi,bj) =
     &        MIN( hFacC(i,j,k,bi,bj), hFacC(i,j-1,k,bi,bj) )
           ENDDO
          ENDDO
         ENDDO

        ELSE
C--   hFacW and hFacS (at U and V points):
C-    Method-2: compute new hFacW,S from rSurfW,S and rLowW,S
C               by applying same rules as for hFacC

         DO k=1, Nr
          hFacMnSz = MAX( hFacMin, MIN(hFacMinDr*recip_drF(k),oneRL) )
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
C      o Non-dimensional distance between grid bound. and domain lower_R bound.
            hFac1tmp = ( rF(k) - rLowW(i,j,bi,bj) )*recip_drF(k)
            hFacCtmp = MIN( hFac1tmp, oneRL )
c           hFacCtmp = MAX( hFacCtmp, zeroRL )
C      o Impose minimum fraction and/or size (dimensional)
            IF ( hFacCtmp.LT.hFacMnSz*halfRL ) THEN
              hFac1tmp = 0.
            ELSE
              hFac1tmp = MAX( hFacCtmp, hFacMnSz )
            ENDIF
C      o Reduce the previous fraction : substract the outside fraction
C        (i.e., beyond reference (=at rest) surface position rSurfW)
            hFac2tmp = ( rF(k) -rSurfW(i,j,bi,bj) )*recip_drF(k)
            hFacCtmp = hFac1tmp - MAX( hFac2tmp, zeroRL )
C      o Impose minimum fraction and/or size (dimensional)
            IF ( hFacCtmp.LT.hFacMnSz*halfRL ) THEN
              hFacW(i,j,k,bi,bj) = 0.
            ELSE
              hFacW(i,j,k,bi,bj) = MAX( hFacCtmp, hFacMnSz )
            ENDIF
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
C      o Non-dimensional distance between grid bound. and domain lower_R bound.
            hFac1tmp = ( rF(k) - rLowS(i,j,bi,bj) )*recip_drF(k)
            hFacCtmp = MIN( hFac1tmp, oneRL )
c           hFacCtmp = MAX( hFacCtmp, zeroRL )
C      o Impose minimum fraction and/or size (dimensional)
            IF ( hFacCtmp.LT.hFacMnSz*halfRL ) THEN
              hFac1tmp = 0.
            ELSE
              hFac1tmp = MAX( hFacCtmp, hFacMnSz )
            ENDIF
C      o Reduce the previous fraction : substract the outside fraction
C        (i.e., beyond reference (=at rest) surface position rSurfS)
            hFac2tmp = ( rF(k) -rSurfS(i,j,bi,bj) )*recip_drF(k)
            hFacCtmp = hFac1tmp - MAX( hFac2tmp, zeroRL )
C      o Impose minimum fraction and/or size (dimensional)
            IF ( hFacCtmp.LT.hFacMnSz*halfRL ) THEN
              hFacS(i,j,k,bi,bj) = 0.
            ELSE
              hFacS(i,j,k,bi,bj) = MAX( hFacCtmp, hFacMnSz )
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C--   Update rLow & reference rSurf at Western & Southern edges (U & V pts):
C     account for adjusted R_low & Ro_surf due to hFacMin constrain on hFacC.
C     Might need further adjustment (e.g., if useShelfIce=T) to match
C     integrated level thickness ( =Sum_k(drF*hFac) )
        DO j=1-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
           rLowW(i,j,bi,bj)  =
     &           MAX(   R_low(i-1,j,bi,bj),   R_low(i,j,bi,bj) )
           rSurfW(i,j,bi,bj) =
     &           MIN( Ro_surf(i-1,j,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfW(i,j,bi,bj) =
     &           MAX( rSurfW(i,j,bi,bj), rLowW(i,j,bi,bj) )
         ENDDO
        ENDDO
        DO j=2-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           rLowS(i,j,bi,bj)  =
     &           MAX(   R_low(i,j-1,bi,bj),   R_low(i,j,bi,bj) )
           rSurfS(i,j,bi,bj) =
     &           MIN( Ro_surf(i,j-1,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfS(i,j,bi,bj) =
     &           MAX( rSurfS(i,j,bi,bj), rLowS(i,j,bi,bj) )
         ENDDO
        ENDDO

c       IF ( useShelfIce ) THEN
C--   Adjust rLow & reference rSurf at Western & Southern edges (U & V pts)
C     to get consistent column thickness from Sum_k(hFac*drF) and rSurf-rLow

C-    Total column thickness at Western & Southern edges
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            tmpVar1(i,j) = 0. _d 0
            tmpVar2(i,j) = 0. _d 0
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            tmpVar1(i,j) = tmpVar1(i,j) + drF(k)*hFacW(i,j,k,bi,bj)
            tmpVar2(i,j) = tmpVar2(i,j) + drF(k)*hFacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO

c        IF ( useMin4hFacEdges ) THEN
         IF ( .TRUE. ) THEN
C-    Adjust only rSurf at W and S edges (correct for the difference)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             rSurfW(i,j,bi,bj) = rLowW(i,j,bi,bj) + tmpVar1(i,j)
             rSurfS(i,j,bi,bj) = rLowS(i,j,bi,bj) + tmpVar2(i,j)
            ENDDO
           ENDDO
         ELSE
C-    Adjust both rLow and rSurf at W & S edges (split correction by half)
C     adjust rSurfW and rLowW:
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             tmpVar1(i,j) = rLowW(i,j,bi,bj) + tmpVar1(i,j)
             tmpVar1(i,j) = ( tmpVar1(i,j) -rSurfW(i,j,bi,bj) )*halfRL
            ENDDO
           ENDDO
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             rSurfW(i,j,bi,bj) = rSurfW(i,j,bi,bj) + tmpVar1(i,j)
             rLowW (i,j,bi,bj) = rLowW (i,j,bi,bj) - tmpVar1(i,j)
            ENDDO
           ENDDO
C     Adjust rSurfS and rLowS:
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             tmpVar2(i,j) = rLowS(i,j,bi,bj) + tmpVar2(i,j)
             tmpVar2(i,j) = ( tmpVar2(i,j) -rSurfS(i,j,bi,bj) )*halfRL
            ENDDO
           ENDDO
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             rSurfS(i,j,bi,bj) = rSurfS(i,j,bi,bj) + tmpVar2(i,j)
             rLowS (i,j,bi,bj) = rLowS (i,j,bi,bj) - tmpVar2(i,j)
            ENDDO
           ENDDO
         ENDIF

C-    end if useShelfIce
c       ENDIF

C-    end bi,bj loops.
       ENDDO
      ENDDO

      CALL EXCH_UV_XYZ_RS( hFacW,  hFacS, .FALSE., myThid )
      CALL EXCH_UV_XY_RS( rSurfW, rSurfS, .FALSE., myThid )
      CALL EXCH_UV_XY_RS(  rLowW,  rLowS, .FALSE., myThid )

C--   Calculate surface k index for interface W & S (U & V points)
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          kSurfW(i,j,bi,bj) = Nr+1
          kSurfS(i,j,bi,bj) = Nr+1
          DO k=Nr,1,-1
           IF (hFacW(i,j,k,bi,bj).NE.zeroRS) kSurfW(i,j,bi,bj) = k
           IF (hFacS(i,j,k,bi,bj).NE.zeroRS) kSurfS(i,j,bi,bj) = k
          ENDDO
!          maskInW(i,j,bi,bj)= 0.
!          IF ( kSurfW(i,j,bi,bj).LE.Nr ) maskInW(i,j,bi,bj)= 1.
!          maskInS(i,j,bi,bj)= 0.
!          IF ( kSurfS(i,j,bi,bj).LE.Nr ) maskInS(i,j,bi,bj)= 1.
! DNG do not need to update these for vertical remeshing
! DNG but will need to update for horizontal remeshing
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Additional closing of Western and Southern grid-cell edges: for example,
C     a) might add some "thin walls" in specific location
C     b) close non-periodic N & S boundaries of lat-lon grid at the N/S poles.
C     new: location now reccorded as kSurfW/S = Nr+2
      rEmpty = rF(1)
      CALL ADD_WALLS2MASKS( rEmpty, myThid )

      ELSE

      ENDIF ! sigma_coord==0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write to disk: Total Column Thickness & hFac(C,W,S):
C     This I/O is now done in write_grid.F
c     CALL WRITE_FLD_XYZ_RS( 'hFacC',' ',hFacC,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacW',' ',hFacW,0,myThid)
c     CALL WRITE_FLD_XYZ_RS( 'hFacS',' ',hFacS,0,myThid)

C--   Masks and reciprocals of hFac[CWS]
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( hFacC(i,j,k,bi,bj).NE.zeroRS ) THEN
            recip_hFacC(i,j,k,bi,bj) = 1. _d 0 / hFacC(i,j,k,bi,bj)
            maskC(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacC(i,j,k,bi,bj) = 0.
            maskC(i,j,k,bi,bj) = 0.
           ENDIF
           IF ( hFacW(i,j,k,bi,bj).NE.zeroRS ) THEN
            recip_hFacW(i,j,k,bi,bj) = 1. _d 0 / hFacW(i,j,k,bi,bj)
            maskW(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacW(i,j,k,bi,bj) = 0.
            maskW(i,j,k,bi,bj) = 0.
           ENDIF
           IF ( hFacS(i,j,k,bi,bj).NE.zeroRS ) THEN
            recip_hFacS(i,j,k,bi,bj) = 1. _d 0 / hFacS(i,j,k,bi,bj)
            maskS(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacS(i,j,k,bi,bj) = 0.
            maskS(i,j,k,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO
#ifdef NONLIN_FRSURF
C--   Save initial geometrical hFac factor into h0Fac (fixed in time):
C     Note: In case 1 pkg modifies hFac (from packages_init_fixed, called
C     later in sequence of calls) this pkg would need also to update h0Fac.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           h0FacC(i,j,k,bi,bj) = _hFacC(i,j,k,bi,bj)
           h0FacW(i,j,k,bi,bj) = _hFacW(i,j,k,bi,bj)
           h0FacS(i,j,k,bi,bj) = _hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#endif /* NONLIN_FRSURF */
C-    end bi,bj loops.
       ENDDO
      ENDDO
!      DO bj = myByLo(myThid), myByHi(myThid)
!       DO bi = myBxLo(myThid), myBxHi(myThid)
!        DO k=1,Nr
!         DO j=1-OLy,sNy+OLy
!          DO i=1-OLx,sNx+OLx
!            uVel(i,j,k,bi,bj)=uVel(i,j,k,bi,bj)*maskW(i,j,k,bi,bj)
!            vVel(i,j,k,bi,bj)=vVel(i,j,k,bi,bj)*maskS(i,j,k,bi,bj)
!            wVel(i,j,k,bi,bj)=0.0
!            salt(i,j,k,bi,bj)=salt(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
!            theta(i,j,k,bi,bj)=theta(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
!
!          ENDDO
!         ENDDO
!        ENDDO
!       ENDDO
!      ENDDO

c #ifdef ALLOW_NONHYDROSTATIC
C--   Calculate "recip_hFacU" = reciprocal hfac distance/volume for W cells
C NOTE:  not used ; computed locally in CALC_GW
c #endif
#endif
#endif

      RETURN
      END
