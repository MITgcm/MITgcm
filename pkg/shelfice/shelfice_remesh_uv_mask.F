#include "SHELFICE_OPTIONS.h"
CBOP
C     !ROUTINE: SHELFICE_REMESH_UV_MASK
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_UV_MASK(
     O                    k1SurfW, k1SurfS, mrgFacW, mrgFacS,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_UV_MASK
C     | o Update masks and geom factors at U and V points
C     | o save surface level index and thickness factors for
C     |   update of horizontal velocity state variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef NONLIN_FRSURF
# include "SURFACE.h"
#endif /* NONLIN_FRSURF */

C     !INPUT/OUTPUT PARAMETERS:
C     k1SurfW   :: surface level index (at cell W.Edge) before remeshing
C     k1SurfS   :: surface level index (at cell S.Edge) before remeshing
C     mrgFacW   :: store hFacW to compute merging weight for U velocity
C     mrgFacS   :: store hFacS to compute merging weight for V velocity
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      INTEGER k1SurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k1SurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL mrgFacW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL mrgFacS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE_REMESHING
#ifdef NONLIN_FRSURF
C     !LOCAL VARIABLES:
C     bi, bj    :: tile indices
C     i, j, k   :: Loop counters
C     tmpVar    :: Temporary array used to integrate column thickness
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER ks, k1, k2
      _RL tmpVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacMnSz, hFacCtmp
      _RL hFac1tmp, hFac2tmp
      _RL Rmin_tmp, hfacInfMom
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           k1SurfW(i,j,bi,bj) = kSurfW(i,j,bi,bj)
           k1SurfS(i,j,bi,bj) = kSurfS(i,j,bi,bj)
           mrgFacW(i,j,1,bi,bj) = 0.
           mrgFacW(i,j,2,bi,bj) = 0.
           mrgFacS(i,j,1,bi,bj) = 0.
           mrgFacS(i,j,2,bi,bj) = 0.
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
           rSurfW(i,j,bi,bj) =
     &           MIN( Ro_surf(i-1,j,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfW(i,j,bi,bj) =
     &           MAX( rSurfW(i,j,bi,bj), rLowW(i,j,bi,bj) )
         ENDDO
        ENDDO
        DO j=2-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           rSurfS(i,j,bi,bj) =
     &           MIN( Ro_surf(i,j-1,bi,bj), Ro_surf(i,j,bi,bj) )
           rSurfS(i,j,bi,bj) =
     &           MAX( rSurfS(i,j,bi,bj), rLowS(i,j,bi,bj) )
         ENDDO
        ENDDO

       ENDDO
      ENDDO

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
           h0FacW(1-OLx,j,k,bi,bj) = 0.
           DO i=2-OLx,sNx+OLx
            h0FacW(i,j,k,bi,bj) =
     &        MIN( h0FacC(i,j,k,bi,bj), h0FacC(i-1,j,k,bi,bj) )
           ENDDO
          ENDDO
          DO i=1-OLx,sNx+OLx
            h0FacS(i,1-OLy,k,bi,bj) = 0.
          ENDDO
          DO j=2-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            h0FacS(i,j,k,bi,bj) =
     &        MIN( h0FacC(i,j,k,bi,bj), h0FacC(i,j-1,k,bi,bj) )
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
              h0FacW(i,j,k,bi,bj) = 0.
            ELSE
              h0FacW(i,j,k,bi,bj) = MAX( hFacCtmp, hFacMnSz )
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
              h0FacS(i,j,k,bi,bj) = 0.
            ELSE
              h0FacS(i,j,k,bi,bj) = MAX( hFacCtmp, hFacMnSz )
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C--   Update rLow & reference rSurf at Western & Southern edges (U & V pts):
C     account for adjusted R_low & Ro_surf due to hFacMin constrain on hFacC.
C     Might need further adjustment (e.g., if useShelfIce=T) to match
C     integrated level thickness ( =Sum_k(drF*hFac) )

C--   Adjust rLow & reference rSurf at Western & Southern edges (U & V pts)
C     to get consistent column thickness from Sum_k(hFac*drF) and rSurf-rLow

C-    Total column thickness at Western edges
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           tmpVar(i,j) = 0. _d 0
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            tmpVar(i,j) = tmpVar(i,j) + drF(k)*h0FacW(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C-    Adjust only rSurf at W (correct for the difference)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           rSurfW(i,j,bi,bj) = rLowW(i,j,bi,bj) + tmpVar(i,j)
         ENDDO
        ENDDO

C-    Total column thickness at Southern edges
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           tmpVar(i,j) = 0. _d 0
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            tmpVar(i,j) = tmpVar(i,j) + drF(k)*h0FacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
C-    Adjust only rSurf at edges (correct for the difference)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           rSurfS(i,j,bi,bj) = rLowS(i,j,bi,bj) + tmpVar(i,j)
         ENDDO
        ENDDO

C-    end bi,bj loops.
       ENDDO
      ENDDO

      CALL EXCH_UV_XYZ_RS( h0FacW,  h0FacS, .FALSE., myThid )
      CALL EXCH_UV_XY_RS( rSurfW, rSurfS, .FALSE., myThid )

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C--   Account for additional closing of Western and Southern grid-cell edges
C     ( which jave been reccorded as kSurfW/S = Nr+2 )
C     and calculate surface k index for interface W & S (U & V points)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          kSurfW(i,j,bi,bj) = MAX( k1SurfW(i,j,bi,bj), Nr+1 )
          kSurfS(i,j,bi,bj) = MAX( k1SurfS(i,j,bi,bj), Nr+1 )
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( kSurfW(i,j,bi,bj).EQ.Nr+2 ) THEN
             h0FacW(i,j,k,bi,bj) = zeroRS
           ELSEIF ( kSurfW(i,j,bi,bj).EQ.Nr+1 ) THEN
             IF (h0FacW(i,j,k,bi,bj).NE.zeroRS) kSurfW(i,j,bi,bj) = k
           ENDIF
           IF ( kSurfS(i,j,bi,bj).EQ.Nr+2 ) THEN
             h0FacS(i,j,k,bi,bj) = zeroRS
           ELSEIF ( kSurfS(i,j,bi,bj).EQ.Nr+1 ) THEN
             IF (h0FacS(i,j,k,bi,bj).NE.zeroRS) kSurfS(i,j,bi,bj) = k
           ENDIF
          ENDDO
         ENDDO
        ENDDO

C-    end bi,bj loops.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--  Compute the mimimum value of r_surf (used for computing hFac_surfC)
        hFacInfMOM = hFacInf
        DO j=1,sNy
         DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           IF (ks.LE.Nr) THEN
             Rmin_tmp = rF(ks+1)
             IF ( ks.EQ.kSurfW(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i-1,j,bi,bj))
             IF ( ks.EQ.kSurfW(i+1,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i+1,j,bi,bj))
             IF ( ks.EQ.kSurfS(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j-1,bi,bj))
             IF ( ks.EQ.kSurfS(i,j+1,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j+1,bi,bj))

             Rmin_surf(i,j,bi,bj) =
     &        MAX( MAX(rF(ks+1),R_low(i,j,bi,bj)) + hFacInf*drF(ks),
     &                                Rmin_tmp + hFacInfMOM*drF(ks)
     &           )
           ENDIF
         ENDDO
        ENDDO

C--  Prepare merging weights for both components of horizontal velocity;
C    they will get updated once new thickness factor are available
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kSurfW(i,j,bi,bj).NE.k1SurfW(i,j,bi,bj) ) THEN
            k1 = k1SurfW(i,j,bi,bj)
            k2 =  kSurfW(i,j,bi,bj)
            IF ( k2.GT.k1 .AND. k2.LE.Nr ) THEN
C-      merging former (=k1) into new (=k2) surface grid cell
              mrgFacW(i,j,1,bi,bj) = hFacW(i,j,k1,bi,bj)
              mrgFacW(i,j,2,bi,bj) = hFacW(i,j,k2,bi,bj)
            ENDIF
          ENDIF
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kSurfS(i,j,bi,bj).NE.k1SurfS(i,j,bi,bj) ) THEN
            k1 = k1SurfS(i,j,bi,bj)
            k2 =  kSurfS(i,j,bi,bj)
            IF ( k2.GT.k1 .AND. k2.LE.Nr ) THEN
C-      merging former (=k1) into new (=k2) surface grid cell
              mrgFacS(i,j,1,bi,bj) = hFacS(i,j,k1,bi,bj)
              mrgFacS(i,j,2,bi,bj) = hFacS(i,j,k2,bi,bj)
            ENDIF
          ENDIF
         ENDDO
        ENDDO

C--   Masks and reciprocals of hFac[CWS]
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF ( h0FacW(i,j,k,bi,bj).NE.zeroRS ) THEN
            recip_hFacW(i,j,k,bi,bj) = 1. _d 0 / h0FacW(i,j,k,bi,bj)
            maskW(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacW(i,j,k,bi,bj) = 0.
            maskW(i,j,k,bi,bj) = 0.
           ENDIF
           IF ( h0FacS(i,j,k,bi,bj).NE.zeroRS ) THEN
            recip_hFacS(i,j,k,bi,bj) = 1. _d 0 / h0FacS(i,j,k,bi,bj)
            maskS(i,j,k,bi,bj) = 1.
           ELSE
            recip_hFacS(i,j,k,bi,bj) = 0.
            maskS(i,j,k,bi,bj) = 0.
           ENDIF
          ENDDO
         ENDDO
        ENDDO

C--   Copy initial geometrical h0FacW & h0FacW into hFac factor.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           hFacW(i,j,k,bi,bj) = h0FacW(i,j,k,bi,bj)
           hFacS(i,j,k,bi,bj) = h0FacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO

C-    end bi,bj loops.
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( Rmin_surf, myThid )

#endif /* NONLIN_FRSURF */
#endif /* ALLOW_SHELFICE_REMESHING */

      RETURN
      END
