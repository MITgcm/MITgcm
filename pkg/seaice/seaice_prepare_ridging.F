#include "SEAICE_OPTIONS.h"

CBOP
C !ROUTINE: SEAICE_PREPARE_RIDGING
C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_PREPARE_RIDGING(
#ifdef SEAICE_ITD
     O     hActual,
     O     hrMin, hrMax, hrExp, ridgeRatio, ridgingModeNorm, partFunc,
#endif /* SEAICE_ITD */
     I     iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_PREPARE_RIDGING
C     | o compute ridging parameters according to Thorndyke et al
C     |   (1975), Hibler (1980), Bitz et al (2001) or
C     |   Lipscomb et al (2007)
C     | o this routine is called from s/r seaice_do_ridging and
C     |   from s/r seaice_calc_ice_strength
C     |
C     | Martin Losch, Apr. 2014, Martin.Losch@awi.de
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     bi, bj    :: outer loop counters
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
C     i/jMin/Max:: loop boundaries
      _RL myTime
      INTEGER bi,bj
      INTEGER myIter
      INTEGER myThid
      INTEGER iMin, iMax, jMin, jMax
#ifdef SEAICE_ITD
C     ridgingModeNorm :: norm to ensure convervation (N in Lipscomb et al 2007)
C     partFunc   :: participation function (a_n in Lipscomb et al 2007)
C     ridgeRatio :: mean ridge thickness/ thickness of ridging ice
C     hrMin      :: min ridge thickness
C     hrMax      :: max ridge thickness   (SEAICEredistFunc = 0)
C     hrExp      :: ridge e-folding scale (SEAICEredistFunc = 1)
C     hActual    :: HEFFITD/AREAITD, regularized
      _RL ridgingModeNorm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL partFunc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,0:nITD)
      _RL hrMin           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hrMax           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hrExp           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL ridgeRatio      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hActual         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
CEndOfInterface

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k       :: inner loop counters
C
      INTEGER i, j
      INTEGER k
C     variables related to ridging schemes
C     gSum        :: cumulative distribution function G
      _RL gSum            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,-1:nITD)
      _RL recip_gStar, recip_aStar, tmp
C     Regularization values squared
      _RL area_reg_sq, hice_reg_sq
CEOP

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     regularization constants
      area_reg_sq = SEAICE_area_reg * SEAICE_area_reg
      hice_reg_sq = SEAICE_hice_reg * SEAICE_hice_reg
      DO k=1,nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         hActual(i,j,k) = 0. _d 0
CML         IF ( AREAITD(i,j,k,bi,bj) .GT. SEAICE_area_reg ) THEN
CML          hActual(i,j,k) = HEFFITD(i,j,k,bi,bj)/AREAITD(i,j,k,bi,bj)
CML         ENDIF
         IF ( HEFFITD(i,j,k,bi,bj) .GT. 0. _d 0 ) THEN
C     regularize as in seaice_growth: compute hActual with regularized
C     AREA and regularize from below with a minimum thickness
          tmp = HEFFITD(i,j,k,bi,bj)
     &         /SQRT( AREAITD(i,j,k,bi,bj)**2 + area_reg_sq )
          hActual(i,j,k) = SQRT(tmp * tmp + hice_reg_sq)
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     compute the cumulative thickness distribution function gSum
      DO j=jMin,jMax
       DO i=iMin,iMax
        gSum(i,j,-1) = 0. _d 0
        gSum(i,j,0)  = 0. _d 0
        IF ( opnWtrFrac(i,j,bi,bj) .GT. SEAICE_area_floor )
     &       gSum(i,j,0) = opnWtrFrac(i,j,bi,bj)
       ENDDO
      ENDDO
      DO k = 1, nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         gSum(i,j,k) = gSum(i,j,k-1)
         IF ( AREAITD(i,j,k,bi,bj) .GT. SEAICE_area_floor )
     &        gSum(i,j,k) = gSum(i,j,k) + AREAITD(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
C     normalize
      DO k = 0, nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( gSum(i,j,nITD).NE.0. _d 0 )
     &        gSum(i,j,k) = gSum(i,j,k) / gSum(i,j,nITD)
        ENDDO
       ENDDO
      ENDDO

C     Compute the participation function
C                    area lost from category n due to ridging/closing
C     partFunc(n) = --------------------------------------------------
C                       total area lost due to ridging/closing

      IF ( SEAICEpartFunc .EQ. 0 ) THEN
C     Thorndike et al. (1975) discretize b(h) = (2/Gstar) * (1 - G(h)/Gstar)
C     The expressions for the partition function partFunc are found by
C     integrating b(h)g(h) between the category boundaries.
       recip_gStar = 1. _d 0 / SEAICEgStar
       DO k = 0, nITD
        DO j=jMin,jMax
         DO i=iMin,iMax
          partFunc(i,j,k) = 0. _d 0
          IF ( gSum(i,j,k) .LT. SEAICEgStar ) THEN
           partFunc(i,j,k) =
     &          (gSum(i,j,k)-gSum(i,j,k-1)) * recip_gStar
     &          *( 2. _d 0 - (gSum(i,j,k-1)+gSum(i,j,k))*recip_gStar)
          ELSEIF (  gSum(i,j,k-1) .LT. SEAICEgStar
     &          .AND. gSum(i,j,k) .GE. SEAICEgStar ) THEN
           partFunc(i,j,k) =
     &          (SEAICEgStar-gSum(i,j,k-1)) * recip_gStar
     &          *( 2. _d 0 - (gSum(i,j,k-1)+SEAICEgStar)*recip_gStar)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ELSEIF  ( SEAICEpartFunc .EQ. 1 ) THEN
C     Lipscomb et al. (2007) discretize b(h) = exp(-G(h)/astar) into
C     partFunc(n) = [exp(-G(n-1)/astar - exp(-G(n)/astar] / [1-exp(-1/astar)].
C     The expression is found by integrating b(h)g(h) between the category
C     boundaries.
       recip_astar = 1. _d 0 / SEAICEaStar
       tmp = 1. _d 0 / ( 1. _d 0 - EXP( -recip_astar ) )
C     abuse gSum as a work array
       k = -1
       DO j=jMin,jMax
        DO i=iMin,iMax
         gSum(i,j,k)     = EXP(-gSum(i,j,k)*recip_astar) * tmp
        ENDDO
       ENDDO
       DO k = 0, nITD
        DO j=jMin,jMax
         DO i=iMin,iMax
          gSum(i,j,k)     = EXP(-gSum(i,j,k)*recip_astar) * tmp
          partFunc(i,j,k) = gSum(i,j,k-1) - gSum(i,j,k)
         ENDDO
        ENDDO
       ENDDO
      ELSE
       STOP 'Ooops: SEAICEpartFunc > 1 not implemented'
      ENDIF

C     Compute variables of ITD of ridged ice
C     ridgeRatio :: mean ridge thickness/ thickness of ridging ice
C     hrMin      :: min ridge thickness
C     hrMax      :: max ridge thickness   (SEAICEredistFunc = 0)
C     hrExp      :: ridge e-folding scale (SEAICEredistFunc = 1)
      DO k = 1, nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         hrMin(i,j,k)      = 0. _d 0
         hrMax(i,j,k)      = 0. _d 0
         hrExp(i,j,k)      = 0. _d 0
C     avoid divisions by zero
         ridgeRatio(i,j,k) = 1. _d 0
        ENDDO
       ENDDO
      ENDDO
      IF ( SEAICEredistFunc .EQ. 0 ) THEN
C     Assume ridged ice is uniformly distributed between hrmin and hrmax.
C     (Hibler, 1980)
       DO k = 1, nITD
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( hActual(i,j,k) .GT. 0. _d 0 ) THEN
C     This is the original Hibler (1980) scheme:
           hrMin(i,j,k) = 2. _d 0 * hActual(i,j,k)
           hrMax(i,j,k) = 2. _d 0 * SQRT(hActual(i,j,k)*SEAICEhStar)
C     CICE does this in addition, so that thick ridging ice is not required
C     to raft:
           hrMin(i,j,k) = MIN(hrMin(i,j,k),hActual(i,j,k)+SEAICEmaxRaft)
           hrMax(i,j,k) = MAX(hrMax(i,j,k),hrMin(i,j,k)+SEAICE_hice_reg)
C
           ridgeRatio(i,j,k) =
     &          0.5 _d 0 * (hrMax(i,j,k)+hrMin(i,j,k))/hActual(i,j,k)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ELSEIF ( SEAICEredistFunc .EQ. 1 ) THEN
C     Follow Lipscomb et al. (2007) and model ridge ITD as an exponentially
C     decaying function
       DO k = 1, nITD
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( hActual(i,j,k) .GT. 0. _d 0 ) THEN
C     regularization is only required in this case but already done above
CML           tmp = MAX(hActual(i,j,k), SEAICE_hice_reg)
           tmp = hActual(i,j,k)
           hrMin(i,j,k) = MIN(2.D0 * tmp, tmp+SEAICEmaxRaft)
           hrExp(i,j,k) = SEAICEmuRidging*SQRT(tmp)
C     arent we missing a factor 0.5 here?
           ridgeRatio(i,j,k)=(hrMin(i,j,k)+hrExp(i,j,k))/tmp
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ELSE
       STOP 'Ooops: SEAICEredistFunc > 1 not implemented'
      ENDIF

C     Compute the norm of the ridging mode N (in Lipscomp et al 2007)
C     or omega (in Bitz et al 2001):
C     rigdingModeNorm = net ice area removed / total area participating.
C     For instance, if a unit area of ice with thickness = 1 participates in
C     ridging to form a ridge with a = 1/3 and thickness = 3, then
C     rigdingModeNorm = 1 - 1/3 = 2/3.
      DO j=jMin,jMax
       DO i=iMin,iMax
        ridgingModeNorm(i,j) = partFunc(i,j,0)
       ENDDO
      ENDDO
      DO k = 1, nITD
       DO j=jMin,jMax
        DO i=iMin,iMax
         partFunc(i,j,k) = partFunc(i,j,k) * heffM(i,j,bi,bj)
         ridgingModeNorm(i,j) = ridgingModeNorm(i,j)
     &        + partFunc(i,j,k)*( 1. _d 0 - 1. _d 0/ridgeRatio(i,j,k) )
        ENDDO
       ENDDO
      ENDDO
C     avoid division by zero
      DO j=jMin,jMax
       DO i=iMin,iMax
        IF ( ridgingModeNorm(i,j) .LE. 0. _d 0 )
     &       ridgingModeNorm(i,j) = 1. _d 0
       ENDDO
      ENDDO

#endif /* SEAICE_ITD */

      RETURN
      END
