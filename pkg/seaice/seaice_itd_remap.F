C     contains:
C     S/R SEAICE_ITD_REMAP
C     S/R SEAICE_ITD_REMAP_LINEAR
C     S/R SEAICE_ITD_REMAP_CHECK_BOUNDS

#include "SEAICE_OPTIONS.h"

CBOP
C !ROUTINE: SEAICE_ITD_REMAP

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ITD_REMAP(
     I     heffitdpre, areaitdpre,
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ITD_REMAP
C     | o checks if absolute ice thickness in any category
C     |   exceeds its category limits
C     | o remaps sea ice area and volume
C     |   and associated ice properties in thickness space
C     |   following the remapping scheme of Lipscomb (2001), JGR
C     |
C     | Martin Losch, started in May 2014, Martin.Losch@awi.de
C     | with many fixes by Mischa Ungermann (MU)
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables to be checked and remapped ===
C     AREAITD   :: sea ice area      by category
C     HEFFITD   :: sea ice thickness by category
C
C     === Global variables to be remappped ===
C     HSNOWITD  :: snow thickness    by category
C     enthalpy ?
C     temperature ?
C     salinity ?
C     age ?
C
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     bi, bj    :: outer loop counters
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER bi,bj
      INTEGER myIter
      INTEGER myThid
      _RL heffitdPre  (1:sNx,1:sNy,1:nITD)
      _RL areaitdPre  (1:sNx,1:sNy,1:nITD)

#ifdef SEAICE_ITD

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k       :: inner loop counters
C
      INTEGER i, j, k
      INTEGER kDonor, kRecvr
      _RL slope, area_reg_sq, hice_reg_sq
      _RL etaMin, etaMax, etam, etap, eta2
      _RL dh0, da0, daMax
CMU      _RL oneMinusEps
      _RL third
      PARAMETER ( third = 0.333333333333333333333333333 _d 0 )
C
      _RL dhActual    (1:sNx,1:sNy,1:nITD)
      _RL hActual     (1:sNx,1:sNy,1:nITD)
      _RL hActualPre  (1:sNx,1:sNy,1:nITD)
      _RL dheff, darea, dhsnw
C
      _RL hLimitNew   (1:sNx,1:sNy,0:nITD)
C     coefficients for represent g(h)
C     g0 :: constant coefficient in g(h)
C     g1 :: linear  coefficient in g(h)
C     hL :: left end of range over which g(h) > 0
C     hL :: right end of range over which g(h) > 0
      _RL g0 (1:sNx,1:sNy,0:nITD)
      _RL g1 (1:sNx,1:sNy,0:nITD)
      _RL hL (1:sNx,1:sNy,0:nITD)
      _RL hR (1:sNx,1:sNy,0:nITD)
C     local copy of AREAITD
      _RL aLoc(1:sNx,1:sNy)
      LOGICAL doRemapping (1:sNx,1:sNy)
CEOP
C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     constants
      area_reg_sq = SEAICE_area_reg**2
      hice_reg_sq = SEAICE_hice_reg**2
CMU      oneMinusEps = 1. _d 0 - SEAICE_eps
C     initialisation
      DO j=1,sNy
       DO i=1,sNx
        doRemapping(i,j) = .FALSE.
        IF ( HEFFM(i,j,bi,bj) .NE. 0. _d 0 ) doRemapping(i,j) = .TRUE.
       ENDDO
      ENDDO
C     do not compute regularized hActual as in seaice_growth, because
C     with regularization, hActual deviates too much from the actual
C     category boundaries and the boundary computation fails too often.
      DO k=1,nITD
       DO j=1,sNy
        DO i=1,sNx
         hActualPre (i,j,k) = 0. _d 0
         hActual (i,j,k) = 0. _d 0
         dhActual(i,j,k) = 0. _d 0
         IF (.FALSE.) THEN
          IF ( areaitdPre(i,j,k) .GT. 0. _d 0 ) THEN
           hActualPre(i,j,k) = heffitdPre(i,j,k)
     &         /SQRT( areaitdPre(i,j,k)**2 + area_reg_sq )
CML           hActualPre(i,j,k) = SQRT( hActualPre(i,j,k)**2 + hice_reg_sq )
          ENDIF
          IF ( AREAITD(i,j,k,bi,bj) .GT. 0. _d 0 ) THEN
           hActual(i,j,k) = HEFFITD(i,j,k,bi,bj)
     &         /SQRT( AREAITD(i,j,k,bi,bj)**2 + area_reg_sq )
CML           hActual(i,j,k) = SQRT( hActual(i,j,k)**2 + hice_reg_sq )
          ENDIF
          dhActual(i,j,k) = hActual(i,j,k) - hActualPre(i,j,k)
         ELSE
          IF ( areaitdPre(i,j,k) .GT. SEAICE_area_reg ) THEN
           hActualPre(i,j,k) = heffitdPre(i,j,k)/areaitdPre(i,j,k)
          ENDIF
          IF ( AREAITD(i,j,k,bi,bj) .GT. SEAICE_area_reg ) THEN
           hActual(i,j,k) = HEFFITD(i,j,k,bi,bj)/AREAITD(i,j,k,bi,bj)
          ENDIF
          dhActual(i,j,k) = hActual(i,j,k) - hActualPre(i,j,k)
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C
C     compute new category boundaries
C
      DO j=1,sNy
       DO i=1,sNx
        hLimitNew(i,j,0) = hLimit(0)
       ENDDO
      ENDDO
      DO k=1,nITD-1
       DO j=1,sNy
        DO i=1,sNx
         IF ( hActualPre(i,j,k)  .GT.SEAICE_eps .AND.
     &        hActualPre(i,j,k+1).GT.SEAICE_eps ) THEN
          slope = ( dhActual(i,j,k+1) - dhActual(i,j,k) )
     &         /( hActualPre(i,j,k+1) - hActualPre(i,j,k) )
          hLimitNew(i,j,k) =   hLimit(k) + dhActual(i,j,k)
     &         +     slope * ( hLimit(k) - hActualPre(i,j,k) )
         ELSEIF ( hActualPre(i,j,k)  .GT.SEAICE_eps ) THEN
          hLimitNew(i,j,k) = hLimit(k) + dhActual(i,j,k)
         ELSEIF ( hActualPre(i,j,k+1).GT.SEAICE_eps ) THEN
          hLimitNew(i,j,k) = hLimit(k) + dhActual(i,j,k+1)
         ELSE
          hLimitNew(i,j,k) = hLimit(k)
         ENDIF
C     After computing the new boundary, check
C     (1) if it is between two adjacent thicknesses
         IF ( ( AREAITD(i,j,k,bi,bj).GT.SEAICE_area_reg .AND.
     &          hActual(i,j,k) .GE. hLimitNew(i,j,k) ) .OR.
     &        ( AREAITD(i,j,k+1,bi,bj).GT.SEAICE_area_reg .AND.
     &          hActual(i,j,k+1) .LE. hLimitNew(i,j,k) ) )
     &        doRemapping(i,j) = .FALSE.
C     (2) that it is been the old boudnaries k-1 and k+1
C     (Note from CICE: we could allow this, but would make the code
C     more complicated)
         IF ( ( hLimitNew(i,j,k) .GT. hLimit(k+1) ) .OR.
     &        ( hLimitNew(i,j,k) .LT. hLimit(k-1) ) )
     &        doRemapping(i,j) = .FALSE.
        ENDDO
       ENDDO
      ENDDO
C     Report problems, if there are any. Because this breaks optimization
C     do not do it by default.
C     Where doRemapping is false, the rebinning of seaice_itd_redist
C     (called at the end) will take care of shifting the ice.
      IF ( debugLevel.GE.debLevA )
     &     CALL SEAICE_ITD_REMAP_CHECK_BOUNDS(
     I     AREAITD, hActual, hActualPre, hLimitNew, doRemapping,
     I     bi, bj, myTime, myIter, myThid )
C     computing the upper limit of the thickest category does not require
C     any checks and can be computed now
      k = nITD
      DO j=1,sNy
       DO i=1,sNx
        hLimitNew(i,j,k) = hLimit(k)
        IF ( AREAITD(i,j,k,bi,bj).GT.SEAICE_area_reg )
     &       hLimitNew(i,j,k) = MAX( 3. _d 0*hActual(i,j,k)
     &       - 2. _d 0 * hLimitNew(i,j,k-1), hLimit(k-1) )
       ENDDO
      ENDDO
C
C     end of limit computation, now compute the coefficients of the
C     linear approximations of g(h) => g(eta) = g0 + g1*eta
C
C     CICE does something specical for the first category.
C     compute coefficients for 1st category
      k = 1
      DO j=1,sNy
       DO i=1,sNx
C     initialisation
        aLoc(i,j) = AREAITD(i,j,k,bi,bj)
C     initialise hL and hR
C     this single line is different from the code that follows below
C     for all categories
        hL(i,j,k) = hLimitNew(i,j,k-1)
        hR(i,j,k) = hLimit(k)
       ENDDO
      ENDDO
      CALL SEAICE_ITD_REMAP_LINEAR(
     O     g0(1,1,k), g1(1,1,k),
     U     hL(1,1,k), hR(1,1,k),
     I     hActual(1,1,k), aLoc,
     I     SEAICE_area_reg, SEAICE_eps, doRemapping,
     I     myTime, myIter, myThid )
C
C     Find area lost due to melting of thin (category 1) ice
C
      DO j=1,sNy
       DO i=1,sNx
        IF ( doRemapping(i,j) .AND.
     &       AREAITD(i,j,k,bi,bj) .GT. SEAICE_area_reg ) THEN
CMU if melting of ice in category 1
         IF ( dhActual(i,j,k) .LT. 0. _d 0 ) THEN
C     integrate g(1) from zero to abs(dhActual)
CMU dh0 is max thickness of ice in first category that is melted
          dh0    = MIN(-dhActual(i,j,k),hLimit(k))
          etaMax = MIN(dh0,hR(i,j,k)) - hL(i,j,k)
          IF ( etaMax > 0. _d 0 ) THEN
CMU da0 is /int_0^dh0 g dh
           da0 = g0(i,j,k)*etaMax + g1(i,j,k)*etaMax*etaMax*0.5 _d 0
           daMax = AREAITD(i,j,k,bi,bj)
     &          * ( 1. _d 0 - hActual(i,j,k)/hActualPre(i,j,k))
           da0 = MIN( da0, daMax )
CMU adjust thickness to conserve volume
           IF ( (AREAITD(i,j,k,bi,bj)-da0) .GT. SEAICE_area_reg ) THEN
             hActual(i,j,k) = hActual(i,j,k)
     &            * AREAITD(i,j,k,bi,bj)/( AREAITD(i,j,k,bi,bj) - da0 )
           ELSE
             hActual(i,j,k) = ZERO
             da0 = AREAITD(i,j,k,bi,bj)
           ENDIF
CMU increase open water fraction
           AREAITD(i,j,k,bi,bj) = AREAITD(i,j,k,bi,bj) - da0
          ENDIF
         ELSE
CMU H_0* = F_0 * dT
          hLimitNew(i,j,k-1) = MIN( dhActual(i,j,k), hLimit(k) )
         ENDIF
        ENDIF
       ENDDO
      ENDDO
C
C     compute all coefficients
C
      DO k=1,nITD
       DO j=1,sNy
        DO i=1,sNx
C     initialisation
         aLoc(i,j) = AREAITD(i,j,k,bi,bj)
C     initialise hL and hR
         hL(i,j,k) = hLimitNew(i,j,k-1)
         hR(i,j,k) = hLimitNew(i,j,k)
        ENDDO
       ENDDO
       CALL SEAICE_ITD_REMAP_LINEAR(
     O      g0(1,1,k), g1(1,1,k),
     U      hL(1,1,k), hR(1,1,k),
     I      hActual(1,1,k), aLoc,
     I      SEAICE_area_reg, SEAICE_eps, doRemapping,
     I      myTime, myIter, myThid )
      ENDDO
C
      DO k=1,nITD-1
       DO j=1,sNy
        DO i=1,sNx
         dheff = 0. _d 0
         darea = 0. _d 0
         IF ( doRemapping(i,j) ) THEN
C     compute integration limits in eta space
          IF ( hLimitNew(i,j,k) .GT. hLimit(k) ) THEN
           etaMin = MAX(       hLimit(k), hL(i,j,k)) - hL(i,j,k)
           etaMax = MIN(hLimitNew(i,j,k), hR(i,j,k)) - hL(i,j,k)
           kDonor = k
           kRecvr = k+1
          ELSE
           etaMin = 0. _d 0
           etaMax = MIN(hLimit(k), hR(i,j,k+1)) - hL(i,j,k+1)
           kDonor = k+1
           kRecvr = k
          ENDIF
C     compute the area and volume to be moved
          IF ( etaMax .GT. etaMin ) THEN
           etam  = etaMax-etaMin
           etap  = etaMax+etaMin
           eta2  = 0.5*etam*etap
           darea = g0(i,j,kDonor)*etam + g1(i,j,kDonor)*eta2
CML           dheff = g0(i,j,kDonor)*eta2
CML     &          +  g1(i,j,kDonor)*etam*(etap*etap-etaMax*etaMin)*third
CML     &          +  darea*hL(i,j,kDonor)
           dheff = g0(i,j,kDonor)*eta2
     &          +  g1(i,j,kDonor)*(etaMax**3-etaMin**3)*third
     &          +  darea*hL(i,j,kDonor)
          ENDIF
C     ... or shift entire category, if nearly all ice is to be shifted.
CMU          IF ( (darea .GT.AREAITD(i,j,kDonor,bi,bj)*oneMinusEps).OR.
CMU     &         (dheff .GT.HEFFITD(i,j,kDonor,bi,bj)*oneMinusEps) ) THEN
          IF ( (darea .GT.AREAITD(i,j,kDonor,bi,bj)-SEAICE_eps).OR.
     &         (dheff .GT.HEFFITD(i,j,kDonor,bi,bj)-SEAICE_eps) ) THEN
           darea = AREAITD(i,j,kDonor,bi,bj)
           dheff = HEFFITD(i,j,kDonor,bi,bj)
          ENDIF
C     regularize: reset to zero, if there is too little ice to be shifted ...
CMU          IF ( (darea .LT. AREAITD(i,j,kDonor,bi,bj)*SEAICE_eps).OR.
CMU     &         (dheff .LT. HEFFITD(i,j,kDonor,bi,bj)*SEAICE_eps) ) THEN
          IF ( (darea .LT. SEAICE_eps).OR.
     &         (dheff .LT. SEAICE_eps) ) THEN
           darea  = 0. _d 0
           dheff  = 0. _d 0
          ENDIF
C     snow scaled by area
          IF ( AREAITD(i,j,kDonor,bi,bj) .GT. SEAICE_area_reg ) THEN
C     snow scaled by area (why not volume?), CICE also does it in this way
           dhsnw = darea/AREAITD(i,j,kDonor,bi,bj)
     &          * HSNOWITD(i,j,kDonor,bi,bj)
CMU          IF ( HEFFITD(i,j,kDonor,bi,bj) .GT. SEAICE_hice_reg ) THEN
CMU           dhsnw = dheff/HEFFITD(i,j,kDonor,bi,bj)
CMU     &         * HSNOWITD(i,j,kDonor,bi,bj)
          ELSE
           dhsnw = HSNOWITD(i,j,kDonor,bi,bj)
          ENDIF
C     apply increments
          HEFFITD(i,j,kRecvr,bi,bj) = HEFFITD(i,j,kRecvr,bi,bj) + dheff
          HEFFITD(i,j,kDonor,bi,bj) = HEFFITD(i,j,kDonor,bi,bj) - dheff
          AREAITD(i,j,kRecvr,bi,bj) = AREAITD(i,j,kRecvr,bi,bj) + darea
          AREAITD(i,j,kDonor,bi,bj) = AREAITD(i,j,kDonor,bi,bj) - darea
          HSNOWITD(i,j,kRecvr,bi,bj)=HSNOWITD(i,j,kRecvr,bi,bj) + dhsnw
          HSNOWITD(i,j,kDonor,bi,bj)=HSNOWITD(i,j,kDonor,bi,bj) - dhsnw
C     end if doRemapping
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: SEAICE_ITD_REMAP_LINEAR

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ITD_REMAP_LINEAR(
     O     g0, g1,
     U     hL, hR,
     I     hActual, area,
     I     SEAICE_area_reg, SEAICE_eps, doRemapping,
     I     myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ITD_REMAP_LINEAR
C     | o compute coefficients g0, g1 for piece-wise linear fit
C     |    g(h) = g0 + g1*h
C     | o compute range boundaries hL, hR for this linear fit
C     |
C     | Martin Losch, May 2014, Martin.Losch@awi.de
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

#include "SIZE.h"

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
C
C     OUTPUT: coefficients for representing g(h)
C     g0 :: constant coefficient in g(h)
C     g1 :: linear  coefficient in g(h)
C     hL :: left end of range over which g(h) > 0
C     hL :: right end of range over which g(h) > 0
      _RL g0 (1:sNx,1:sNy)
      _RL g1 (1:sNx,1:sNy)
      _RL hL (1:sNx,1:sNy)
      _RL hR (1:sNx,1:sNy)
C     INPUT:
C     hActual :: ice thickness of current category
C     area    :: ice concentration of current category
      _RL hActual (1:sNx,1:sNy)
      _RL area    (1:sNx,1:sNy)
C     regularization constants
      _RL SEAICE_area_reg
      _RL SEAICE_eps
C     doRemapping :: mask where can be done, excludes points where
C                    new category limits are outside certain bounds
      LOGICAL doRemapping (1:sNx,1:sNy)

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j       :: inner loop counters
C
      INTEGER i, j
C     auxCoeff :: helper variable
C     recip_etaR :: reciprocal of range interval in eta space
C     etaNoR   :: ratio of distance to lower limit over etaR
      _RL auxCoeff
      _RL recip_etaR, etaNoR
      _RL third, sixth
      PARAMETER ( third = 0.333333333333333333333333333 _d 0 )
      PARAMETER ( sixth = 0.666666666666666666666666666 _d 0 )
CEOP
C
C     initialisation of hL, hR is done outside this routine
C
      DO j=1,sNy
       DO i=1,sNx
        g0(i,j) = 0. _d 0
        g1(i,j) = 0. _d 0
        IF ( doRemapping(i,j) .AND.
     &       area(i,j) .GT. SEAICE_area_reg .AND.
     &       hR(i,j) - hL(i,j) .GT. SEAICE_eps ) THEN
C     change hL and hR if hActual falls outside the central third of the range
         IF ( hActual(i,j) .LT. (2. _d 0*hL(i,j) + hR(i,j))*third ) THEN
          hR(i,j) = 3. _d 0 * hActual(i,j) - 2. _d 0 * hL(i,j)
         ELSEIF ( hActual(i,j).GT.(hL(i,j)+2. _d 0*hR(i,j))*third ) THEN
          hL(i,j) = 3. _d 0 * hActual(i,j) - 2. _d 0 * hR(i,j)
         ENDIF
C     calculate new etaR = hR - hL;
C     catch the case of hR=hL, which can happen when hActual=hR or hL
C     before entering this routine; in this case g0=g1=0.
         recip_etaR = 0. _d 0
CMU         IF ( hR(i,j) .GT. hL(i,j) ) ! crucial change; lets the model explode
         IF ( hR(i,j) - hL(i,j) .GT. SEAICE_eps )
     &        recip_etaR = 1. _d 0 / (hR(i,j) - hL(i,j))
C     some abbreviations to avoid computing the same thing multiple times
         etaNoR     = (hActual(i,j) - hL(i,j))*recip_etaR
         auxCoeff   = 6. _d 0 * area(i,j)*recip_etaR
C     equations (14) of Lipscomb (2001), JGR
         g0(i,j) = auxCoeff*( sixth - etaNoR )
         g1(i,j) = 2. _d 0 * auxCoeff*recip_etaR*( etaNoR - 0.5 _d 0 )
        ELSE
C     not doRemapping
C     reset hL and hR
         hL(i,j) = 0. _d 0
         hR(i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: SEAICE_ITD_REMAP_CHECK_BOUNDS

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ITD_REMAP_CHECK_BOUNDS(
     I     AREAITD, hActual, hActualPre, hLimitNew, doRemapping,
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ITD_REMAP_CHECK_BOUNDS
C     | o where doRemapping = .FALSE. print a warning
C     |
C     | Martin Losch, May 2014, Martin.Losch@awi.de
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     bi, bj    :: outer loop counters
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER bi,bj
      INTEGER myIter
      INTEGER myThid
C     hActual :: ice thickness of current category
      _RL hActual   (1:sNx,1:sNy,1:nITD)
      _RL hActualPre(1:sNx,1:sNy,1:nITD)
C     hLimitNew :: new "advected" category boundaries after seaice_growth
      _RL hLimitNew (1:sNx,1:sNy,0:nITD)
C     AREAITD :: ice concentration of current category
      _RL AREAITD   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
C     doRemapping :: mask where can be done, excludes points where
C                    new category limits are outside certain bounds
      LOGICAL doRemapping (1:sNx,1:sNy)

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k     :: inner loop counters
C
      INTEGER i, j, k
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(39) tmpBuf
CEOP

       DO j=1,sNy
        DO i=1,sNx
         IF (.NOT.doRemapping(i,j) ) THEN
          DO k=1,nITD-1
           WRITE(tmpBuf,'(A,2I5,A,I10)')
     &          ' at (', i, j, ') in timestep ', myIter
           IF ( AREAITD(i,j,k,bi,bj).GT.SEAICE_area_reg .AND.
     &          hActual(i,j,k) .GE. hLimitNew(i,j,k) ) THEN
            WRITE(msgBuf,'(A,I3,A)')
     &           'SEAICE_ITD_REMAP: hActual(k) >= hLimitNew(k) '//
     &           'for category ', k, tmpBuf
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
CML            PRINT *, hActual(i,j,k),
CML     &           hLimitNew(i,j,k), hLimit(k)
           ENDIF
           IF ( AREAITD(i,j,k+1,bi,bj).GT.SEAICE_area_reg .AND.
     &          hActual(i,j,k+1) .LE. hLimitNew(i,j,k) ) THEN
            WRITE(msgBuf,'(A,I3,A)')
     &           'SEAICE_ITD_REMAP: hActual(k+1) <= hLimitNew(k) '//
     &           'for category ', k, tmpBuf
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
            PRINT '(8(1X,E10.4))',
     &           AREAITD(i,j,k+1,bi,bj), hActual(i,j,k+1),
     &           hActualPre(i,j,k+1),
     &           AREAITD(i,j,k,bi,bj), hActual(i,j,k),
     &           hActualPre(i,j,k),
     &           hLimitNew(i,j,k), hLimit(k)
           ENDIF
           IF ( hLimitNew(i,j,k) .GT. hLimit(k+1) ) THEN
            WRITE(msgBuf,'(A,I3,A)')
     &           'SEAICE_ITD_REMAP: hLimitNew(k) > hLimitNew(k+1) '//
     &           'for category ', k, tmpBuf
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
           ENDIF
           IF ( hLimitNew(i,j,k) .LT. hLimit(k-1) ) THEN
            WRITE(msgBuf,'(A,I3,A)')
     &           'SEAICE_ITD_REMAP: hLimitNew(k) < hLimitNew(k-1) '//
     &           'for category ', k, tmpBuf
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
           ENDIF
          ENDDO
         ENDIF
        ENDDO
       ENDDO

#endif /* SEAICE_ITD */

      RETURN
      END
