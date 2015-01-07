C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_ice_strength.F,v 1.9 2015/01/07 13:27:10 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: SEAICE_CALC_ICE_STRENGTH
C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_CALC_ICE_STRENGTH(
     I     bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_CALC_ICE_STRENGTH
C     | o compute ice strengh PRESS0
C     |   according to
C     |   (a) Hibler (1979)
C     |   (b) Thorndyke et al (1975) and Hibler (1980)
C     |   (c) Bitz et al (2001) and Lipscomb et al (2007)
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

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     bi, bj    :: outer loop counters
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      INTEGER bi,bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k       :: inner loop counters
C     i/jMin/Max  :: loop boundaries
C
      INTEGER i, j
      INTEGER iMin, iMax, jMin, jMax
      _RL tmpscal1, tmpscal2
#ifdef SEAICE_ITD
C     variables related to ridging schemes
C     ridgingModeNorm :: norm to ensure convervation (N in Lipscomb et al 2007)
C     partFunc   :: participation function (a_n in Lipscomb et al 2007)
C     ridgeRatio :: mean ridge thickness/ thickness of ridging ice
C     hrMin      :: min ridge thickness
C     hrMax      :: max ridge thickness   (SEAICEredistFunc = 0)
C     hrExp      :: ridge e-folding scale (SEAICEredistFunc = 1)
C     hActual    :: HEFFITD/AREAITD
      INTEGER k
      _RL ridgingModeNorm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL partFunc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,0:nITD)
      _RL hrMin           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hrMax           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hrExp           (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL ridgeRatio      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
      _RL hActual         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nITD)
#endif /* SEAICE_ITD */
CEOP

C     loop boundaries
      iMin=1-OLx
      iMax=sNx+OLx
      jMin=1-OLy
      jMax=sNy+OLy

#ifdef SEAICE_ITD
C     compute the fraction of open water as early as possible, i.e.
C     before advection, but also before it is used in calculating the ice
C     strength according to Rothrock (1975), hidden in S/R seaice_repare_ridging
      DO j=jMin,jMax
       DO i=iMin,iMax
        opnWtrFrac(i,j,bi,bj) = 1. _d 0 - AREA(i,j,bi,bj)
       ENDDO
      ENDDO

      IF ( useHibler79IceStrength ) THEN
#else
      IF ( .TRUE. ) THEN
#endif /* SEAICE_ITD */
       DO j=jMin,jMax
        DO i=iMin,iMax
C--   now set up ice pressure and viscosities
         IF ( (HEFF(i,j,bi,bj).LE.SEAICEpresH0).AND.
     &        (SEAICEpresPow0.NE.1) ) THEN
          tmpscal1=MAX(HEFF(i,j,bi,bj)/SEAICEpresH0,ZERO)
          tmpscal2=SEAICEpresH0*(tmpscal1**SEAICEpresPow0)
         ELSEIF ( (HEFF(i,j,bi,bj).GT.SEAICEpresH0).AND.
     &         (SEAICEpresPow1.NE.1) ) THEN
          tmpscal1=MAX(HEFF(i,j,bi,bj)/SEAICEpresH0,ZERO)
          tmpscal2=SEAICEpresH0*(tmpscal1**SEAICEpresPow1)
         ELSE
          tmpscal2=HEFF(i,j,bi,bj)
         ENDIF
         PRESS0(I,J,bi,bj)=SEAICE_strength*tmpscal2
     &        *EXP(-SEAICE_cStar*(SEAICE_area_max-AREA(i,j,bi,bj)))
         ZMAX(I,J,bi,bj)  = SEAICE_zetaMaxFac*PRESS0(I,J,bi,bj)
         ZMIN(I,J,bi,bj)  = SEAICE_zetaMin
         PRESS0(I,J,bi,bj)= PRESS0(I,J,bi,bj)*HEFFM(I,J,bi,bj)
        ENDDO
       ENDDO
#ifdef SEAICE_ITD
      ELSE
C     not useHiber79IceStrength
       DO j=jMin,jMax
        DO i=iMin,iMax
         PRESS0(i,j,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
       CALL SEAICE_PREPARE_RIDGING(
     O      hActual,
     O      hrMin, hrMax, hrExp, ridgeRatio, ridgingModeNorm, partFunc,
     I      iMin, iMax, jMin, jMax, bi, bj, myTime, myIter, myThid )
       IF ( SEAICEredistFunc .EQ. 0 ) THEN
        tmpscal1 = 1. _d 0 / 3. _d 0
        DO k = 1, nITD
         DO j=jMin,jMax
          DO i=iMin,iMax
C     replace (hrMax**3-hrMin**3)/(hrMax-hrMin) by identical
C     hrMax**2+hrMin**2 + hrMax*hrMin to avoid division by potentially
C     small number
           IF ( partFunc(i,j,k) .GT. 0. _d 0 )
     &          PRESS0(i,j,bi,bj) = PRESS0(i,j,bi,bj)
     &          + partFunc(i,j,k) * ( - hActual(i,j,k)**2
     &          + ( hrMax(i,j,k)**2 + hrMin(i,j,k)**2 
     &          + hrMax(i,j,k)*hrMin(i,j,k) )*tmpscal1
     &          / ridgeRatio(i,j,k) )
          ENDDO
         ENDDO
        ENDDO
       ELSEIF ( SEAICEredistFunc .EQ. 1 ) THEN
        DO k = 1, nITD
         DO j=jMin,jMax
          DO i=iMin,iMax
           PRESS0(i,j,bi,bj) = PRESS0(i,j,bi,bj)
     &          + partFunc(i,j,k) * ( - hActual(i,j,k)**2 +
     &          (           hrMin(i,j,k)*hrMin(i,j,k)
     &          + 2. _d 0 * hrMin(i,j,k)*hrExp(i,j,k)
     &          + 2. _d 0 * hrExp(i,j,k)*hrExp(i,j,k)
     &          )/ridgeRatio(i,j,k) )
          ENDDO
         ENDDO
        ENDDO
       ENDIF
C
       tmpscal1 = SEAICE_cf*0.5*gravity*(rhoConst-SEAICE_rhoIce)
     &      * SEAICE_rhoIce/rhoConst
       DO j=jMin,jMax
        DO i=iMin,iMax
         PRESS0(i,j,bi,bj) = PRESS0(i,j,bi,bj)/ridgingModeNorm(i,j)
     &        *tmpscal1
         ZMAX(I,J,bi,bj)  = SEAICE_zetaMaxFac*PRESS0(I,J,bi,bj)
         ZMIN(I,J,bi,bj)  = SEAICE_zetaMin
         PRESS0(I,J,bi,bj)= PRESS0(I,J,bi,bj)*HEFFM(I,J,bi,bj)
        ENDDO
       ENDDO
#endif /* SEAICE_ITD */
      ENDIF

      RETURN
      END
