C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_bottomdrag_coeffs.F,v 1.1 2016/04/22 08:43:41 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_BOTTOMDRAG_COEFFS
C     !INTERFACE:
      SUBROUTINE SEAICE_BOTTOMDRAG_COEFFS(
     I     uIce, vIce, 
#ifdef SEAICE_ITD
     I     HEFFITD, AREAITD, AREA,
#else
     I     HEFF, AREA,
#endif      
     O     CbotC,
     I     iStep, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_BOTTOMDRAG_COEFFS
C     | o Compute the non-linear drag coefficients for ice-bottom 
C     |   drag, as a parameterization for grounding fastice
C     |   following 
C     |   Lemieux et al. (2015), doi:10.1002/2014JC010678
C     *==========================================================*
C     | written by Martin Losch, Apr 2016
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
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     iStep  :: current sub-time step iterate 
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER iStep
C     u/vIce :: local copies of the current ice velocity
      _RL uIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SEAICE_ITD
      _RL HEFFITD(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
      _RL AREAITD(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
#else
      _RL HEFF(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif      
      _RL AREA(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     CbotC     :: drag coefficients
      _RL CbotC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef SEAICE_ALLOW_BOTTOMDRAG
C     === local variables ===
C     i,j,bi,bj,ksrf :: loop indices
      INTEGER i,j,bi,bj
      INTEGER kSrf
#ifdef SEAICE_ITD
      INTEGER k
#endif /* SEAICE_ITD */
      _RL     tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmp, hActual, hCrit, recip_k1, u0sq, fac, rFac
CEOP

      kSrf=1
C     some abbreviations
      u0sq     = SEAICEbasalDragU0*SEAICEbasalDragU0
      recip_k1 = 0. _d 0
      IF ( SEAICEbasalDragK1 .GT. 0. _d 0 ) 
     &     recip_k1 = 1. _d 0/SEAICEbasalDragK1
C     fac scales the soft maximum for more accuracy
      fac = 10. _d 0
      rFac = 1./fac

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+Oly
         DO i=1-OLx,sNx+OLy
          CbotC(I,J,bi,bj) = 0. _d 0
          tmpFld(I,J)      = 0. _d 0
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+Oly-1
         DO i=1-OLx,sNx+OLy-1
          IF ( AREA(I,J,bi,bj) .GT. 0.01 _d 0 ) THEN
#ifdef OBCS_UVICE_OLD
           tmp = 0.25 _d 0*(
     &          (   uIce(i  ,j,bi,bj)+uIce(i+1,j,bi,bj)
     &          )**2
     &          + ( vIce(i, j ,bi,bj)+vIce(i,j+1,bi,bj)
     &          )**2 )
#else /* OBCS_UVICE_OLD */
           tmp = 0.25 _d 0*(
     &          ( uIce(i  ,j,bi,bj)*maskInW( i ,j,bi,bj)
     &          + uIce(i+1,J,bi,bj)*maskInW(i+1,j,bi,bj) )**2
     &        + ( vIce(i,j  ,bi,bj)*maskInS(i, j ,bi,bj)
     &          + vIce(i,j+1,bi,bj)*maskInS(i,j+1,bi,bj) )**2 )
#endif /* OBCS_UVICE_OLD */
           tmpFld(I,J) = SEAICEbasalDragK2 / SQRT(tmp + u0sq)
          ENDIF
         ENDDO
        ENDDO
#ifdef SEAICE_ITD
        DO k=1,nITD
#endif /* SEAICE_ITD */
         DO j=1-OLy,sNy+Oly-1
          DO i=1-OLx,sNx+OLy-1
           IF ( AREA(I,J,bi,bj) .GT. 0.01 _d 0 ) THEN
CML           hActual = HEFF(i,j,bi,bj)
CML     &          /SQRT( AREAITD(i,j,bi,bj)**2 + area_reg_sq )
CML           hActual = SQRT(hActual * hActual + hice_reg_sq)
CML           hCrit   = ABS(R_low(I,J,bi,bj)) * recip_k1
#ifdef SEAICE_ITD
            hActual = HEFFITD(I,J,k,bi,bj)
C     here we do not need recip_k1, because we resolve the very thick ice
            hCrit   = ABS(R_low(I,J,bi,bj))*AREAITD(I,J,k,bi,bj)
#else
            hActual = HEFF(I,J,bi,bj)
            hCrit   = ABS(R_low(I,J,bi,bj))*AREA(I,J,bi,bj)*recip_k1
#endif /* SEAICE_ITD */
C     we want to have some soft maximum for better differentiability:
C     max(a,b;k) = ln(exp(k*a)+exp(k*b))/k
C     In our case, b=0, so exp(k*b) = 1.
C     max(a,0;k) = ln(exp(k*a)+1)/k
C     If k*a gets too large, EXP will overflow, but for the anticipated
C     values of hActual < 100m, and k=10, this should be very unlikely
CML             CbotC(I,J,bi,bj) = 
CML     &            tmpFld(I,J) * MAX( hActual - hCrit, 0. _d 0) 
            CbotC(I,J,bi,bj) = CbotC(I,J,bi,bj) 
     &           + tmpFld(I,J) 
     &           * LOG(EXP( fac*(hActual - hCrit) ) + 1. _d 0)*rFac
     &           * EXP( - SEAICE_cBasalStar
     &                  *(SEAICE_area_max - AREA(I,J,bi,bj)) )
     &           * maskC(I,J,kSrf,bi,bj)
           ENDIF
          ENDDO
         ENDDO
#ifdef SEAICE_ITD
        ENDDO
#endif /* SEAICE_ITD */
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_BOTTOMDRAG */

      RETURN
      END
