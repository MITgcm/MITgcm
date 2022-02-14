#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_BOTTOMDRAG_COEFFS
C     !INTERFACE:
      SUBROUTINE SEAICE_BOTTOMDRAG_COEFFS(
     I     uIceLoc, vIceLoc, HEFFMLoc,
#ifdef SEAICE_ITD
     I     HEFFITDLoc, AREAITDLoc, AREALoc,
#else
     I     HEFFLoc, AREALoc,
#endif
     O     CbotLoc,
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
C     u/vIceLoc :: local copies of the current ice velocity
C     HEFFMLoc  :: local copy of land-sea masks
C     CbotLoc   :: drag coefficients
C     iStep     :: current sub-time step iterate
C     myTime    :: Simulation time
C     myIter    :: Simulation timestep number
C     myThid    :: my Thread Id. number
      _RL uIceLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL HEFFMLoc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef SEAICE_ITD
      _RL HEFFITDLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
      _RL AREAITDLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nITD,nSx,nSy)
#else
      _RL HEFFLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
      _RL AREALoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL CbotLoc   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iStep
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

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

      IF (SEAICEbasalDragK2.GT.0. _d 0) THEN
C     avoid this computation for a non-zero coefficient
      IF ( usingPCoords ) THEN
       kSrf = Nr
      ELSE
       kSrf = 1
      ENDIF
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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          CbotLoc(i,j,bi,bj) = 0. _d 0
          tmpFld (i,j)       = 0. _d 0
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          IF ( AREALoc(i,j,bi,bj) .GT. 0.01 _d 0 ) THEN
#ifdef OBCS_UVICE_OLD
           tmp = 0.25 _d 0*(
     &          (   uIceLoc(i  ,j,bi,bj)+uIceLoc(i+1,j,bi,bj)
     &          )**2
     &          + ( vIceLoc(i, j ,bi,bj)+vIceLoc(i,j+1,bi,bj)
     &          )**2 )
#else /* OBCS_UVICE_OLD */
           tmp = 0.25 _d 0*(
     &          ( uIceLoc(i  ,j,bi,bj)*maskInW( i ,j,bi,bj)
     &          + uIceLoc(i+1,j,bi,bj)*maskInW(i+1,j,bi,bj) )**2
     &        + ( vIceLoc(i,j  ,bi,bj)*maskInS(i,j  ,bi,bj)
     &          + vIceLoc(i,j+1,bi,bj)*maskInS(i,j+1,bi,bj) )**2 )
#endif /* OBCS_UVICE_OLD */
           tmpFld(i,j) = SEAICEbasalDragK2 / SQRT(tmp + u0sq)
          ENDIF
         ENDDO
        ENDDO
#ifdef SEAICE_ITD
        DO k=1,nITD
#endif /* SEAICE_ITD */
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx-1
           IF ( AREALoc(i,j,bi,bj) .GT. 0.01 _d 0 ) THEN
CML           hActual = HEFFLoc(i,j,bi,bj)
CML     &          /SQRT( AREAITDLoc(i,j,bi,bj)**2 + area_reg_sq )
CML           hActual = SQRT(hActual * hActual + hice_reg_sq)
CML           hCrit   = ABS(R_low(i,j,bi,bj)) * recip_k1
#ifdef SEAICE_ITD
            hActual = HEFFITDLoc(i,j,k,bi,bj)
C     here we do not need recip_k1, because we resolve the very thick ice
            hCrit   = ABS(R_low(i,j,bi,bj))*AREAITDLoc(i,j,k,bi,bj)
#else
            hActual = HEFFLoc(i,j,bi,bj)
            hCrit   = ABS(R_low(i,j,bi,bj))*AREALoc(i,j,bi,bj)*recip_k1
#endif /* SEAICE_ITD */
C     we want to have some soft maximum for better differentiability:
C     max(a,b;k) = ln(exp(k*a)+exp(k*b))/k
C     In our case, b=0, so exp(k*b) = 1.
C     max(a,0;k) = ln(exp(k*a)+1)/k
C     If k*a gets too large, EXP will overflow, but for the anticipated
C     values of hActual < 100m, and k=10, this should be very unlikely
CML             CbotLoc(i,j,bi,bj) =
CML     &            tmpFld(i,j) * MAX( hActual - hCrit, 0. _d 0)
            CbotLoc(i,j,bi,bj) = CbotLoc(i,j,bi,bj)
     &           + tmpFld(i,j)
     &           * LOG(EXP( fac*(hActual - hCrit) ) + 1. _d 0)*rFac
     &           * EXP( - SEAICE_cBasalStar
     &                  *(SEAICE_area_max - AREALoc(i,j,bi,bj)) )
     &           * HEFFMLoc(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
#ifdef SEAICE_ITD
        ENDDO
#endif /* SEAICE_ITD */
       ENDDO
      ENDDO
C     endif SEAICEbasalDragK2.GT.0.
      ENDIF
#endif /* SEAICE_ALLOW_BOTTOMDRAG */

      RETURN
      END
