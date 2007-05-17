#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE GET_OCNVARS( myTime, myIter, myThid )
C     *==========================================================*
C     | Gets the ocean data from either the ocean common block,  |
C     | or, in MPI mode, from the passed common block. This data |
C     | is used by the atmosphere and sea ice models.            |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "THSICE_VARS.h"
#include "ATM2D_VARS.h"

#ifdef ATM2D_MPI_ON
#  include "OCNSIZE.h"
#  include "OCNVARS.h"
#  include "OCNIDS.h"
#else
#  include "PARAMS.h"
#  include "GRID.h"
#  include "SURFACE.h"
#  include "DYNVARS.h"
#  ifdef ALLOW_DIC
#    include "DIC_ABIOTIC.h"
#  endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime - current simulation time (ocean model time)
C     myIter - iteration number (ocean model)
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER i,j


#ifdef ATM2D_MPI_ON
C presuming ocnsize.h same as size.h
      DO j=1,sNy
        DO i=1,sNx
          sstFromOcn(i,j)= SST_ocn(i,j) 
          sssFromOcn(i,j)= SSS_ocn(i,j) 
          mlDepth(i,j) = OcMxlD_ocn(i,j)
          IF ( ocnCpl_exchange_DIC ) oFluxCO2(i,j) = fluxCO2_ocn(i,j)
C          IF ( ocnCpl_exchange_DIC ) 
C     &       print *,'ofluxCO2:',i,j,fluxCO2_ocn(i,j)

C Put in seaice common block
          tOceMxl(i,j,1,1)= SST_ocn(i,j) 
          sOceMxl(i,j,1,1)= SSS_ocn(i,j) 
          hOceMxl(i,j,1,1) = OcMxlD_ocn(i,j)
          v2ocMxL(i,j,1,1) =  vSq_ocn(i,j)
        ENDDO
      ENDDO

#else
       DO j=1,sNy
        DO i=1,sNx
          sstFromOcn(i,j) = theta(i,j,1,1,1)
          sssFromOcn(i,j) = salt (i,j,1,1,1)
        ENDDO
       ENDDO

#  ifdef NONLIN_FRSURF
Cjrs ask J-M about next lines
      IF ( nonlinFreeSurf.GT.0 .AND.
     &    (staggerTimeStep .OR. .NOT.cpl_earlyExpImpCall ) ) THEN 
       IF ( select_rStar.GT.0 ) THEN
        DO j=1,sNy
         DO i=1,sNx
           mlDepth(i,j) =
     &          h0FacC(i,j,1,1,1)*rStarFacC(i,j,1,1)*drF(1)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
           mlDepth(i,j) = hFac_surfC(i,j,1,1)*drF(1)
         ENDDO
        ENDDO
       ENDIF
      ELSE
#  else /* NONLIN_FRSURF */
      IF (.TRUE.) THEN
#  endif /* NONLIN_FRSURF */
       DO j=1,sNy
        DO i=1,sNx
           mlDepth(i,j) = hFacC(i,j,1,1,1)*drF(1)
        ENDDO
       ENDDO
      ENDIF

      DO j=1,sNy
        DO i=1,sNx
          v2ocMxL(i,j,1,1) = 
     &              ( uVel(i , j,1,1,1)*uVel(i , j,1,1,1)
     &              + uVel(i+1,j,1,1,1)*uVel(i+1,j,1,1,1)
     &              + vVel(i,j+1,1,1,1)*vVel(i,j+1,1,1,1)
     &              + vVel(i,j , 1,1,1)*vVel(i,j , 1,1,1)
     &              )*0.5 _d 0
        ENDDO
      ENDDO

      DO j=1,sNy
        DO i=1,sNx

#  ifdef ALLOW_DIC
          oFluxCO2(i,j) = fluxCO2(i,j,1,1)
#  endif

C Put in seaice common block
          tOceMxl(i,j,1,1)= sstFromOcn(i,j) 
          sOceMxl(i,j,1,1)= sssFromOcn(i,j) 
          hOceMxl(i,j,1,1) = mlDepth(i,j)
        ENDDO
      ENDDO
         
#endif

      RETURN
      END

