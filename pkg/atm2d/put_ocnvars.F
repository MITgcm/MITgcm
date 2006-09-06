#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE PUT_OCNVARS(  myTime, myIter, myThid )
C     *==========================================================*
C     | Put fluxes for the ocean where needed: MPI mode, into    |
c     | common block for passing; single-proc, into FFIELDS.h    |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

#ifdef ATM2D_MPI_ON
#  include "OCNSIZE.h"
#  include "OCNVARS.h"
#else
#  include "PARAMS.h"
#  include "FFIELDS.h"
#  include "SURFACE.h"
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
      DO j=1,sNy
        DO i=1,sNx

C OCNVARS common set from ATM2D common
          atmSLPr_ocn(i,j)= pass_slp(i,j) 
          HeatFlux_ocn(i,j)= pass_qnet(i,j) 
          qShortWave_ocn(i,j)= pass_solarnet(i,j) 
          TauX_ocn(i,j)= pass_fu(i,j) 
          TauY_ocn(i,j)= pass_fv(i,j) 
          FWFlux_ocn(i,j)= pass_precip(i,j) + pass_evap(i,j) 
     &                     + pass_runoff(i,j)
          SaltFlx_ocn(i,j) = sFluxFromIce(i,j)
C need to pass seaice mass?
C need to pass wspeed and pCO2

        ENDDO
      ENDDO
#else
C FFIELDS.h common block set from ATM2D common
      DO j=1,sNy
        DO i=1,sNx

C          pLoad(i,j,1,1)= pass_slp(i,j) 
C          _EXCH_XY_R4(pLoad, myThid )
          Qnet(i,j,1,1)= pass_qnet(i,j) 
          Qsw(i,j,1,1)= pass_solarnet(i,j) 
          fu(i,j,1,1)= pass_fu(i,j) 
          fv(i,j,1,1)= pass_fv(i,j) 
          EmPmR(i,j,1,1)= pass_precip(i,j) + pass_evap(i,j) 
     &                     + pass_runoff(i,j)
          saltFlux(i,j,1,1)= sFluxFromIce(i,j)

          CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
          _EXCH_XY_R4(EmPmR, myThid )   !for NL Free surf?

#  ifdef ATMOSPHERIC_LOADING
Cjrs ask J-M about this
C              phi0surf(i,j,1,1) = pass_slp(i,j)*recip_rhoConst
C     &                 + gravity*seaIceMass(i,j,1,1)*recip_rhoConst
#  endif /* ATMOSPHERIC_LOADING */

C need to pass seaice mass? seaIceLoad(i,j,1,1)=
C need to pass wspeed and pCO2

        ENDDO
      ENDDO

#endif

C      PRINT *,'***After seaice thicken/extend'
C      PRINT *,'pass_runoff:',pass_runoff(JBUGI,JBUGJ)
C      PRINT *,'pass_precip:',pass_precip(JBUGI,JBUGJ)
C      PRINT *,'pass_evap:', pass_evap(JBUGI,JBUGJ)
C      PRINT *,'pass_qnet:',pass_qnet(JBUGI,JBUGJ)
C      PRINT *,'pass_fu:',pass_fu(JBUGI,JBUGJ)
C      PRINT *,'sFluxFromIce:',sFluxFromIce(JBUGI,JBUGJ)
C      PRINT *,'slp:',pass_slp(JBUGI,JBUGJ)

      RETURN
      END

