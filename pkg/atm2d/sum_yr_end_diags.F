#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE SUM_YR_END_DIAGS(  myTime, myIter, myThid )
C     *==========================================================*
C     | Tabulate year-end annual mean diagnostics                |
c     | Called at top of coupled period, after seaice growth/melt|
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "THSICE_VARS.h"
#include "ATM2D_VARS.h"


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
      _RL iceArea

      iceArea = 0. _d 0
      DO j=1,int(sNy/2)
        DO i=1,sNx
          IF (iceMask(i,j,1,1).GT. 0. _d 0) 
     &        iceArea = iceArea + rA(i,j,1,1)*iceMask(i,j,1,1)
        ENDDO
      ENDDO
      SHice_min=min(SHice_min,iceArea)  
      SHice_max=max(SHice_max,iceArea)  

      iceArea = 0. _d 0
      DO j=int(sNy/2),sNy
        DO i=1,sNx
          IF (iceMask(i,j,1,1).GT. 0. _d 0) 
     &        iceArea = iceArea + rA(i,j,1,1)*iceMask(i,j,1,1)
        ENDDO
      ENDDO
      NHice_min=min(NHice_min,iceArea)  
      NHice_max=max(NHice_max,iceArea)  

      DO j=1,sNy
        DO i=1,sNx

          IF (maskC(i,j,1,1,1) .EQ. 1.) THEN
            sst_tave= sst_tave + rA(i,j,1,1)*sstFromOcn(i,j)
            sss_tave= sss_tave + rA(i,j,1,1)*sssFromOcn(i,j)
            HF2ocn_tave = HF2ocn_tave + rA(i,j,1,1)*pass_qnet(i,j)
            FW2ocn_tave = FW2ocn_tave + rA(i,j,1,1)*
     &       (pass_precip(i,j) + pass_evap(i,j) + pass_runoff(i,j))

            CO2flx_tave = CO2flx_tave + rA(i,j,1,1)*oFluxCO2(i,j)
          ENDIF

        ENDDO
      ENDDO

      RETURN
      END

