#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE MONTH_END_DIAGS(  monid, myTime, myIter, myThid )
C     *==========================================================*
C     | Prints out rolling month-end quantities, and sums these  |
c     | into the tave monthly diagnostic variables.              |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     monid  - current month of the year
C     myTime - current simulation time (ocean model time)
C     myIter - iteration number (ocean model)
C     myThid - Thread no. that called this routine.
      INTEGER monid
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) suff
      INTEGER ndmonth(12)
      DATA ndmonth/31,28,31,30,31,30,31,31,30,31,30,31/
      INTEGER i,j
      _RS norm_factor

      norm_factor = ndmonth(monid)*86400.0

      DO j=1,sNy
        DO i=1,sNx

          qnet_atm_ta(i,j,monid)= qnet_atm_ta(i,j,monid) + 
     &                            qnet_atm(i,j)
          evap_atm_ta(i,j,monid)= evap_atm_ta(i,j,monid) + 
     &                            evap_atm(i,j)
          precip_atm_ta(i,j,monid)= precip_atm_ta(i,j,monid) + 
     &                              precip_atm(i,j)
          runoff_atm_ta(i,j,monid)=  runoff_atm_ta(i,j,monid) +  
     &                               runoff_atm(i,j)
          sum_qrel_ta(i,j,monid)= sum_qrel_ta(i,j,monid) + 
     &                            sum_qrel(i,j)
          sum_frel_ta(i,j,monid)= sum_frel_ta(i,j,monid) + 
     &                            sum_frel(i,j)
          sum_iceMask_ta(i,j,monid)= sum_iceMask_ta(i,j,monid) + 
     &                               sum_iceMask(i,j)
          sum_iceHeight_ta(i,j,monid)= sum_iceHeight_ta(i,j,monid) + 
     &                                 sum_iceHeight(i,j)
          sum_iceTime_ta(i,j,monid)= sum_iceTime_ta(i,j,monid) + 
     &                               sum_iceTime(i,j)
          sum_oceMxLT_ta(i,j,monid)= sum_oceMxLT_ta(i,j,monid) + 
     &                               sum_oceMxLT(i,j)
          sum_oceMxLS_ta(i,j,monid)= sum_oceMxLS_ta(i,j,monid) + 
     &                               sum_oceMxLS(i,j)

          qnet_atm(i,j)=qnet_atm(i,j)/ norm_factor
          evap_atm(i,j)=evap_atm(i,j)/ norm_factor
          precip_atm(i,j)= precip_atm(i,j)/ norm_factor
          runoff_atm(i,j)=  runoff_atm(i,j)/ norm_factor
          sum_qrel(i,j)= sum_qrel(i,j)/ norm_factor
          sum_frel(i,j)= sum_frel(i,j)/ norm_factor
          sum_iceMask(i,j)= sum_iceMask(i,j)/ norm_factor
          sum_iceHeight(i,j)= sum_iceHeight(i,j)/ norm_factor
          sum_iceTime(i,j)= sum_iceTime(i,j)/ norm_factor
          sum_oceMxLT(i,j)= sum_oceMxLT(i,j)/ norm_factor
          sum_oceMxLS(i,j)= sum_oceMxLS(i,j)/ norm_factor

        ENDDO
      ENDDO

      WRITE(suff,'(I2.2)') monid
      CALL WRITE_FLD_XY_RL('mQnetAtmtave.', suff, qnet_atm,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mEvapAtmtave.', suff, evap_atm,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mPrecipAtmtave.', suff, precip_atm,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mRunoffAtmtave.', suff, runoff_atm,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mQrelfluxtave.', suff, sum_qrel,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mFrelfluxtave.', suff, sum_frel,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mIceMasktave.', suff, sum_iceMask,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mIceHeighttave.', suff, sum_iceHeight,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mIceTimetave.', suff, sum_iceTime,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mOceMxLTtave.', suff, sum_oceMxLT,
     &                     myIter, myThid)
      CALL WRITE_FLD_XY_RL('mOceMxLStave.', suff, sum_oceMxLS,
     &                     myIter, myThid)

      DO j=1,sNy
        DO i=1,sNx

          qnet_atm(i,j)= 0. _d 0
          evap_atm(i,j)= 0. _d 0
          precip_atm(i,j)= 0. _d 0
          runoff_atm(i,j)= 0. _d 0 
          sum_qrel(i,j)= 0. _d 0
          sum_frel(i,j)= 0. _d 0
          sum_iceMask(i,j)= 0. _d 0
          sum_iceHeight(i,j)= 0. _d 0
          sum_iceTime(i,j)= 0. _d 0
          sum_oceMxLT(i,j)= 0. _d 0
          sum_oceMxLS(i,j)= 0. _d 0

        ENDDO
      ENDDO

      RETURN
      END

