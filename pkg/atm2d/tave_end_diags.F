#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE TAVE_END_DIAGS(  nYears, myTime, myIter, myThid )
C     *==========================================================*
C     | Calculate and dump all diagnostics at tave periods.      |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     nYears - number of years in this dump (maybe be different from
C              tave if starting time not divisible by tave)
C     myTime - current simulation time (ocean model time)
C     myIter - iteration number (ocean model)
C     myThid - Thread no. that called this routine.
      INTEGER nYears
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) suff, fn
      INTEGER ndmonth(12)
      DATA ndmonth/31,28,31,30,31,30,31,31,30,31,30,31/
      _RL secYr
      DATA secYr /31536000. _d 0/  
      INTEGER i,j,mn,j_atm
      INTEGER simYr
      INTEGER dUnit
      _RS norm_factor
      _RL qnet_ann(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL evap_ann(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL precip_ann(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL runoff_ann(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qrel_ann(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL frel_ann(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qnet_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL evap_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL precip_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL runoff_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL qrel_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL frel_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceMask_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceHeight_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceTime_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oceMxLT_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oceMxLS_mon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)


      DO j=1,sNy
        DO i=1,sNx

          qnet_ann(i,j) = 0. _d 0
          evap_ann(i,j) = 0. _d 0
          precip_ann(i,j) = 0. _d 0
          runoff_ann(i,j) = 0. _d 0
          qrel_ann(i,j) = 0. _d 0
          frel_ann(i,j) = 0. _d 0

        ENDDO
      ENDDO

      DO mn=1,nForcingPer

        norm_factor=nYears*ndmonth(mn)*86400.0
        DO j=1,sNy
          DO i=1,sNx

            qnet_mon(i,j)= qnet_atm_ta(i,j,mn)/norm_factor
            evap_mon(i,j)= evap_atm_ta(i,j,mn)/norm_factor
            precip_mon(i,j)= precip_atm_ta(i,j,mn)/norm_factor
            runoff_mon(i,j)=  runoff_atm_ta(i,j,mn)/norm_factor
            qrel_mon(i,j)= sum_qrel_ta(i,j,mn)/norm_factor
            frel_mon(i,j)= sum_frel_ta(i,j,mn)/norm_factor
            iceMask_mon(i,j)= sum_iceMask_ta(i,j,mn)/norm_factor
            iceHeight_mon(i,j)= sum_iceHeight_ta(i,j,mn)/norm_factor
            iceTime_mon(i,j)= sum_iceTime_ta(i,j,mn)/norm_factor
            oceMxLT_mon(i,j)= sum_oceMxLT_ta(i,j,mn)/norm_factor
            oceMxLS_mon(i,j)= sum_oceMxLS_ta(i,j,mn)/norm_factor

            qnet_ann(i,j) = qnet_ann(i,j) + 
     &                      qnet_mon(i,j)*ndmonth(mn)/365.0
            evap_ann(i,j) = evap_ann(i,j) + 
     &                      evap_mon(i,j)*ndmonth(mn)/365.0
            precip_ann(i,j) = precip_ann(i,j) + 
     &                        precip_mon(i,j)*ndmonth(mn)/365.0
            runoff_ann(i,j) = runoff_ann(i,j) + 
     &                        runoff_mon(i,j)*ndmonth(mn)/365.0
            qrel_ann(i,j) = qrel_ann(i,j) + 
     &                      qrel_mon(i,j)*ndmonth(mn)/365.0
            frel_ann(i,j) = frel_ann(i,j) + 
     &                      frel_mon(i,j)*ndmonth(mn)/365.0

            qnet_atm_ta(i,j,mn)= 0. _d 0
            evap_atm_ta(i,j,mn)= 0. _d 0
            precip_atm_ta(i,j,mn)= 0. _d 0
            runoff_atm_ta(i,j,mn)= 0. _d 0 
            sum_qrel_ta(i,j,mn)= 0. _d 0
            sum_frel_ta(i,j,mn)= 0. _d 0
            sum_iceMask_ta(i,j,mn)= 0. _d 0
            sum_iceHeight_ta(i,j,mn)= 0. _d 0
            sum_iceTime_ta(i,j,mn)= 0. _d 0
            sum_oceMxLT_ta(i,j,mn)= 0. _d 0
            sum_oceMxLS_ta(i,j,mn)= 0. _d 0

          ENDDO
        ENDDO

        DO j_atm=1,jm0
          sum_tauu_ta(j_atm,mn) = sum_tauu_ta(j_atm,mn) / norm_factor
          sum_tauv_ta(j_atm,mn) = sum_tauv_ta(j_atm,mn) / norm_factor
          sum_wsocean_ta(j_atm,mn) = sum_wsocean_ta(j_atm,mn) /
     &                               norm_factor
          sum_ps4ocean_ta(j_atm,mn) = sum_ps4ocean_ta(j_atm,mn) / 
     &                                norm_factor
        ENDDO

        WRITE(suff,'(I2.2)') mn
        CALL WRITE_FLD_XY_RL('amQnetAtmtave.', suff, 
     &                       qnet_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amEvapAtmtave.', suff, 
     &                       evap_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amPrecipAtmtave.', suff, 
     &                       precip_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amRunoffAtmtave.', suff, 
     &                       runoff_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amQrelfluxtave.', suff, 
     &                       qrel_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amFrelfluxtave.', suff, 
     &                       frel_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amIceMasktave.', suff, 
     &                       iceMask_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amIceHeighttave.', suff, 
     &                       iceHeight_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amIceTimetave.', suff, 
     &                       iceTime_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amOceMxLTtave.', suff, 
     &                       oceMxLT_mon, myIter, myThid)
        CALL WRITE_FLD_XY_RL('amOceMxLStave.', suff, 
     &                       oceMxLS_mon, myIter, myThid)

      ENDDO

      WRITE(suff,'(I10.10)') myIter
      CALL WRITE_FLD_XY_RL('QnetAtmtave.',  suff,
     &                     qnet_ann,  myIter, myThid)
      CALL WRITE_FLD_XY_RL('EvapAtmtave.',  suff,
     &                     evap_ann,  myIter, myThid)
      CALL WRITE_FLD_XY_RL('PrecipAtmtave.',  suff,
     &                     precip_ann,  myIter, myThid)
      CALL WRITE_FLD_XY_RL('RunoffAtmtave.',  suff,
     &                     runoff_ann,  myIter, myThid)
      CALL WRITE_FLD_XY_RL('Qrelfluxtave.',  suff,
     &                     qrel_ann,  myIter, myThid)
      CALL WRITE_FLD_XY_RL('Frelfluxtave.',  suff,
     &                     frel_ann,  myIter, myThid)


      simYr = int(myIter*deltaTClock/secYr)
      CALL MDSFINDUNIT( dUnit, mythid )
      
      WRITE(fn,'(A,I5.5)') 'attauu.', simYr
      OPEN(dUnit, FILE=fn, STATUS='unknown',
     &     ACCESS='direct', RECL=8*jm0*nForcingPer, FORM='unformatted')
      WRITE(dUnit,REC=1) sum_tauu_ta
      CLOSE(dUnit)

      WRITE(fn,'(A,I5.5)') 'attauv.', simYr
      OPEN(dUnit, FILE=fn, STATUS='unknown',
     &     ACCESS='direct', RECL=8*jm0*nForcingPer, FORM='unformatted')
      WRITE(dUnit,REC=1) sum_tauv_ta
      CLOSE(dUnit)

      WRITE(fn,'(A,I5.5)') 'atwind.', simYr
      OPEN(dUnit, FILE=fn, STATUS='unknown',
     &     ACCESS='direct', RECL=8*jm0*nForcingPer, FORM='unformatted')
      WRITE(dUnit,REC=1) sum_wsocean_ta
      CLOSE(dUnit)

      WRITE(fn,'(A,I5.5)') 'atps4ocn.', simYr
      OPEN(dUnit, FILE=fn, STATUS='unknown',
     &     ACCESS='direct', RECL=8*jm0*nForcingPer, FORM='unformatted')
      WRITE(dUnit,REC=1) sum_ps4ocean_ta
      CLOSE(dUnit)

      DO mn=1,nForcingPer
        DO j_atm=1,jm0
          sum_tauu_ta(j_atm,mn) = 0. _d 0
          sum_tauv_ta(j_atm,mn) = 0. _d 0
          sum_wsocean_ta(j_atm,mn) = 0. _d 0
          sum_ps4ocean_ta(j_atm,mn) = 0. _d 0
        ENDDO
      ENDDO

      RETURN
      END

