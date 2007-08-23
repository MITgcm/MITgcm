#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE YR_END_DIAGS(  iyr, myTime, myIter, myThid )
C     *==========================================================*
C     | Normalize and dump year-end annual mean diagnostics      |
c     |                                                          |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "ATM2D_VARS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iyr    - current year of simulation
C     myTime - current simulation time (ocean model time)
C     myIter - iteration number (ocean model)
C     myThid - Thread no. that called this routine.
      INTEGER iyr
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     LOCAL VARIABLES:
      _RL secYr
      DATA secYr /31536000.D0/   
      _RL norm_factor

      norm_factor = globalArea * secYr / dtcouplo
      sst_tave = sst_tave / norm_factor
      sss_tave = sss_tave / norm_factor
      HF2ocn_tave = HF2ocn_tave / norm_factor
      FW2ocn_tave = FW2ocn_tave / norm_factor
      CO2flx_tave = CO2flx_tave / norm_factor

      OPEN(25,ACCESS='APPEND',FILE='resocean.dat',STATUS='old')

      WRITE(25,*) '      '
      WRITE(25,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      WRITE(25,*) 'YEAR MEAN', iyr
      WRITE(25,*) 'SST SSS', sst_tave, sss_tave
      WRITE(25,*) 'HFLUX FWFLUX',  HF2ocn_tave,  FW2ocn_tave
      WRITE(25,*) 'SH ICE', SHice_max*1. _d -12, SHice_min*1. _d -12
      WRITE(25,*) 'NH ICE', NHice_max*1. _d -12, NHice_min*1. _d -12
      IF  (CO2flx_tave .NE. 0. _d 0) 
     &   WRITE(25,*) 'OCEAN CO2FLUX', CO2flx_tave
      WRITE(25,*) '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      WRITE(25,*) '      '

      CLOSE(25)

      SHice_min = 1. _d 18
      NHice_min = 1. _d 18
      SHice_max = 0. _d 0
      NHice_max = 0. _d 0
      sst_tave =  0. _d 0
      sss_tave =  0. _d 0
      HF2ocn_tave =  0. _d 0
      FW2ocn_tave =  0. _d 0
      CO2flx_tave =  0. _d 0

      RETURN
      END

