#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE SUM_OCN_FLUXES( myThid )
C     *==========================================================*
C     | Sums the atmos-> ocn fluxes. Note the stress reduction   |
c     | (wind forcing) which occurs as icefract > windice_thres  |
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"

C     === Global SeaIce Variables ===
#include "THSICE_VARS.h"

C     === Atmos/Ocean/Seaice Interface Variables ===
#include "ATM2D_VARS.h"

c start phasing out wind stress to ocean at this ice fraction
      _RS windice_thres
      PARAMETER ( windice_thres= 0.5 ) 

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER i,j

      DO j=1, sNy
        DO i=1,sNx
        
          IF (maskC(i,j,1,1,1) .EQ. 1.) THEN

C         Ad hoc phase out wind stress if sufficient ice coverage
C         similar idea as stressReduction used in thsice_main
          IF (iceMask(i,j,1,1) .GT. windice_thres) THEN

           fu_2D(i,j)= fu_2D(i,j) * (1. _d 0 - 
     &          (iceMask(i,j,1,1) + iceMask(i-1,j,1,1)) * 0.5 _d 0 )
     &                 / (1. _d 0 - windice_thres)
           fv_2D(i,j)= fv_2D(i,j) * (1. _d 0 -
     &          (iceMask(i,j,1,1) + iceMask(i,j-1,1,1)) * 0.5 _d 0 )
     &                 / (1. _d 0 - windice_thres)
           wspeed_2D(i,j)= wspeed_2D(i,j) 
     &                    * (1. _d 0 - iceMask(i,j,1,1))
     &                    / (1. _d 0 - windice_thres)
          ENDIF

          sum_runoff(i,j)= sum_runoff(i,j) + runoff_2D(i,j)
          sum_precip(i,j)= sum_precip(i,j) + 
     &                     precipo_2D(i,j)*(1. _d 0-iceMask(i,j,1,1))
          sum_evap(i,j)= sum_evap(i,j) + 
     &                   evapo_2D(i,j)*(1. _d 0-iceMask(i,j,1,1))
          sum_qnet(i,j)= sum_qnet(i,j) + 
     &                   qneto_2D(i,j)*(1. _d 0-iceMask(i,j,1,1))
          sum_fu(i,j)= sum_fu(i,j) + fu_2D(i,j)
          sum_fv(i,j)= sum_fv(i,j) + fv_2D(i,j)
          sum_wspeed(i,j)= sum_wspeed(i,j) + wspeed_2D(i,j)
          sum_solarnet(i,j)= sum_solarnet(i,j) + 
     &               solarnet_ocn_2D(i,j)*(1. _d 0-iceMask(i,j,1,1))
          sum_slp(i,j)= sum_slp(i,j) + slp_2D(i,j)
          sum_pCO2(i,j)= sum_pCO2(i,j) + pCO2_2D(i,j)
          ENDIF
        ENDDO
      ENDDO  

C      PRINT *,'Sum_ocn_fluxes:',JBUGI,JBUGJ,fu_2D(JBUGI,JBUGJ),
C     &       fv_2D(JBUGI,JBUGJ), runoff_2D(JBUGI,JBUGJ), 
C     &       precipo_2D(JBUGI,JBUGJ), evapo_2D(JBUGI,JBUGJ),
C     &       qneto_2D(JBUGI,JBUGJ)
      
      RETURN
      END
