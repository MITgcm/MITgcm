#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE INIT_ATM2D(dtatm, dtocn, dtcouple, myThid )
C     *==========================================================*
C     |  INIT_1DTO2D                                             |
C     |    This initialization routine should be run after the   |
c     |    the ocean grid/pickup have been read in.               |
c     |                                                          |
c     |  Note: grid variable indices bi,bj are hard-coded 1,1    |
c     |  This should work if coupler or atmos/coupler on one     |
c     |  machine.                                                |
c     |                                                          |
C     *==========================================================*
c
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "AGRID.h"

C     === Global Ocean Variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     === Global SeaIce Parameters ===
#include "THSICE_PARAMS.h"

C     === Atmos/Ocean/Seaice Interface Variables ===
#include "ATM2D_VARS.h"
 

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     dtatm, dtocn, dtcouple - Timesteps from couple.nml (hours)
C     myThid - Thread no. that called this routine.
      INTEGER dtatm, dtocn, dtcouple
      INTEGER myThid

C     LOCAL VARIABLES:
      INTEGER i,j,jj
      INTEGER ib, ibj1, ibj2  ! runoff band loop counters
      INTEGER j_atm, mn
      INTEGER dUnit
      _RL end1, end2, enda1, enda2, enda3 !used to compute grid conv areas
      _RL totrun_b(sNy) ! total file "runoff" in runoff bands
      _RL a1,a2
      _RS atm_dyG(jm0)  ! southern point/(boundary) of atmos grid
      DATA atm_dyG/2.0,44*4.0,2.0/  ! grid spacing for atmosphere

      dtatmo = dtatm * 3600.
      dtocno = dtocn * 3600.
      dtcouplo= dtcouple * 3600.

C override data.ice seaice time step parms
C these will need to change if coupling procedure changed
      thSice_deltaT = dtcouplo
      thsIce_dtTemp = dtatmo
      ocean_deltaT = dtcouplo

CJRS  This next check - only kill it if not MPI?
      IF (dtocno.NE.dTtracerLev(1)) THEN
        PRINT *,'Ocean tracer timestep differs between coupler '
        PRINT *,'and the ocean data file'
        STOP
      ENDIF

c Assuming the atmospheric grid array not passed, do this:
      atm_yG(1)=-90.0
      DO j_atm=2,jm0
        atm_yG(j_atm)=atm_yG(j_atm-1)+atm_dyG(j_atm-1)
        atm_yC(j_atm-1)=(atm_yG(j_atm-1)+atm_yG(j_atm))/2.0
      ENDDO
      atm_yC(jm0)=atm_yG(jm0)+atm_dyG(jm0)/2.0

c end atmos grid initialization

      atm_oc_ind(1)=2
      atm_oc_wgt(1)=1. _d 0
      atm_oc_frac1(1)= (sin(yG(1,2,1,1)*deg2rad) - 
     &        sin(yG(1,1,1,1)*deg2rad))/
     &        (sin(atm_yG(3)*deg2rad)-sin(atm_yG(1)*deg2rad))
      atm_oc_frac2(1)= 0. _d 0   ! assumes ocean(1) fits in atm(1)
      atm_oc_ind(sNy)=jm0-1
      atm_oc_wgt(sNy)=1. _d 0
      atm_oc_frac1(sNy)= (sin((yG(1,sNy,1,1) + 
     &      dyG(1,sNy,1,1)/6.37D6/deg2rad)*deg2rad)-
     &      sin(yG(1,sNy,1,1)*deg2rad))/
     &      (sin((atm_yG(jm0)+atm_dyG(jm0))*deg2rad)- 
     &      sin(atm_yG(jm0-1)*deg2rad))
      atm_oc_frac2(sNy)= 0. _d 0   ! assumes ocean(1) fits in atm(1)

      endwgt1 = sin(atm_yG(2)*deg2rad)            !hard-coded that the atmos
      endwgt2 = sin(atm_yG(3)*deg2rad) - endwgt1  !grid is same in NH and SH
      endwgt1 = endwgt1 + 1. _d 0                 !and goes 90S to 90N
      rsumwgt = 1. _d 0/(endwgt1 + endwgt2)

      atm_yG(2)=atm_yG(1) ! grid now combined atm end points
      atm_yG(jm0)=90. _d 0

      DO j=2, sNy-1

        DO jj=2,jm0-1
          IF  ((yG(1,j,1,1).GE.atm_yG(jj)).AND.
     &         (yG(1,j,1,1).LT.atm_yG(jj+1))) j_atm=jj
        ENDDO  
        
        atm_oc_ind(j)=j_atm
        end1= sin(yG(1,j,1,1) *deg2rad)
        end2= sin(yG(1,j+1,1,1) *deg2rad)
        enda1 = sin(atm_yG(j_atm) *deg2rad)
        enda2 = sin(atm_yG(j_atm+1) *deg2rad)
        IF ( yG(1,j+1,1,1) .GT. atm_yG(j_atm+1) ) THEN
           enda3 = sin(atm_yG(j_atm+2) *deg2rad)
           atm_oc_wgt(j)=(enda2-end1)/ (end2-end1)
           atm_oc_frac1(j)= (enda2-end1) / (enda2 - enda1)
           atm_oc_frac2(j)= (end2 - enda2) / (enda3 - enda2)
        ELSE
          atm_oc_wgt(j)=1. _d 0
          atm_oc_frac1(j)= (end2-end1)/ (enda2-enda1)
          atm_oc_frac2(j)=0. _d 0
        ENDIF
      ENDDO
c
c find land fraction
c
      DO j_atm=1,jm0
        cflan(j_atm)=0. _d 0
        ocnArea(j_atm)=0. _d 0
      ENDDO

      DO j=1,sNy
        DO i=1,sNx
          IF (maskC(i,j,1,1,1).EQ.1.) THEN
            ocnArea(atm_oc_ind(j))=ocnArea(atm_oc_ind(j)) +
     &                           rA(i,j,1,1)*atm_oc_wgt(j)
            IF (atm_oc_wgt(j).LT.1.d0) THEN
              ocnArea(atm_oc_ind(j)+1)=ocnArea(atm_oc_ind(j)+1) +
     &                           rA(i,j,1,1)*(1.d0-atm_oc_wgt(j))
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      DO j_atm=3,jm0-2
        cflan(j_atm)=1. _d 0 - ocnArea(j_atm) / 
     &              (2. _d 0 * PI * 6.37 _d 6 * 6.37 _d 6 * 
     &    (sin(atm_yG(j_atm+1)*deg2rad) - sin(atm_yG(j_atm)*deg2rad)))
        if (cflan(j_atm).LT.1. _d -14) cflan(j_atm)=0. _d 0
      ENDDO

C     deal with the combined atmos grid end cells...
      cflan(2)= 1. _d 0 - ocnArea(2) / 
     &         (2. _d 0*PI*6.37 _d 6*6.37 _d 6*
     &         (sin(atm_yG(3)*deg2rad)+1. _d 0))
      IF (cflan(2).LT.1. _d -14) cflan(2)=0. _d 0
      cflan(1)=cflan(2)
      cflan(jm0-1)= 1.d0 - ocnArea(jm0-1) / 
     &             (2. _d 0*PI*6.37 _d 6*6.37 _d 6*
     &             (1. _d 0-sin(atm_yG(jm0-1)*deg2rad)))
      IF (cflan(jm0-1).LT.1. _d -14) cflan(jm0-1)=0. _d 0
      cflan(jm0)=cflan(jm0-1)

      PRINT *,'Land fractions on atmospheric grid: ' 
      PRINT *, cflan
      PRINT *,'Lookup grid index, weights:'
      PRINT *, atm_oc_ind,atm_oc_wgt
C      PRINT *,'Lookup fraction 1 of atmos grid:'
C      PRINT *, atm_oc_frac1
C      PRINT *,'Lookup fraction 2 of atmos grid:'
C      PRINT *, atm_oc_frac2

c
c read in mean 1D atmos wind files -- store in memory
c
      DO j_atm=1,jm0
        DO mn=1,nForcingPer
          atau(j_atm,mn)=0. _d 0
          atav(j_atm,mn)=0. _d 0
          awind(j_atm,mn)=0. _d 0
        ENDDO
      ENDDO

      CALL MDSFINDUNIT( dUnit, myThid )

      IF ( atmosTauuFile .NE. ' '  ) THEN
         OPEN(dUnit, FILE=atmosTauuFile,STATUS='old',
     &        ACCESS='direct', RECL=8*jm0*nForcingPer, 
     &        FORM='unformatted')
         READ(dUnit,REC=1), atau
         CLOSE(dUnit)
      ENDIF

      IF ( atmosTauvFile .NE. ' '  ) THEN
         OPEN(dUnit, FILE=atmosTauvFile, STATUS='old',
     &        ACCESS='direct', RECL=8*jm0*nForcingPer, 
     &        FORM='unformatted')
         READ(dUnit, REC=1), atav
         CLOSE(dUnit)
      ENDIF

      IF ( atmosWindFile .NE. ' '  ) THEN
         OPEN(dUnit, FILE=atmosWindFile, STATUS='old',
     &        ACCESS='direct', RECL=8*jm0*nForcingPer, 
     &        FORM='unformatted')
         READ(dUnit, REC=1), awind
         CLOSE(dUnit)
      ENDIF

C The polar data point values for winds are effectively N/A given the
C pole issue... although they are read in here, they are never used in
C any calculations, as the polar ocean points access the data at atmos
C 2 and jm0-1 points.


c read in runoff data
c to put runoff into specific grid cells
c
      IF ( runoffMapFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( runoffMapFile, ' ',
     &                      runoffVal, 0, myThid )
      ELSE
        DO j=1,sNy
          DO i=1,sNx
            if ( (maskC(i,j,1,1,1).EQ.1.) .AND.
     &          ( (maskC(i-1,j,1,1,1).EQ.0.).OR.
     &            (maskC(i+1,j,1,1,1).EQ.0.).OR.
     &            (maskC(i,j-1,1,1,1).EQ.0.).OR.
     &            (maskC(i,j+1,1,1,1).EQ.0.).OR.
     &            (maskC(i+1,j+1,1,1,1).EQ.0.).OR.
     &            (maskC(i-1,j-1,1,1,1).EQ.0.).OR.
     &            (maskC(i+1,j-1,1,1,1).EQ.0.).OR.
     &            (maskC(i-1,j+1,1,1,1).EQ.0.) ) ) THEN
              runoffVal(i,j)=1. _d 0
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      DO ib=1,numBands
        ibj1=1
        if (ib.GT.1) ibj1=  rband(ib-1)+1
        ibj2=sNy
        if (ib.LT.numBands) ibj2= rband(ib)
        totrun_b(ib)=0.d0
        DO j=ibj1,ibj2
          DO i=1,sNx
            totrun_b(ib)=totrun_b(ib)+runoffVal(i,j)*maskC(i,j,1,1,1)
          ENDDO
        ENDDO
        DO j=ibj1,ibj2
          runIndex(j)= ib     ! for lookup of rband as fn. of latitude
          DO i=1,sNx
            runoffVal(i,j)=runoffVal(i,j)*maskC(i,j,1,1,1)/totrun_b(ib)
          ENDDO
        ENDDO
      ENDDO

      CALL INIT_SUMVARS(myThid)
 
C     Initialize 1D diagnostic variables 
      DO j_atm=1,jm0
        DO mn=1,nForcingPer
          sum_tauu_ta(j_atm,mn)= 0. _d 0
          sum_tauv_ta(j_atm,mn)= 0. _d 0
          sum_wsocean_ta(j_atm,mn)= 0. _d 0
          sum_ps4ocean_ta(j_atm,mn)= 0. _d 0
        ENDDO
      ENDDO

C     Initialize 2D diagnostic variables 
      DO i=1-OLx,sNx+OLx
        DO j=1-OLy,sNy+OLy
          DO mn=1,nForcingPer
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

C     Initialize following for safety and/or cold start
      DO i=1-OLx,sNx+OLx
        DO j=1-OLy,sNy+OLy
          pass_runoff(i,j)= 0. _d 0
          pass_qnet(i,j)= 0. _d 0
          pass_evap(i,j)= 0. _d 0
          pass_precip(i,j)= 0. _d 0
          pass_fu(i,j)= 0. _d 0
          pass_fv(i,j)= 0. _d 0
          pass_wspeed(i,j)= 0. _d 0
          pass_solarnet(i,j)= 0. _d 0
          pass_slp(i,j)= 0. _d 0
          pass_siceLoad(i,j)= 0. _d 0
          pass_pCO2(i,j)= 0. _d 0
          pass_prcAtm(i,j)= 0. _d 0
          sFluxFromIce(i,j)= 0. _d 0
        ENDDO
      ENDDO

      RETURN
      END

