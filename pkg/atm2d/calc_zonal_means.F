#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE CALC_ZONAL_MEANS(doAll, myTime, myIter, myThid )
C     *==========================================================*
C     | Calculate zonal mean ocean quantities (at a specific     |
C     | point in time). If first argument is false, only seaice  |
C     | means are calculated, i.e. called after an atm timestep. |
C     *==========================================================*
        IMPLICIT NONE

C     === Global Atmosphere Variables ===
#include "ATMSIZE.h"
#include "AGRID.h"

C     === Global Ocean Variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"

C     === Global SeaIce Variables === 
#include "THSICE_VARS.h"
      INTEGER siLo, siHi, sjLo, sjHi
      PARAMETER ( siLo = 1-OLx , siHi = sNx+OLx )
      PARAMETER ( sjLo = 1-OLy , sjHi = sNy+OLy )

C     === Atmos/Ocean/Seaice Interface Variables ===
#include "ATM2D_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     doAll   - boolean, false -> only vars changed after atm step
C     myTime - current simulation time (ocean model time)
C     myIter - iteration number (ocean model)
C     myThid  - Thread no. that called this routine.
      LOGICAL doAll
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     LOCAL VARIABLES:
      _RL mWgt       ! weight of ocean point j+1
      INTEGER i,j    ! loop counters for the ocean grid
      INTEGER j_atm  ! loop counter for the atm grid

      DO j_atm=1,jm0
        IF (doAll) THEN
          ctocn(j_atm)=0. _d 0
          cfice(j_atm)=0. _d 0
          cco2flux(j_atm)=0. _d 0
        ENDIF
        ctice(j_atm)=0. _d 0
        csAlb(j_atm)=0. _d 0
      ENDDO
      
       CALL THSICE_ALBEDO(
     I          1, 1, siLo, siHi, sjLo, sjHi,
     I          1, sNx, 1, sNy,
     I          iceMask(siLo,sjLo,1,1), iceHeight(siLo,sjLo,1,1),
     I          snowHeight(siLo,sjLo,1,1), Tsrf(siLo,sjLo,1,1),
     I          snowAge(siLo,sjLo,1,1),
     O          siceAlb(siLo,sjLo,1,1),
     I          myTime, myIter, myThid )

      DO j=1,sNy
       DO i=1,sNx

         IF (maskC(i,j,1,1,1).EQ.1.) THEN

           IF (doAll) THEN
             ctocn(atm_oc_ind(j))= ctocn(atm_oc_ind(j)) + 
     &                             sstFromOcn(i,j) * rA(i,j,1,1) * 
     &                      (1. _d 0-iceMask(i,j,1,1))*atm_oc_wgt(j)
             cfice(atm_oc_ind(j))=cfice(atm_oc_ind(j)) + 
     &              rA(i,j,1,1)*iceMask(i,j,1,1)*atm_oc_wgt(j)
             cco2flux(atm_oc_ind(j))=cco2flux(atm_oc_ind(j)) + 
     &                  oFluxCO2(i,j)*rA(i,j,1,1)*atm_oc_wgt(j)
           ENDIF
           ctice(atm_oc_ind(j))=ctice(atm_oc_ind(j)) + Tsrf(i,j,1,1)
     &             *rA(i,j,1,1)*iceMask(i,j,1,1)*atm_oc_wgt(j)
           csAlb(atm_oc_ind(j))=csAlb(atm_oc_ind(j)) + siceAlb(i,j,1,1)
     &             *rA(i,j,1,1)*iceMask(i,j,1,1)*atm_oc_wgt(j)


           IF (atm_oc_wgt(j).LT.1. _d 0) THEN
             mWgt= 1. _d 0-atm_oc_wgt(j)
             IF (doAll) THEN
               ctocn(atm_oc_ind(j)+1)= ctocn(atm_oc_ind(j)+1) + 
     &                                 sstFromOcn(i,j) * rA(i,j,1,1) * 
     &                                 (1. _d 0-iceMask(i,j,1,1))*mWgt
               cfice(atm_oc_ind(j)+1)= cfice(atm_oc_ind(j)+1) + 
     &             rA(i,j,1,1)*iceMask(i,j,1,1)*mWgt
               cco2flux(atm_oc_ind(j)+1)= cco2flux(atm_oc_ind(j)+1) + 
     &                  oFluxCO2(i,j)*rA(i,j,1,1)*mWgt
             ENDIF
             ctice(atm_oc_ind(j)+1)= ctice(atm_oc_ind(j)+1) + 
     &             Tsrf(i,j,1,1)*rA(i,j,1,1)*iceMask(i,j,1,1)*mWgt
             csAlb(atm_oc_ind(j)+1)= csAlb(atm_oc_ind(j)+1) + 
     &             siceAlb(i,j,1,1)*rA(i,j,1,1)*iceMask(i,j,1,1)*mWgt
           ENDIF

         ENDIF

       ENDDO
      ENDDO

      DO j_atm=2,jm0-1
        
        IF (ocnArea(j_atm).GT.1. _d -32) THEN
          
          IF (doAll)THEN
            cfice(j_atm)= cfice(j_atm)/ocnArea(j_atm)
            cco2flux(j_atm)= cco2flux(j_atm)/ocnArea(j_atm)
          ENDIF
          IF (cfice(j_atm).GT.1. _d -32) THEN
            ctice(j_atm)= ctice(j_atm)/ocnArea(j_atm)/cfice(j_atm)
            csAlb(j_atm)= csAlb(j_atm)/ocnArea(j_atm)/cfice(j_atm)
          ENDIF

          IF ((1. _d 0-cfice(j_atm).GT.1. _d -32).AND.doAll) 
     &        ctocn(j_atm)= ctocn(j_atm)/ocnArea(j_atm)
     &                        /(1. _d 0-cfice(j_atm))

        ENDIF
       
C       At present, keeping separate variables in AGRID.h and ATM2D_VARS.h

        IF (doALL) THEN
          mmsst(j_atm)= ctocn(j_atm)
          mmfice(j_atm)= cfice(j_atm)
          mmco2flux(j_atm)= cco2flux(j_atm)
        ENDIF
        mmtice(j_atm)= ctice(j_atm)
        mmsAlb(j_atm)= csAlb(j_atm)
      
      ENDDO

C     Copy data to atmosphere polar points
      IF (doALL) THEN
          mmsst(1)= ctocn(2)
          mmsst(jm0)= ctocn(jm0-1)
          mmfice(1)= cfice(2)
          mmfice(jm0)= cfice(jm0-1)
          mmco2flux(1)= cco2flux(2)
          mmco2flux(jm0)= cco2flux(jm0-1)
      ENDIF
        mmtice(1)= ctice(2)
        mmtice(jm0)= ctice(jm0-1)
        mmsAlb(1)= csAlb(2)
        mmsAlb(jm0)= csAlb(jm0-1)

      RETURN
      END
