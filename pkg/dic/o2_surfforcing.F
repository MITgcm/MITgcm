#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE O2_SURFFORCING( PTR_O2, GO2,
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           myIter, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE O2_SURFFORCING                               |
C     | o Calculate the oxygen air-sea flux terms                |
C     | o following external_forcing_o2.F from Mick             |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef DIC_BIOTIC
#include "DIC_ABIOTIC.h"
#include "PTRACERS.h"
#endif


C     == Routine arguments ==
      INTEGER myIter, myThid
      _RL myTime
      _RL  PTR_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax, bi, bj

#ifdef ALLOW_PTRACERS


C     == Local variables ==
C I, J, K - Loop counters
      INTEGER I,J,K
C Solubility relation coefficients
      _RL SchmidtNoO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL O2sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL FluxO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL AtmosO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  aTT 
      _RL  aTK
      _RL  aTS
      _RL  aTS2
      _RL  aTS3
      _RL  aTS4
      _RL  aTS5
      _RL  o2s
      _RL  ttemp
      _RL  stemp
      _RL  oCnew


      K=1

C calculate SCHMIDT NO. for O2
        DO j=jMin,jMax
          DO i=iMin,iMax
            IF (hFacC(i,j,k,bi,bj).NE.0.) THEN
              SchmidtNoO2(i,j) = 
     &            sox1 
     &          + sox2 * theta(i,j,k,bi,bj)
     &          + sox3 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)  
     &          + sox4 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj) 
     &                *theta(i,j,k,bi,bj)

C Determine surface flux of O2 
C exchange coeff, accounting for ice cover and Schmidt no.
              Kwexch(i,j) = 
     &             pisvel(i,j,bi,bj)
     &             / sqrt(SchmidtNoO2(i,j)/660.0)
c ice influence
cQQ           Kwexch(i,j)  =(1.d0-Fice(i,j,bi,bj))*Kwexch(i,j)

              ttemp = theta(i,j,k,bi,bj)
              stemp = salt(i,j,k,bi,bj)
C determine saturation O2
C using Garcia and Gordon (1992), L&O (mistake in original???)
              aTT  = 298.15-ttemp
              aTK  = 273.15+ttemp
              aTS  = log(aTT/aTK)
              aTS2 = aTS*aTS
              aTS3 = aTS2*aTS
              aTS4 = aTS3*aTS
              aTS5 = aTS4*aTS

              oCnew  = oA0 + oA1*aTS + oA2*aTS2 + oA3*aTS3 + 
     &            oA4*aTS4 + oA5*aTS5
     &          + stemp*(oB0 + oB1*aTS + oB2*aTS2 + oB3*aTS3)
     &          + oC0*(stemp*stemp)

              o2s = EXP(oCnew)

c Convert from ml/l to mol/m^3
              O2sat(i,j) = o2s/22391.6*1000.0

c Determine flux, inc. correction for local atmos surface pressure
cQQ PTR_O2?
              FluxO2(i,j) = maskC(i,j,k,bi,bj)*Kwexch(i,j)*
     &                     (atmosP(i,j,bi,bj)*O2sat(i,j) 
     &                      - PTR_O2(i,j,1)) 
            ELSE
              FluxO2(i,j) = 0.d0
            ENDIF


          END DO
        END DO

C update surface tendencies      
        DO j=jMin,jMax
          DO i=iMin,iMax
           GO2(i,j)= maskC(i,j,1,bi,bj)*FluxO2(i,j)*recip_drF(1) 
          ENDDO
         ENDDO
#endif


        RETURN
        END

