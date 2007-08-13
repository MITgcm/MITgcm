C $Header: /u/gcmpack/MITgcm/pkg/dic/o2_surfforcing.F,v 1.14 2007/08/13 02:29:40 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: O2_SURFFORCING

C !INTERFACE: ==========================================================
      SUBROUTINE O2_SURFFORCING( PTR_O2, SGO2,
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           myIter, myTime, myThid )

C !DESCRIPTION:
C Calculate the oxygen air-sea flux terms               

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef DIC_BIOTIC
#include "DIC_ABIOTIC.h"
#endif


c  !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  PTR_O2               :: oxygen tracer field
      _RL myTime
      _RL  PTR_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin,iMax,jMin,jMax, bi, bj
      INTEGER myIter, myThid

c  !OUTPUT PARAMETERS: ===================================================
C  SGO2                  :: air-sea exchange of oxygen
      _RL  SGO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_PTRACERS

#ifdef ALLOW_O2

C !LOCAL VARIABLES: ===================================================
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
CEOP


      K=1

C calculate SCHMIDT NO. for O2
        DO j=jmin,jmax
          DO i=imin,imax
            IF (hFacC(i,j,k,bi,bj).NE.0.) THEN
              SchmidtNoO2(i,j) = 
     &            sox1 
     &          + sox2 * theta(i,j,k,bi,bj)
     &          + sox3 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)  
     &          + sox4 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj) 
     &                *theta(i,j,k,bi,bj)

C Determine surface flux of O2 
C exchange coeff, accounting for ice cover and Schmidt no.
              pisvel(i,j,bi,bj)  =0.337*wind(i,j,bi,bj)**2/3.6d5
              Kwexch(i,j) = 
     &             pisvel(i,j,bi,bj)
     &             / sqrt(SchmidtNoO2(i,j)/660.0)
c ice influence
              Kwexch(i,j)  =(1.d0-Fice(i,j,bi,bj))*Kwexch(i,j)

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

#ifdef USE_PLOAD
C Convert anomalous pressure pLoad (in Pa) from atmospheric model
C to total pressure (in Atm)
C Note: it is assumed the reference atmospheric pressure is 1Atm=1013mb
C       rather than the actual ref. pressure from Atm. model so that on
C       average AtmosP is about 1 Atm.
               AtmosP(i,j,bi,bj)= 1. _d 0 + pLoad(i,j,bi,bj)/Pa2Atm
#endif

c Determine flux, inc. correction for local atmos surface pressure
cQQ PTR_O2?
              FluxO2(i,j) = maskC(i,j,k,bi,bj)*Kwexch(i,j)*
     &                     (AtmosP(i,j,bi,bj)*O2sat(i,j) 
     &                      - PTR_O2(i,j,1)) 
            ELSE
              FluxO2(i,j) = 0.d0
            ENDIF


          END DO
        END DO

C update surface tendencies      
        DO j=jmin,jmax
          DO i=imin,imax
           SGO2(i,j)= maskC(i,j,1,bi,bj)*FluxO2(i,j)
     &         *recip_drF(1) * recip_hFacC(i,j,1,bi,bj)
          ENDDO
         ENDDO
#endif
#endif


        RETURN
        END

