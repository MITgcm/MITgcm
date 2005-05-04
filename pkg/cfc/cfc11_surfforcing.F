#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE cfc11_SURFFORCING( PTR_cfc11, surfcfc11,
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           myIter, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CFC11_SURFFORCING                               |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "CFC.h"

C     == Routine arguments ==
      INTEGER myIter, myThid
      _RL myTime
      _RL  PTR_cfc11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  surfcfc11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax, bi, bj

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_CFC


C     == Local variables ==
C I, J, K - Loop counters
      INTEGER I,J,K
C Solubility relation coefficients
      _RL SchmidtNocfc11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SolCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cfc11sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fluxcfc11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Csat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ttemp, ttemp2

      K=1

C calculate SCHMIDT NO. for O2
        DO j=jMin,jMax
          DO i=iMin,iMax
            IF (hFacC(i,j,k,bi,bj).NE.0.) THEN
C calculate SCHMIDT NO. for CFC11
              SchmidtNocfc11(i,j) = 
     &            sca_11_1 
     &          + sca_11_2 * theta(i,j,k,bi,bj)
     &          + sca_11_3 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)  
     &          + sca_11_4 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj) 
     &                *theta(i,j,k,bi,bj)
c calculate solubility for CFC11
              ttemp=( theta(i,j,k,bi,bj) + 273.16)* 0.01
              ttemp2=( B3_11 * ttemp + B2_11 )*
     &            ttemp + B1_11 
              SolCFC11(i,j)
     &         = exp ( A1_11
     &           +  A2_11 / ttemp
     &           +  A3_11 * log( ttemp )
     &           +  A4_11 * ttemp * ttemp
     &           +  Salt(i,j,k,bi,bj)* ttemp2 )
c     conversion from mol/(l * atm) to mol/(m^3 * atm)
              SolCFC11(i,j) = 1000. * SolCFC11(i,j)
c     conversion from mol/(m^3 * atm) to mol/(m3 * pptv)
              SolCFC11(i,j) = 1.0e-12 * SolCFC11(i,j)
C Determine surface flux (Fcfc11)
              Csat(i,j) = SolCFC11(i,j)*AtmosP(i,j,bi,bj)
     &                     *AtmosCFC11(i,j,bi,bj)
              Kwexch(i,j) = (1.0 - fice(i,j,bi,bj))*pisvel(i,j,bi,bj)
     &                    / sqrt(SchmidtNoCFC11(i,j)/660.0)
              FluxCFC11(i,j) =
     &        Kwexch(i,j)*(Csat(i,j) - PTR_CFC11(i,j,1))

            ELSE
              FluxCFC11(i,j) = 0.d0
            ENDIF


          ENDDO
        ENDDO

C update surface tendencies      
        DO j=jMin,jMax
          DO i=iMin,iMax
           SURFCFC11(i,j)=
     &         maskC(i,j,1,bi,bj)*FluxCFC11(i,j)*recip_drF(1) 
          ENDDO
         ENDDO
#endif
#endif


        RETURN
        END

