#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE cfc12_SURFFORCING( PTR_cfc12, surfcfc12,
     I           bi,bj,iMin,iMax,jMin,jMax,
     I           myIter, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE CFC12_SURFFORCING                               |
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
      _RL  PTR_cfc12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  surfcfc12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax, bi, bj

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_CFC


C     == Local variables ==
C I, J, K - Loop counters
      INTEGER I,J,K
C Solubility relation coefficients
      _RL SchmidtNocfc12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SolCFC12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cfc12sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fluxcfc12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Csat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ttemp, ttemp2

      K=1

C calculate SCHMIDT NO. for O2
        DO j=jMin,jMax
          DO i=iMin,iMax
            IF (hFacC(i,j,k,bi,bj).NE.0.) THEN
C calculate SCHMIDT NO. for CFC12
              SchmidtNocfc12(i,j) = 
     &            sca_12_1 
     &          + sca_12_2 * theta(i,j,k,bi,bj)
     &          + sca_12_3 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)  
     &          + sca_12_4 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj) 
     &                *theta(i,j,k,bi,bj)
c calculate solubility for CFC12
              ttemp=( theta(i,j,k,bi,bj) + 273.16)* 0.01
              ttemp2=( B3_12 * ttemp + B2_12 )*
     &            ttemp + B1_12 
              SolCFC12(i,j)
     &         = exp ( A1_12
     &           +  A2_12 / ttemp
     &           +  A3_12 * log( ttemp )
     &           +  A4_12 * ttemp * ttemp
     &           +  Salt(i,j,k,bi,bj)* ttemp2 )
c     conversion from mol/(l * atm) to mol/(m^3 * atm)
              SolCFC12(i,j) = 1000. * SolCFC12(i,j)
c     conversion from mol/(m^3 * atm) to mol/(m3 * pptv)
              SolCFC12(i,j) = 1.0e-12 * SolCFC12(i,j)
C Determine surface flux (Fcfc12)
              Csat(i,j) = SolCFC12(i,j)*AtmosP(i,j,bi,bj)
     &                     *AtmosCFC12(i,j,bi,bj)
              Kwexch(i,j) = (1.0 - fice(i,j,bi,bj))*pisvel(i,j,bi,bj)
     &                    / sqrt(SchmidtNoCFC12(i,j)/660.0)
              FluxCFC12(i,j) =
     &        Kwexch(i,j)*(Csat(i,j) - PTR_CFC12(i,j,1))

            ELSE
              FluxCFC12(i,j) = 0.d0
            ENDIF


          ENDDO
        ENDDO

C update surface tendencies      
        DO j=jMin,jMax
          DO i=iMin,iMax
           SURFCFC12(i,j)=
     &         maskC(i,j,1,bi,bj)*FluxCFC12(i,j)*recip_drF(1) 
          ENDDO
         ENDDO
#endif
#endif


        RETURN
        END

