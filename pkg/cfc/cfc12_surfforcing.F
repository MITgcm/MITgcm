C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc12_surfforcing.F,v 1.4 2013/06/10 02:56:57 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C     !ROUTINE: CFC12_SURFFORCING
C     !INTERFACE:
      SUBROUTINE CFC12_SURFFORCING(
     I                          pTr_CFC12, AtmosCFC12,
     O                          fluxCFC12,
     I                          bi, bj, iMin, iMax, jMin, jMax,
     I                          myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CFC12_SURFFORCING
C     | o Calculate the CFC12 air-sea fluxes
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "CFC.h"

C     !INPUT/OUTPUT PARAMETERS:
C     pTr_CFC12  :: ocean CFC12 concentration
C     AtmosCFC12 :: atmospheric CFC12 field
C     fluxCFC12  :: air-sea CFC12 fluxes
C     bi, bj     :: current tile indices
C     iMin,iMax  :: computation domain, 1rst index bounds
C     jMin,jMax  :: computation domain, 2nd  index bounds
C     myTime     :: current time in simulation
C     myIter     :: current iteration number
C     myThid     :: my Thread Id number
      _RL  pTr_CFC12 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  AtmosCFC12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  fluxCFC12 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj
      INTEGER iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_CFC

C     !LOCAL VARIABLES:
C i, j, k - Loop counters
      INTEGER i,j,k
C Solubility relation coefficients
      _RL SchmidtNocfc12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SolCFC12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL cfc12sat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Kwexch(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Csat(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ttemp, ttemp2

      k=1

C calculate SCHMIDT NO. for O2
        DO j=jMin,jMax
         DO i=iMin,iMax
           IF ( maskC(i,j,k,bi,bj).NE.zeroRS ) THEN
C calculate SCHMIDT NO. for CFC12
             SchmidtNocfc12(i,j) =
     &            sca_12_1
     &          + sca_12_2 * theta(i,j,k,bi,bj)
     &          + sca_12_3 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
     &          + sca_12_4 * theta(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
     &                *theta(i,j,k,bi,bj)
c calculate solubility for CFC12
             ttemp=( theta(i,j,k,bi,bj) + 273.16 _d 0)* 0.01 _d 0
             ttemp2=( B3_12 * ttemp + B2_12 )*ttemp + B1_12
             SolCFC12(i,j) =
     &           EXP ( A1_12
     &           +  A2_12 / ttemp
     &           +  A3_12 * log( ttemp )
     &           +  A4_12 * ttemp * ttemp
     &           +  Salt(i,j,k,bi,bj)* ttemp2 )
c     conversion from mol/(l * atm) to mol/(m^3 * atm)
             SolCFC12(i,j) = 1000. _d 0 * SolCFC12(i,j)
c     conversion from mol/(m^3 * atm) to mol/(m3 * pptv)
             SolCFC12(i,j) = 1. _d -12 * SolCFC12(i,j)
C Determine surface flux (Fcfc12)
             Csat(i,j) = SolCFC12(i,j)*AtmosP(i,j,bi,bj)
     &                     *AtmosCFC12(i,j)
             Kwexch(i,j) = (1. _d 0 - fIce(i,j,bi,bj))
     &                   * pisVel(i,j,bi,bj)
     &                   / SQRT(SchmidtNoCFC12(i,j)/660. _d 0)
             fluxCFC12(i,j) =
     &           Kwexch(i,j)*(Csat(i,j) - pTr_CFC12(i,j,1))

           ELSE
             fluxCFC12(i,j) = 0. _d 0
           ENDIF

         ENDDO
        ENDDO

#endif /* ALLOW_CFC */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
