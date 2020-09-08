#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_VOLFRAC
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_VOLFRAC(
     I                       bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SALT_PLUME_VOLFRAC
C     | o Compute saltplume penetration.
C     *==========================================================*
C     | Compute fraction of volume flux associated with saltplume
C     | flux penetrating through the entire water columns due to
C     | rejected salt during freezing.
C     |
C     | For example, if surface value is Saltplume0,
C     | and each level gets equal fraction 1/5 down to SPDepth=5,
C     | SALT_PLUME_VOLFRAC will report
C     | dSPvolkLev2Above[2to1,3to2,4to3,5to4,6to5] = [4/5,3/5,2/5,1/5,  0]
C     | dSPvolSurf2kLev [1to1,1to2,1to3,1to4,1to5] = [1/5,1/5,1/5,1/5,1/5]
C     | sum [into5] = 1to5 + 6to5 - 5to4 = 1/5 +   0 - 1/5 = 0
C     |     [into4] = 1to4 + 5to4 - 4to3 = 1/5 + 1/5 - 2/5 = 0
C     |     [into3] = 1to3 + 4to3 - 3to2 = 1/5 + 2/5 - 3/5 = 0
C     |     [into2] = 1to2 + 3to2 - 2to1 = 1/5 + 3/5 - 4/5 = 0
C     |     [into1] = 1to1 + 2to1 - 1to[1,2,3,4,5] = 1/5 + 4/5 - 5/5 = 0
C     | NOTE: volume will always be conserved.
C     | =====
C     | Written by   : ATN (based on SALT_PLUME_FRAC)
C     | Date         : Apr 14, 2014
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "SALT_PLUME.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     input arguments
C     SPDpeth :: corresponding SaltPlumeDepth(i,j) at this grid point
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id. number
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     input/output arguments
C      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_SALT_PLUME
#ifdef SALT_PLUME_VOLUME

C     !LOCAL VARIABLES:
      _RL     dMbdt        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     temp         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dplumek
      INTEGER SPkBottom    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,k,kp1,Nlev,Nrp1
      INTEGER imt
      parameter( imt=(sNx+2*OLx)*(sNy+2*OLy) )

C initialize at every time step
      Nrp1=Nr+1
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dSPvolSurf2kLev(i,j,k,bi,bj)  = 0. _d 0
         dSPvolkLev2Above(i,j,k,bi,bj) = 0. _d 0
         SPplumek(i,j,k,bi,bj)         = 1. _d 0
        ENDDO
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        SPplumek(i,j,Nrp1,bi,bj)         = 1. _d 0
        SPbrineVolFlux(i,j,bi,bj)        = 0. _d 0
        SPkBottom(i,j)                   = 0
       ENDDO
      ENDDO

C call salt_plume_frac to fill in SPplumek and SPkBottom
C use dMbdt+temp as a temporary arrays here to save memory:
      DO k = Nrp1,1,-1
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         temp(i,j)=SaltPlumeDepth(i,j,bi,bj)
         dMbdt(i,j)=abs(rF(k))
        ENDDO
       ENDDO
       CALL SALT_PLUME_FRAC(
     I               imt,oneRS,temp,
#ifdef SALT_PLUME_SPLIT_BASIN
     I               XC(1-Olx,1-Oly,bi,bj),YC(1-Olx,1-Oly,bi,bj),
#endif
     U               dMbdt,
     I               myTime, 1, myThid )
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         SPplumek(i,j,k,bi,bj)=dMbdt(i,j)
         IF(SPplumek(i,j,k,bi,bj).GT. 0.9999999) THEN
          SPkBottom(i,j)=k
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C reinitialize dMbdt = 0
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        dMbdt(i,j)=0. _d 0
       ENDDO
      ENDDO

C Now calculating dplumek, dSPvolumeUp, dSPvolSurf2kLev
C units:
C Sbrine=dsb/dt*dt/(rhoConst*SPalpha*drF)[g/m2/s*s/(kg/m3*m)]=[g/kg]
C SPplumek : fraction : unitless
C SaltPlumeFlux: dsb/dt [g/kg.kg/m^2/s = g/m^2/s]
C brine_mass_flux dMb/dt = dsb/dt / Sbrine [kg/m2/s]
C                        = dsb/dt / (dsb/dt*dt/(rhoConst*SPalpha*drF))
C                        = rhoConst*SPalpha*drF/dt [kg/m3 m/s]=[kg/m2/s]
C dVbrine/dt = dMb/dt 1/rhoConst [m/s]

C has 2 ways to define brine properties: either provide
C (A) SPalpha: vol frac or (B) SPbrineSalt: brine salinity.
C (A) SPalpha:  can calc SPbrineSalt as fxn of dhice/dt,
C     constrained by SPbrineSaltmax:
C     SPbrineSalt=SaltPlumeFlux/rhoConst/SPalpha/drF(1)*dt
C     SPbrineSalt=min(SPbrineSalt,SPbrineSaltmax)
C     dMbdt = saltPlumeFlux / SPbrineSalt
C           = rhoConst*SPalpha*drF(1)/dt <-- a function of SPalpha
C (B) SPbrinesalt provided
C     dMbdt = saltPlumeFlux / SPbrineSalt <-- fxn of SPbrineSalt

C Assuming we go with (B) here:
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C brine mass and volume at surface:
        dMbdt(i,j)=saltPlumeFlux(i,j,bi,bj)/SPbrineSconst
        SPbrineVolFlux(i,j,bi,bj)=dMbdt(i,j)*mass2rUnit
       ENDDO
      ENDDO

C Distributing down: this is always from level 1 to depth
      DO k=Nr,1,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dplumek=SPplumek(i,j,k+1,bi,bj)-SPplumek(i,j,k,bi,bj)
         dSPvolSurf2kLev(i,j,k,bi,bj)=dplumek*SPbrineVolFlux(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C Now volume up: need to scan from bottom of SPDepth
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Nlev=SPkBottom(i,j)
        IF(Nlev.GE.1 .AND. Nlev.LE.Nr) THEN
         DO k=Nlev,1,-1
          kp1=k+1
          dSPvolkLev2Above(i,j,k,bi,bj)=dSPvolkLev2Above(i,j,kp1,bi,bj)
     &                                  - dSPvolSurf2kLev(i,j,k,bi,bj)
         ENDDO
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(
     &      SPplumek,'PLUMEKB1',0,Nr,1,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* SALT_PLUME_VOLUME */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
