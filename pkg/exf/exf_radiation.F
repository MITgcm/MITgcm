#include "EXF_OPTIONS.h"

CBOP
C !ROUTINE: EXF_RADIATION

C !INTERFACE: ==========================================================
      SUBROUTINE EXF_RADIATION( exf_Tsf, myTime, myIter, myThid )

C !DESCRIPTION:
C Set radiative fluxes at the surface
C
C - Use atmospheric state to compute surface fluxes.
C - Compute net from downward and downward from net longwave and
C   shortwave radiation, IF needed.
C   lwflux = Stefan-Boltzmann constant * emissivity * SST - lwdown
C   swflux = - ( 1 - albedo ) * swdown

C !USES:
      IMPLICIT NONE
C == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

C !INPUT PARAMETERS:
C myTime  :: Current time in simulation
C myIter  :: Current iteration number in simulation
C myThid  :: My Thread Id number
C exf_Tsf :: local copy of global field gcmSST or extrapolated
C            surface temperature (in deg Celsius)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      _RL exf_Tsf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_DOWNWARD_RADIATION
C !LOCAL VARIABLES: ====================================================
C bi,bj     :: Tile indices
C kl,ks    :: loop indices
      INTEGER bi,bj
      INTEGER i,j
#ifdef ALLOW_ATM_TEMP
c     INTEGER ks
      INTEGER kl
#endif
CEOP

#ifdef ALLOW_ATM_TEMP
c     ks = 1
      kl = 2
      IF ( usingPCoords ) THEN
c      ks = Nr
       kl = Nr-1
      ENDIF

      IF ( lwfluxfile .EQ. ' ' .AND. lwdownfile .NE. ' ' ) THEN
C     Loop over tiles.
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)

         DO j = 1,sNy
          DO i = 1,sNx
           lwflux(i,j,bi,bj) =
     &          ocean_emissivity*stefanBoltzmann*
     &          exf_Tsf(i,j,bi,bj)**4
     &          - lwdown(i,j,bi,bj)
#ifdef EXF_LWDOWN_WITH_EMISSIVITY
     &          *ocean_emissivity
C     the lw exitance (= out-going long wave radiation) is
C     emissivity*stefanBoltzmann*T^4 + rho*lwdown, where the
C     reflectivity rho = 1-emissivity for conservation reasons:
C     the sum of emissivity, reflectivity, and transmissivity must be
C     one, and transmissivity is zero in our case (long wave radiation
C     does not penetrate the ocean surface)
#endif /* EXF_LWDOWN_WITH_EMISSIVITY */
          ENDDO
         ENDDO

C--   end bi,bj loops
        ENDDO
       ENDDO
      ENDIF

C-jmc: commented out: no need to compute Downward-LW (not used) from Net-LW
c     IF ( lwfluxfile .NE. ' ' .AND. lwdownfile .EQ. ' ' ) THEN
C     Loop over tiles.
c      DO bj = myByLo(myThid),myByHi(myThid)
c       DO bi = myBxLo(myThid),myBxHi(myThid)
c        DO j = 1,sNy
c         DO i = 1,sNx
c          lwdown(i,j,bi,bj) =
c    &          ocean_emissivity*stefanBoltzmann*
c    &          ((gcmSST(i,j,bi,bj)+cen2kel)**4)
c    &          - lwflux(i,j,bi,bj)
c         ENDDO
c        ENDDO
c       ENDDO
c      ENDDO
c     ENDIF
#endif /* ALLOW_ATM_TEMP */

#if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
      IF ( swfluxfile .EQ. ' ' .AND. swdownfile .NE. ' ' ) THEN
#ifdef ALLOW_ZENITHANGLE
      IF ( useExfZenAlbedo .OR. useExfZenIncoming ) THEN
       CALL EXF_ZENITHANGLE(myTime, myIter, myThid)
#ifdef ALLOW_AUTODIFF
      ELSE
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           zen_albedo (i,j,bi,bj) = 0. _d 0
           zen_fsol_diurnal (i,j,bi,bj) = 0. _d 0
           zen_fsol_daily (i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#endif
      ENDIF
#endif /* ALLOW_ZENITHANGLE */
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_ZENITHANGLE
         IF ( useExfZenAlbedo ) THEN
          DO j = 1,sNy
           DO i = 1,sNx
            swflux(i,j,bi,bj) = - swdown(i,j,bi,bj)
     &                        * (1.0-zen_albedo(i,j,bi,bj))
           ENDDO
          ENDDO
         ELSE
#endif /* ALLOW_ZENITHANGLE */
          DO j = 1,sNy
           DO i = 1,sNx
            swflux(i,j,bi,bj) = - swdown(i,j,bi,bj)
     &                        * (1.0-exf_albedo)
           ENDDO
          ENDDO
#ifdef ALLOW_ZENITHANGLE
         ENDIF
#endif
        ENDDO
       ENDDO
      ENDIF
C-jmc: commented out: no need to compute Downward-SW (not used) from Net-SW
c     IF ( swfluxfile .NE. ' ' .AND. swdownfile .EQ. ' ' ) THEN
c      DO bj = myByLo(myThid),myByHi(myThid)
c       DO bi = myBxLo(myThid),myBxHi(myThid)
c        DO j = 1,sNy
c         DO i = 1,sNx
c          swdown(i,j,bi,bj) = -swflux(i,j,bi,bj) / (1.0-exf_albedo)
c         ENDDO
c        ENDDO
c       ENDDO
c      ENDDO
c     ENDIF
#endif /* ALLOW_ATM_TEMP or SHORTWAVE_HEATING */

#endif /* ALLOW_DOWNWARD_RADIATION */

      RETURN
      END
