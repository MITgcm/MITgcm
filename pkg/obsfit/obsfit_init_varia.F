#include "OBSFIT_OPTIONS.h"

CBOP
C !ROUTINE: OBSFIT_INIT_VARIA

C !INTERFACE:
      SUBROUTINE OBSFIT_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Initialize ObsFit variables (for cost function)
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

#ifdef ALLOW_GRDCHK
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     myThid :: my thread ID number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER itlo,ithi
      INTEGER jtlo,jthi
      INTEGER num_file
#ifdef ALLOW_GRDCHK
      INTEGER err
#endif /* ALLOW_GRDCHK */

C Initialize tiled cost function variables
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO num_file = 1, NFILESMAX_OBS
            obsfit_dummy(num_file,bi,bj) = 0. _d 0
          ENDDO
        ENDDO
      ENDDO

C Initialize global cost function variables
      _BEGIN_MASTER(myThid)

      DO num_file = 1, NFILESMAX_OBS
        objf_obsfit(num_file) = 0. _d 0
        num_obsfit(num_file) = 0. _d 0
        obsfit_globaldummy(num_file) = 0. _d 0
      ENDDO

      _END_MASTER(myThid)

#ifdef ALLOW_GRDCHK
      IF ( obsfitDoNcOutput ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO num_file = 1, NFILESMAX_OBS
              IF ( sampleNo(num_file,bi,bj).GT.0 ) THEN
C Write data from buffer to .nc file
                err = NF_SYNC( fidfwd_obs(num_file,bi,bj) )
                CALL OBSFIT_NF_ERROR(
     &               'INIT_VARIA: NF_SYNC fidfwd_obs',
     &               err,bi,bj,myThid )

# ifdef ALLOW_TANGENTLINEAR_RUN
                IF ( TANGENT_SIMULATION ) THEN
                  err = NF_SYNC( fidtan_obs(num_file,bi,bj) )
                  CALL OBSFIT_NF_ERROR(
     &                 'INIT_VARIA: NF_SYNC fidtan_obs',
     &                 err,bi,bj,myThid )
                ENDIF
# endif /* ALLOW_TANGENTLINEAR_RUN */

              ENDIF !IF (ObsfNo)
            ENDDO !DO num_file
          ENDDO !DO bi
        ENDDO! DO bj
      ENDIF !IF (obsfitDoNcOutput)
#endif /* ALLOW_GRDCHK */

      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
