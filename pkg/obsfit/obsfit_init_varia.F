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
#include "OBSFIT_SIZE.h"
#include "OBSFIT.h"
#ifdef ALLOW_GRDCHK
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     myThid    :: my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER itlo,ithi
      INTEGER jtlo,jthi
      INTEGER num_file
#ifdef ALLOW_GRDCHK
      INTEGER err
#endif
      jtlo = mybylo(myThid)
      jthi = mybyhi(myThid)
      itlo = mybxlo(myThid)
      ithi = mybxhi(myThid)

C Initialize tiled cost function variables
      DO bj = jtlo,jthi
       DO bi = itlo,ithi
        DO num_file=1,NFILESMAX_OBS
         obsfit_dummy(num_file,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C Initialize global cost function variables
      _BEGIN_MASTER(myThid)

      DO num_file=1,NFILESMAX_OBS
       objf_obsfit(num_file) = 0. _d 0
       num_obsfit(num_file) = 0. _d 0
       obsfit_globaldummy(num_file) = 0. _d 0
      ENDDO

      _END_MASTER(myThid)

#ifdef ALLOW_GRDCHK
      IF (obsfitDoNcOutput) THEN
       DO bj = jtlo,jthi
        DO bi = itlo,ithi
         DO num_file = 1, NFILESMAX_OBS
          IF (ObsNo(num_file).GT.0) THEN
C     Write data from buffer to .nc file
           err = NF_SYNC( fidfwd_obs(num_file,bi,bj) )
           IF ( err .NE. NF_NOERR ) THEN
            WRITE(standardMessageUnit,*)
     &           'S/R OBSFIT_INIT_VARIA: NF_SYNC returned: ',
     &           NF_STRERROR(err), ' for ', fidfwd_obs(num_file,bi,bj)
           ENDIF
c           CALL PROFILES_NF_ERROR(
c     &          'INIT_VARIA: NF_SYNC fidforward',
c     &          err,bi,bj,myThid )

# ifdef ALLOW_TANGENTLINEAR_RUN
           IF (TANGENT_SIMULATION) THEN
            err = NF_SYNC( fidtan_ob(num_file,bi,bj) )
            IF ( err .NE. NF_NOERR ) THEN
             WRITE(standardMessageUnit,*)
     &            'S/R OBSFIT_INIT_VARIA: NF_SYNC returned: ',
     &            NF_STRERROR(err), ' for ', fidtan_obs(num_file,bi,bj)
            ENDIF
c            CALL PROFILES_NF_ERROR(
c     &           'INIT_VARIA: NF_SYNC fidtangent',
c     &           err,bi,bj,myThid )
           ENDIF
# endif /* ALLOW_TANGENTLINEAR_RUN */

          ENDIF !IF (ProfNo)
         ENDDO !DO num_file
        ENDDO
       ENDDO
      ENDIF !IF (profilesDoNcOutput)
#endif /* ALLOW_GRDCHK */
      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
