#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: NF90IO_OPEN

C     !INTERFACE:
      FUNCTION NF90IO_OPEN(fname, ncid)
C     !DESCRIPTION:
C     Convenience wrappper around NF90_OPEN that
C     opens in non-parallel if usingMPI not set

C     !USES:
      use netcdf
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "NF90IO.h"

C     !INPUT PARAMETERS:
C     fname :: path to file to open
      CHARACTER (LEN=*), intent(in) :: fname

C     !OUTPUT PARAMETERS:
C     ncid        :: netCDF id of opened file
C     NF90IO_OPEN :: error code
      INTEGER  :: ncid
CEOP

#ifdef ALLOW_NF90IO

C     !LOCAL VARIABLES:
      INTEGER i, err, info, mode

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (usingMPI) THEN
#ifdef ALLOW_USE_MPI
         mode = IOR(nf90_write, nf90_mpiio)
         CALL MPI_INFO_CREATE(info, err)
         DO i=1,NF90IO_MAX_N_INFO
          IF ( nf90io_info_keys(i) .NE. ' ' .AND.
     &         nf90io_info_values(i) .NE. ' ' ) THEN
           CALL MPI_INFO_SET(info, nf90io_info_keys(i),
     &                             nf90io_info_values(i), err)
          ENDIF
         ENDDO
         err = nf90_open(fname, mode, ncid,
     &                   comm=MPI_COMM_MODEL, info=info)
         CALL MPI_INFO_FREE(info, err)
#endif
      ELSE
         err = nf90_open(fname, nf90_write, ncid)
      ENDIF

      NF90IO_OPEN = err

#endif /* ALLOW_NF90IO */

      END FUNCTION

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
