
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: NF90IO_CLOSE

C     !INTERFACE:
      SUBROUTINE NF90IO_CLOSE( ncid, myThid )

C     !DESCRIPTION:
C     Close netCDF file.

C     !USES:
#ifdef ALLOW_NF90IO
      use netcdf
#endif
      IMPLICIT NONE
#ifdef ALLOW_NF90IO
#include "NF90IO.h"
#endif

C     !INPUT PARAMETERS:
C     ncid    :: netCDF id of file to close
C     myThid  :: my Thread Id number
      INTEGER ncid, myThid
CEOP

#ifdef ALLOW_NF90IO

C     !LOCAL VARIABLES:
      INTEGER err

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      err = nf90_close(ncid)
      CALL nf90ERR(err, "Closing netcdf file", myThid)

#endif /* ALLOW_NF90IO */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
