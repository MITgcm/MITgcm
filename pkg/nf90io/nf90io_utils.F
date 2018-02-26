#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CCCCCCCCCCCCCCC
      FUNCTION NF90IO_VAR_PAR_ACCESS(ncid, varid)

      use netcdf
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "NF90IO.h"



C     !DESCRIPTION:
C     Convenience wrappper around NF90_VAR_PAR_ACCESS that
C     doesnt do anything if usingMPI is false

      INTEGER, intent(in) :: ncid,varid
      INTEGER  :: err
      IF (usingMPI) THEN
         err = nf90_var_par_access(ncid, varid, nf90_collective)
      ELSE
         err = NF90_NOERR
      ENDIF
      NF90IO_VAR_PAR_ACCESS = err
      END FUNCTION

CCCCCCCCCCCCCCC
      FUNCTION NF90IO_OPEN(fname, ncid)

      use netcdf
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "NF90IO.h"

C     !DESCRIPTION:
C     Convenience wrappper around NF90_OPEN that
C     opens in non-parallel if usingMPI not set

      CHARACTER (LEN=*), intent(in) :: fname
      INTEGER  :: ncid, err

      IF (usingMPI) THEN
#ifdef ALLOW_USE_MPI
         err = nf90_open(fname, IOR(nf90_write, nf90_mpiio),
     $        ncid,
     $        comm=MPI_COMM_WORLD, info=MPI_INFO_NULL)
#endif
      ELSE
         err = nf90_open(fname, nf90_write,ncid)
      ENDIF

      NF90IO_OPEN = err
      END FUNCTION



CCCCCCCCCCCCCCC
      SUBROUTINE NF90IO_FILL_3Dnorec(ncid, vname, dat, myThid)

C     !DESCRIPTION:
C     Convenience function for handling all MNC and NetCDF library
C     errors.
      use netcdf
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NF90IO.h"


      character*(*) vname
      INTEGER ncid, myThid, iRec
      _RL dat (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      INTEGER start(3), count(3), rec_dimid
      INTEGER bi, bj, err, varid

      err = nf90_inq_varid(ncid, vname, varid)
      CALL nf90ERR(err, "Getting varid",myThid)
      err = nf90io_var_par_access(ncid, varid)
      CALL nf90ERR(err, "Setting varid paraccess 3D",myThid)

      count = (/  sNx, sNy, Nr /)
      do bj = 1,nSy
         do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
            start = (/ myXGlobalLo+(bi-1)*sNx,
     &           myYGlobalLo+(bj-1)*sNy,
     &           1/)
            err = nf90_put_var(ncid, varid,
     &             dat(1:sNx, 1:sNy, 1:Nr, bi, bj),
     &              start = start, count = count)
            CALL nf90ERR(err, "Putting data into file", myThid)
         enddo
      enddo
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCc

CCCCCCCCCCCCCCC
      SUBROUTINE NF90IO_FILL_4D(ncid, vname, dat, iRec, myThid)

C     !DESCRIPTION:
C     Convenience function for handling all MNC and NetCDF library
C     errors.
      use netcdf
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NF90IO.h"


      character*(*) vname
      INTEGER ncid, myThid, iRec
      _RL dat (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

      INTEGER start(4), count(4), rec_dimid
      INTEGER bi, bj, err, varid

      err = nf90_inq_varid(ncid, vname, varid)
      CALL nf90ERR(err, "Getting varid",myThid)
      err = nf90io_var_par_access(ncid, varid)
      CALL nf90ERR(err, "Setting varid paraccess 4d",myThid)

      count = (/  sNx, sNy, Nr, 1 /)
      do bj = 1,nSy
         do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
            start = (/ myXGlobalLo+(bi-1)*sNx,
     &           myYGlobalLo+(bj-1)*sNy,
     &           1,
     &           iRec /)
            err = nf90_put_var(ncid, varid,
     &             dat(1:sNx, 1:sNy, 1:Nr, bi, bj),
     &              start = start, count = count)
            CALL nf90ERR(err, "Putting data into file", myThid)
         enddo
      enddo
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCc

CCCCCCCCCCCCCCC
      SUBROUTINE NF90IO_FILL_4DNlev(ncid, vname, nlev, dat, iRec,
     $     myThid)

C     !DESCRIPTION:
C     Convenience function for handling all MNC and NetCDF library
C     errors.
      use netcdf
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NF90IO.h"

      character*(*) vname
      INTEGER ncid, myThid, nlev, iRec
      _RL dat (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nlev,nSx,nSy)

      INTEGER start(4), count(4), rec_dimid
      INTEGER bi, bj, err, varid

      err = nf90_inq_varid(ncid, vname, varid)
      CALL nf90ERR(err, "Getting varid",myThid)
      err = nf90io_var_par_access(ncid, varid)
      CALL nf90ERR(err, "Setting varid paraccess 4dN",myThid)

      count = (/  sNx, sNy, nlev, 1 /)
      do bj = 1,nSy
         do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
            start = (/ myXGlobalLo+(bi-1)*sNx,
     &           myYGlobalLo+(bj-1)*sNy,
     &           1,
     &           iRec /)
            err = nf90_put_var(ncid, varid,
     &             dat(1:sNx, 1:sNy, 1:nlev, bi, bj),
     &              start = start, count = count)
            CALL nf90ERR(err, "Putting data into file", myThid)
         enddo
      enddo
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCc


CCCCCCCCCCCCCCC
      SUBROUTINE NF90IO_FILL_3D(ncid, vname, dat, iRec, myThid)

C     !DESCRIPTION:
C     Convenience function for handling all MNC and NetCDF library
C     errors.
      use netcdf
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NF90IO.h"


      character*(*) vname
      INTEGER ncid, myThid, iRec
      _RL dat (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER start(3), count(3), rec_dimid
      INTEGER bi, bj, err, varid

      err = nf90_inq_varid(ncid, vname, varid)
      CALL nf90ERR(err, "Getting varid",myThid)
      err = nf90io_var_par_access(ncid, varid)
      CALL nf90ERR(err, "Setting varid paraccess 3d",myThid)

      count = (/  sNx, sNy, 1 /)
      do bj = 1,nSy
         do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
            start = (/ myXGlobalLo+(bi-1)*sNx,
     &           myYGlobalLo+(bj-1)*sNy,
     &           iRec /)
            err = nf90_put_var(ncid, varid,
     &             dat(1:sNx, 1:sNy, bi, bj),
     &              start = start, count = count)
            CALL nf90ERR(err, "Putting data into file", myThid)
         enddo
      enddo
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCc

      SUBROUTINE NF90IO_FILL_2Dnorec(ncid, vname, dat, myThid)

C     !DESCRIPTION:
C     Convenience function for handling all MNC and NetCDF library
C     errors.
      use netcdf
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NF90IO.h"


      character*(*) vname
      INTEGER ncid, myThid
      _RL dat (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER start(2), count(2), rec_dimid
      INTEGER bi, bj, err, varid

      err = nf90_inq_varid(ncid, vname, varid)
      CALL nf90ERR(err, "Getting varid",myThid)
      err = nf90io_var_par_access(ncid, varid)
      CALL nf90ERR(err, "Setting varid paraccess 2dnr",myThid)

      count = (/  sNx, sNy /)
      do bj = 1,nSy
         do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
            start = (/ myXGlobalLo+(bi-1)*sNx,
     &           myYGlobalLo+(bj-1)*sNy /)
            err = nf90_put_var(ncid, varid,
     &             dat(1:sNx, 1:sNy, bi, bj),
     &              start = start, count = count)
            CALL nf90ERR(err, "Putting data into file", myThid)
         enddo
      enddo
      RETURN
      END


CCCCCCCCCCCCCCC
      SUBROUTINE nf90DefineVar(ncid, varname, dimnames, attnames,
     $     attvals, varid, myThid)

C     !USES:
      use netcdf

      implicit none
#include "EEPARAMS.h"
      INTEGER  myThid, ncid
      CHARACTER(*) varname
      CHARACTER (len=250), dimension(:) :: dimnames
      CHARACTER attnames(250)*10
      CHARACTER attvals(250)*10

C     OUTPUT:
      INTEGER varid
CEOP

C     !LOCAL VARIABLES:
      integer i,lenm, dim1, dim2, dim3, dim4, err
      integer dims(4), n

      n = size(dimnames(:4))

      err = nf90_def_var(ncid, varname, NF90_INT, (/ 1 /), varid)
      CALL nf90ERR(err,"Adding i variable",myThid)
      err = nf90_put_att(ncid, varid,
     $     "standard_name", "x_grid_index")
      err = nf90_put_att(ncid, varid, "long_name",
     $     "x-dimension of the t grid")
      err = nf90_put_att(ncid, varid, "units",
     $     "")
      CALL nf90ERR(err,"Setting attributes",myThid)

      RETURN
      END

CCCCCCCCCCCCCCCC
      SUBROUTINE nf90ERR( status, msg, myThid )

C     !DESCRIPTION:
C     Convenience function for handling all MNC and NetCDF library
C     errors.

C     !USES:
      use netcdf

      implicit none
#include "EEPARAMS.h"

C     !DESCRIPTION:

C     !USES:
      INTEGER  myThid, status
      character*(*) msg
CEOP

C     !LOCAL VARIABLES:
      integer i,lenm
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

      DO i = 1,MAX_LEN_MBUF
        msgbuf(i:i) = ' '
      ENDDO

      IF ( status .NE. NF90_NOERR ) THEN
        write(msgbuf,'(2a)') 'NetCDF ERROR: '
        lenm = ILNBLNK(msgbuf)
        print *, msgbuf(1:lenm)
        CALL print_error(msgbuf(1:lenm), mythid)
        print *, '==='
        print *, NF90_STRERROR(status)
        print *, '==='
        lenm = ILNBLNK(msg)
        lenm = MIN(lenm,MAX_LEN_MBUF-11)
        write(msgbuf,'(2a)') 'NF90IO ERROR: ', msg(1:lenm)
        lenm = ILNBLNK(msgbuf)
        print *, msgbuf(1:lenm)
        CALL print_error(msgbuf(1:lenm), mythid)
        STOP 'ABNORMAL END: S/R NF90ERR'
      ENDIF
      RETURN
      END
