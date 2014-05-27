C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_readparms.F,v 1.35 2014/05/27 23:41:27 jmc Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C--  File mnc_readparms.F
C--   Contents
C--   o MNC_READPARMS
C--   o MNC_SET_OUTDIR

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_READPARMS

C     !INTERFACE:
      SUBROUTINE MNC_READPARMS( myThid )

C     !DESCRIPTION:
C     Read the MNC run-time parameters file.  IF the file does not
C     exist, MNC will assume that it is not needed (that is, some other
C     IO routines such as MDSIO will be used) and will not issue any
C     errors.

C     !USES:
      implicit none
#include "SIZE.h"
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MNC_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER i, nl, ku
      CHARACTER*(MAX_LEN_MBUF) data_file
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      NAMELIST /MNC_01/
     &     mnc_use_indir, mnc_use_outdir, mnc_outdir_date,
     &     mnc_outdir_num, mnc_use_name_ni0, mnc_echo_gvtypes,
     &     pickup_write_mnc, pickup_read_mnc,
     &     timeave_mnc, snapshot_mnc, monitor_mnc, autodiff_mnc,
     &     writegrid_mnc, readgrid_mnc,
     &     mnc_outdir_str, mnc_indir_str, mnc_max_fsize,
     &     mnc_filefreq,
     &     mnc_read_bathy, mnc_read_salt, mnc_read_theta

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useMNC ) THEN
C-    pkg MNC is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.mnc is found
         CALL PACKAGES_UNUSED_MSG( 'useMNC', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C-----
C     Need some work to make MNC multi-threaded safe.
C     For now, switch it off (otherwise, it is hanging up somewhere)
      IF ( nThreads.GT.1 ) THEN
        _BARRIER
        _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(2A)') '**WARNING** MNC_READPARMS: ',
     &                       'useMNC unsafe with multi-threads'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(2A)') '**WARNING** MNC_READPARMS: ',
     &                       'for now, switch useMNC to FALSE'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        useMNC = .FALSE.
        _END_MASTER( myThid )
        _BARRIER
        RETURN
      ENDIF
C-----

C     Set default values for MNC run-time parameters
      DO i = 1,MAX_LEN_FNAM
        mnc_outdir_str(i:i) = ' '
        mnc_indir_str(i:i)  = ' '
      ENDDO
      mnc_echo_gvtypes      =  .FALSE.
      mnc_use_outdir        =  .FALSE.
        mnc_outdir_str(1:4) =  'mnc_'
        mnc_outdir_date     =  .FALSE.
        mnc_outdir_num      =  .TRUE.
      mnc_use_name_ni0      =  .FALSE.
      pickup_write_mnc      =  .TRUE.
      pickup_read_mnc       =  .TRUE.
      mnc_use_indir         =  .FALSE.
        mnc_indir_str(1:4)  =  '    '
      monitor_mnc           =  .TRUE.
      timeave_mnc           =  .TRUE.
      snapshot_mnc          =  .TRUE.
      autodiff_mnc          =  .TRUE.
      writegrid_mnc         =  .TRUE.
C     2GB is 2147483648 bytes or approx: 2.1475e+09
      mnc_max_fsize         =  2.1 _d 9
      readgrid_mnc          =  .FALSE.

C     New parms for initial files
      mnc_read_bathy        =  .FALSE.
      mnc_read_salt         =  .FALSE.
      mnc_read_theta        =  .FALSE.

C     Temporary hack for Baylor
      mnc_filefreq          =  -1

C     Set the file name
      DO i=1,MAX_LEN_MBUF
        data_file(i:i) = ' '
      ENDDO
      WRITE(data_file,'(a)') 'data.mnc'
      nl = ILNBLNK(data_file)

      WRITE(msgbuf,'(3a)') ' MNC_READPARMS: opening file ''',
     &       data_file(1:nl), ''''
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &     SQUEEZE_RIGHT,myThid)

      CALL OPEN_COPY_DATA_FILE(data_file(1:nl),'MNC_READPARMS',
     &     ku, myThid )
      READ(ku,NML=MNC_01)
      CLOSE(ku)

      WRITE(msgBuf,'(a)') ' MNC_READPARMS: finished reading data.mnc'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &     SQUEEZE_RIGHT,myThid)

C     Pickups must always be read in an EXCLUSIVE fashion
      IF (pickup_read_mnc)  pickup_read_mdsio = .FALSE.

C     IO handling is done in one of two senses:
C     (1) outputTypesInclusive=.TRUE. is an "inclusive-or" meaning that
C         one or more write methods can occur simultaneously or
C     (2) outputTypesInclusive=.FALSE. is an "exclusive-or" meaning that
C         only one write method can occur in a given run
C
C     Since all the *_mdsio flags default to .TRUE. and
C     outputTypesInclusive defaults to .FALSE., the logic here is
C     simple:
      IF ( (.NOT. outputTypesInclusive)
     &     .AND. pickup_write_mnc ) pickup_write_mdsio = .FALSE.
      IF ( (.NOT. outputTypesInclusive)
     &     .AND. timeave_mnc ) timeave_mdsio = .FALSE.
      IF ( (.NOT. outputTypesInclusive)
     &     .AND. snapshot_mnc ) snapshot_mdsio = .FALSE.
      IF ( (.NOT. outputTypesInclusive)
     &     .AND. monitor_mnc ) monitor_stdio = .FALSE.

C     Reads are always an exclusive relationship
      IF (pickup_read_mnc) pickup_read_mdsio = .FALSE.

C     Create and/or set the MNC output directory
      IF (mnc_use_outdir) THEN
        IF ( mnc_outdir_num .OR. mnc_outdir_date ) THEN
          CALL MNC_SET_OUTDIR(myThid)
        ELSE
          DO i = 1,MNC_MAX_CHAR
            mnc_out_path(i:i) = ' '
          ENDDO
          write(mnc_out_path,'(2A)')
     &         mnc_outdir_str(1:ILNBLNK(mnc_outdir_str)), '/'
        ENDIF
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_SET_OUTDIR

C     !INTERFACE:
      SUBROUTINE MNC_SET_OUTDIR( myThid )

C     !DESCRIPTION:
C     Create the output (sub--)directory for the MNC output files.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MNC_PARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i,j,k, ntot, npathd, idate
      character*(MNC_MAX_PATH) pathd
      character*(100) cenc
      integer ienc(MNC_MAX_PATH)
      integer ncenc

C     Functions
      integer ILNBLNK

      cenc(1:26)  = 'abcdefghijklmnopqrstuvwxyz'
      cenc(27:52) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      cenc(53:70) = '0123456789_.,+-=/~'
      ncenc = 70
      npathd = 100
      IF (mnc_outdir_date) THEN
        idate = 1
      ELSE
        idate = 0
      ENDIF
      DO i = 1,MNC_MAX_PATH
        pathd(i:i) = ' '
      ENDDO
      k = ILNBLNK(mnc_outdir_str)
      IF (k .GT. MNC_MAX_PATH)  k = MNC_MAX_PATH
      pathd(1:k) = mnc_outdir_str(1:k)
      ntot = 0
      DO i = 1,k
        DO j = 1,ncenc
          IF (pathd(i:i) .EQ. cenc(j:j)) THEN
            ntot = ntot + 1
            ienc(ntot) = j
            GOTO 20
          ENDIF
        ENDDO
 20     CONTINUE
      ENDDO

      CALL mnccdir(ntot, ienc, idate)

      DO i = 1,MNC_MAX_PATH
        mnc_out_path(i:i) = ' '
      ENDDO
      IF (ntot .GT. 0) THEN
        IF (ntot .GT. (MNC_MAX_PATH-40)) THEN
          ntot = MNC_MAX_PATH - 40
        ENDIF
        DO i = 1,ntot
          j = ienc(i)
          mnc_out_path(i:i) = cenc(j:j)
        ENDDO
        mnc_out_path((ntot+1):(ntot+1)) = '/'
      ENDIF

      RETURN
      END
