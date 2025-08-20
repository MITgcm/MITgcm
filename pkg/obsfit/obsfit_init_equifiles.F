#include "OBSFIT_OPTIONS.h"

CBOP
C     !ROUTINE: OBSFIT_INIT_EQUIFILES

C     !INTERFACE: 
      SUBROUTINE OBSFIT_INIT_EQUIFILES( 
     I                                  num_file,
     I                                  fid1,
     I                                  file2,
     O                                  fid2,
     I                                  length,
     I                                  bi,
     I                                  bj,
     I                                  myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Initialization of model counterparts files             
C     | for ObsFit observations 
C     ==================================================================

C !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
# include "netcdf.inc"
#endif
CEOP

C     !INPUT PARAMETERS:
C     myThid: my thread ID number
      INTEGER num_file, length, fid1, fid2
      CHARACTER*(*) file2
      INTEGER bi, bj, myThid

#ifdef ALLOW_OBSFIT
C     !LOCAL VARIABLES:
      INTEGER dimid, varid(3), varid0
      INTEGER irec, err
      _RL tmp_vec(2)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (obsfitDoNcOutput) THEN
      
C Create a netcdf file
        err = NF_CREATE( file2, NF_CLOBBER, fid2 )
        CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_CREATE fid2',
     &       err,bi,bj,myThid )
        err = NF_DEF_DIM( fid2, 'iSAMPLE', length, dimid )
        CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_DEF_DIM sample',
     &       err,bi,bj,myThid )
        err = NF_DEF_VAR( fid2, 'sample_ind_glob', 
     &        NF_INT, 1, dimid, varid(1))
        CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_DEF_VAR sample',
     &       err,bi,bj,myThid )
        err = NF_PUT_ATT_INT( fid2, varid(1), '_FillValue',
     &        NF_INT, 1, -9999 )
        CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_PUT_ATT_INT sample',
     &       err,bi,bj,myThid )

        err = NF_INQ_VARID( fid1, obsfit_nameval, varid0 )
        IF (err.EQ.NF_NOERR) THEN

          err = NF_DEF_VAR(fid2,obsfit_nameequi,
     &          NF_DOUBLE,1,dimid,varid(2))
          CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_DEF_VAR val',
     &         err,bi,bj,myThid )
          err = NF_PUT_ATT_DOUBLE(fid2,varid(2),
     &          '_FillValue',NF_DOUBLE,1,-9999. _d 0)
          CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_PUT_ATT_INT val',
     &         err,bi,bj,myThid )

          err = NF_DEF_VAR(fid2,obsfit_namemask,
     &          NF_DOUBLE,1,dimid,varid(3))
          CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_DEF_VAR mask',
     &         err,bi,bj,myThid )
          err = NF_PUT_ATT_DOUBLE(fid2,varid(3),
     &          '_FillValue',NF_DOUBLE,1,-9999. _d 0)
          CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILE: NF_PUT_ATT_INT mask',
     &         err,bi,bj,myThid )

        ELSE
            WRITE(msgBuf,'(2A)')
     &          'S/R OBSFIT_INIT_EQUIFILES: no variable obs_val'
            CALL PRINT_MESSAGE( msgBuf,
     &           standardMessageUnit, SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(2A)')
     &           'INIT_EQUIFILES: NF_INQ_VARID obs_val'
            CALL OBSFIT_NF_ERROR( msgBuf, err, bi, bj, myThid )

        ENDIF

        err = NF_ENDDEF( fid2 )
        CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILES: NF_ENDDEF fid2',
     &       err,bi,bj,myThid )
        err = NF_CLOSE( fid2 )
        CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILES: NF_CLOSE fid2',
     &       err,bi,bj,myThid )

        err = NF_OPEN(file2,NF_WRITE,fid2)
        CALL OBSFIT_NF_ERROR( 'INIT_EQUIFILES: NF_OPEN'//file2//')',
     &       err,bi,bj,myThid )

      ELSE !IF (obsfitDoNcOutput)

C Create a binary file
        OPEN( fid2, FILE=file2, FORM='unformatted', STATUS='unknown',
     &   ACCESS='direct', RECL=2*WORDLENGTH*2 )

        DO irec = 1, 2
          tmp_vec(irec) = zeroRL
        ENDDO
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(2,tmp_vec)
#endif

        DO irec = length, 1, -1
          WRITE( fid2, rec=(irec*2-1) ) tmp_vec
          WRITE( fid2, rec=(irec*2) ) tmp_vec
        ENDDO

      ENDIF !IF (obsfitDoNcOutput)

#endif

      END

