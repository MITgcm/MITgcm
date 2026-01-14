#include "OBSFIT_OPTIONS.h"
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: OBSFIT_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE OBSFIT_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Initialize ObsFit variables that are kept fixed during the run
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_CAL
#include "cal.h"
#endif
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     myThid :: my thread ID number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBSFIT
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN

C     !LOCAL VARIABLES:
C     i,j,bi,bj    :: loop indices
C     iG,jG        :: global indices
C     diffsecs     :: absolute time
      INTEGER i,j,k,l,bi,bj,iG,jG
      INTEGER obs_num, num_file, sampleNo_tile
      INTEGER ic, ip, iq, kLev, iINTERP
      INTEGER chunkObs, chunk
      INTEGER iUnit
      INTEGER stopObsfit, stopGenericGrid
      INTEGER loopThroughSamples, weighSamples, readDelT
      INTEGER obsIsInRunTime, sampleIsInTile, sampleIsValid
      INTEGER fid, dimid, varID(2)
      INTEGER varID0, varID1a, varID1b, varID1c
      INTEGER varID2, varID3a, varID3b, varID4, varID5
      INTEGER varID6, varID7
      INTEGER varID_intp1, varID_intp2, varID_intp11, varID_intp22
      INTEGER varID_intp3, varID_intp4, varID_intp5, varID_intp6
      INTEGER tmpdate(4), tmpdiff(4)
      INTEGER tmp_type2(1000), type_cur
      INTEGER sample_i, sample_j, sample_k1, sample_k2
      INTEGER np_cur
      INTEGER ObsNo_valid, ObsNo_div1000
      INTEGER sample_cnt
      INTEGER hh, obsNo_hh, obs_np_max
      INTEGER ntype_ssh,ntype_other, obs_is_ssh_loc
      INTEGER vec_start, vec_count
      INTEGER vec_start2(2), vec_count2(2)
      INTEGER IL, JL, err, nerr
      _RL tmpyymmdd(1000),tmphhmmss(1000),diffsecs
      _RL yymmddMin,yymmddMax
      _RL hhmmssMin,hhmmssMax
      _RL tmpnp(1000), tmpdelT(1000)
      _RL tmp_lon, tmp_lon2(1000), tmp_lat2(1000), lon_cur, lat_cur
      _RL tmp_depth2(1000), depth_cur, delT_cur
      _RL tmp_weight2(1000)
      _RL tmp_sample_time(NSAMPLES_MAX_GLO)
      _RL tmp_sample_delT(NSAMPLES_MAX_GLO)
      _RL lon_1, lon_2, lat_1, lat_2
      _RL depth_1, depth_2
      _RL lon_tmp1, lon_tmp2, lat_tmp1, lat_tmp2
      _RL lat_fac, lon_fac, depth_fac
      _RL mask(NUM_INTERP_PTS_OBS)
      _RL w(NUM_INTERP_PTS_OBS), wtot
      _RL tmp_i(1000,NUM_INTERP_PTS_OBS)
      _RL tmp_j(1000,NUM_INTERP_PTS_OBS)
      _RL tmp_k(1000,NUM_INTERP_PTS_OBS)
      _RL tmp_frac(1000,NUM_INTERP_PTS_OBS),tmp_sum_fracs
      _RL tmp_xC11(1000),tmp_yC11(1000)
      _RL tmp_xCNINJ(1000),tmp_yCNINJ(1000)
c     _RL xy_buffer_r8(0:sNx+1,0:sNy+1)
      CHARACTER*(MAX_LEN_FNAM) obsfitfile, fnamedatanc
      CHARACTER*(MAX_LEN_FNAM) obsfnameequinc, obsfnameequincglo
      CHARACTER*(MAX_LEN_FNAM) fnamemisfit
# ifdef ALLOW_ADJOINT_RUN
      CHARACTER*(MAX_LEN_FNAM) adobsfnameequinc, adobsfnameequincglo
# endif
# ifdef ALLOW_TANGENTLINEAR_RUN
      CHARACTER*(MAX_LEN_FNAM) tlobsfnameequinc, tlobsfnameequincglo
# endif
      CHARACTER*(MAX_LEN_MBUF) msgbuf
      LOGICAL exst

      iUnit = standardMessageUnit
      WRITE( msgbuf,'(A)' ) ' '
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A)' )
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A)' )
     &     '// insitu obsfit model sampling >>> START <<<'
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A)' )
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgbuf,'(A)' ) ' '
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

      stopObsfit=0
      stopGenericGrid=0

      IF ( (.NOT.obsfitDoGenGrid) .AND.
     &     (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
       WRITE( msgBuf,'(2A)' ) 'OBSFIT_INIT_FIXED: ',
     &      'obsfitDoGenGrid=.true. is required'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE( msgBuf,'(2A)' ) 'OBSFIT_INIT_FIXED: ',
     &      'unless usingSphericalGrid=.TRUE. and rotateGrid=.FALSE.'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R OBSFIT_INIT_FIXED'
      ENDIF

c      IF ( obsfitDoGenGrid ) THEN
c       WRITE(msgBuf,'(2A)') 'OBSFIT_INIT_FIXED: ',
c     &  'obsfitDoGenGrid has not been tested yet'
c       CALL PRINT_ERROR( msgBuf, myThid )
c       CALL ALL_PROC_DIE( myThid )
c       STOP 'ABNORMAL END: S/R OBSFIT_INIT_FIXED'
c      ENDIF

      WRITE( msgbuf,'(A)' ) ' '
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A)' ) 'general packages parameters :'

      JL  = ILNBLNK( obsfitDir )
      IF (JL.NE.0) THEN
       WRITE( msgbuf,'(2A)' ) '  obsfitDir ',obsfitDir(1:JL)
      ELSE
       WRITE( msgbuf,'(2A)' ) '  obsfitDir ','./'
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A,l5)' ) '  obsfitDoGenGrid  ',obsfitDoGenGrid
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A,l5)' ) '  obsfitDoNcOutput ',obsfitDoNcOutput
      CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A)' ) ' '
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

      _BEGIN_MASTER( myThid )

C Initialize global variables for obsfit_read_obs.F
      obsfit_curfile_buff = 0
      DO l = 1, 1000
       obsfit_data_buff(l) = 0. _d 0
       obsfit_uncert_buff(l) = 0. _d 0
      ENDDO

      yymmddMin=modelstartdate(1)
      yymmddMax=modelenddate(1)
      hhmmssMin=modelstartdate(2)
      hhmmssMax=modelenddate(2)

      DO num_file = 1, NFILESMAX_OBS

       ObsNo_hh=0
       obs_np_max=0
       obs_is_ssh_loc=0
       obs_is_ssh(num_file)=0

       obsfitFile=' '
       IL = ILNBLNK( obsfitFiles(num_file) )
       IF (IL.NE.0) THEN
        WRITE( obsfitFile,'(1A)' ) obsfitFiles(num_file)(1:IL)
        WRITE( msgbuf,'(A)' ) ' '
        CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
        WRITE( msgbuf,'(A,I3,2A)' ) 'obsfit file #', num_file,
     &       ' is ', obsfitFile(1:IL)
        CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
       ENDIF

       IL = ILNBLNK( obsfitFile )
       IF (IL.NE.0) THEN

C===========================================================
C Open input files and read information
C===========================================================

        WRITE( fnamedatanc,'(2A)' ) obsfitFile(1:IL),'.nc'
        err = NF_OPEN( fnamedatanc, NF_NOWRITE, fiddata_obs(num_file) )
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_OPEN fiddata',err,0,0,myThid )

C Read number of observations and available dimensions:
        fid = fiddata_obs(num_file)

        err = NF_INQ_DIMID( fid,'iOBS', dimid )
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID iOBS',err,0,0,myThid )
        err = NF_INQ_DIMLEN( fid, dimid, obsNo(num_file) )
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMLEN ObsNo',err,0,0,myThid )

        WRITE(msgbuf,'(A,I9)')
     &       '  # of observations in file                   =',
     &       obsNo(num_file)
        CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

C Read number of interpolation points (generic grid only):
        err = NF_INQ_DIMID(fid,'iINTERP', dimid)
        IF (err.EQ.NF_NOERR) THEN
         err = NF_INQ_DIMLEN( fid, dimid, iINTERP )
        ELSE

         IF ( debugLevel .GE. debLevA ) THEN
          WRITE( msgBuf,'(3A,I3)' )
     &         'S/R OBSFIT_INIT_FIXED: ',
     &         'no iINTERP dim in data file using iINTERP ',
     &         '= NUM_INTERP_POINTS =', NUM_INTERP_PTS_OBS
          CALL PRINT_MESSAGE( msgBuf, iUnit, SQUEEZE_RIGHT, myThid )
         ENDIF

         iINTERP = NUM_INTERP_PTS_OBS

        ENDIF!if (err.EQ.NF_NOERR)

        WRITE( msgbuf,'(A,I9)' )
     &       '  # of interpolation points                   =',
     &       iINTERP
        CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

C Read number of samples (if missing, assume it's 1):
        loopThroughSamples = 1
        err = NF_INQ_VARID( fid, 'obs_np', varID0 )
        IF (err.NE.NF_NOERR) THEN
         loopThroughSamples = 0

         WRITE( msgbuf,'(2A)' )
     &        '      input obsfit file does not have NP ',
     &        '      (num. samples); assume NP=1 for all obs '
         CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

        ENDIF

C Read the info for observations:
        err = NF_INQ_VARID( fid, 'obs_YYYYMMDD', varID1a )
        nerr = err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID obs_YYYYMMDD',err,0,0,myThid )
        err = NF_INQ_VARID( fid, 'obs_HHMMSS', varID1b )
        nerr = nerr + err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID obs_HHMMSS',err,0,0,myThid )
        err = NF_INQ_VARID( fid, 'obs_val', varID6 )
        nerr = nerr + err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID obs_val',err,0,0,myThid )
        err = NF_INQ_VARID( fid, 'obs_uncert', varID7 )
        nerr = nerr + err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID obs_uncert',err,0,0,myThid )

        IF (nerr.NE.NF_NOERR) THEN
         IL  = ILNBLNK( obsfitFile )
         WRITE( msgBuf,'(3A)' )
     &        'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &        '.nc is not in the pkg/obsfit format'
         CALL PRINT_ERROR( msgBuf, myThid )
         stopObsfit = 1
        ENDIF

        readDelT = 1
        err = NF_INQ_VARID( fid,'obs_delT', varID1c )
C Time interval (if missing, assume instantaneous):
        IF (err.NE.NF_NOERR) THEN
         readDelT = 0
         WRITE( msgbuf,'(2A)' )
     &        '      input obsfit file does not have time interval;',
     &        '      assume observations are instantaneous '
         CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

C Read the info for samples:
        err = NF_INQ_VARID( fid,'sample_type',varID2 )
        nerr = err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID sample_type',err,0,0,myThid )
        err = NF_INQ_VARID( fid,'sample_lon', varID3a )
        nerr = err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID sample_lon',err,0,0,myThid )
        err = NF_INQ_VARID( fid,'sample_lat', varID3b )
        nerr = err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID sample_lat',err,0,0,myThid )
        err = NF_INQ_VARID( fid,'sample_depth', varID4 )
        nerr = err
        CALL OBSFIT_NF_ERROR(
     &       'INIT_FIXED: NF_INQ_DIMID sample_depth',err,0,0,myThid )

        IF (err.NE.NF_NOERR) THEN
         IL  = ILNBLNK( obsfitFile )
         WRITE( msgBuf,'(3A)' )
     &        'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &        '.nc is not in the pkg/obsfit format'
         CALL PRINT_ERROR( msgBuf, myThid )
         stopObsfit=1
        ENDIF

        weighSamples = 1
        err = NF_INQ_VARID( fid,'sample_weight', varID5 )
C Weight for each sample (if missing, assume all samples weigh equally):
        IF (err.NE.NF_NOERR) THEN
         weighSamples = 0
         WRITE( msgbuf,'(2A)' )
     &        '      input obsfit file does not have sample weight;',
     &        '      assume all samples are weighed evenly '
         CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

        IF (obsfitDoGenGrid) THEN
C Read interpolation information (grid points, fractions, etc.)
         err = NF_INQ_VARID(fid,'sample_interp_XC11',varID_intp1)
         err = NF_INQ_VARID(fid,'sample_interp_YC11',varID_intp2)
         err = NF_INQ_VARID(fid,'sample_interp_XCNINJ',varID_intp11)
         err = NF_INQ_VARID(fid,'sample_interp_YCNINJ',varID_intp22)
         err = NF_INQ_VARID(fid,'sample_interp_frac',varID_intp3)
         err = NF_INQ_VARID(fid,'sample_interp_i',varID_intp4)
         err = NF_INQ_VARID(fid,'sample_interp_j',varID_intp5)
         err = NF_INQ_VARID(fid,'sample_interp_k',varID_intp6)
         IF (err.NE.NF_NOERR) THEN
          IL  = ILNBLNK( obsfitFile )
          WRITE( msgBuf,'(3A)' )
     &         'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL), '.nc is',
     &         ' missing interpolation information (obsfitDoGenGrid)'
          CALL PRINT_ERROR( msgBuf, myThid )
          stopGenericGrid=2
         ENDIF
        ENDIF

C Variables names for equi files
        obsfit_nameequi = 'mod_val'
        obsfit_namemask = 'mod_mask'
        obsfit_nameval = 'obs_val'
        obsfit_nameuncert = 'obs_uncert'

C Create global files for obs-equivalent
        IF ( myProcId .EQ. 0 ) THEN

         WRITE( obsfnameequincglo,'(3A)' )
     &        obsfitDir(1:JL),obsfitFile(1:IL),'.equi.nc'
# ifdef ALLOW_ADJOINT_RUN
         WRITE( adobsfnameequincglo,'(4A)' )
     &        obsfitDir(1:JL),'ad',
     &        obsfitFile(1:IL),'.equi.nc'
# endif /* ALLOW_ADJOINT_RUN */
# ifdef ALLOW_TANGENTLINEAR_RUN
         WRITE( tlobsfnameequincglo,'(4A)' )
     &        obsfitDir(1:JL),'tl',
     &        obsfitFile(1:IL),'.equi.nc'
# endif /* ALLOW_TANGENTLINEAR_RUN */

         JL = ILNBLNK( obsfitDir )
         INQUIRE( file=obsfnameequincglo, exist=exst )
         IF (.NOT.exst) THEN
          err = NF_CREATE( obsfnameequincglo, NF_CLOBBER,
     &          fidglobal(num_file) )
          err = NF_DEF_DIM( fidglobal(num_file), 'iOBS',
     &          ObsNo(num_file), dimid )
          err = NF_DEF_VAR( fidglobal(num_file),
     &          obsfit_nameequi,NF_DOUBLE,1,dimid,varID(1) )
          err = NF_PUT_ATT_DOUBLE( fidglobal(num_file),
     &          varID(1),'_FillValue',NF_DOUBLE,1, -9999. _d 0 )
          err = NF_DEF_VAR( fidglobal(num_file),
     &          obsfit_namemask,NF_DOUBLE,1,dimid,varID(2) )
          err = NF_PUT_ATT_DOUBLE( fidglobal(num_file),
     &          varID(2),'_FillValue',NF_DOUBLE,1, -9999. _d 0 )
          err = NF_ENDDEF( fidglobal(num_file) )
          err = NF_CLOSE( fidglobal(num_file) )
          err = NF_OPEN( obsfnameequincglo, NF_WRITE,
     &          fidglobal(num_file) )
         ELSE
          err = NF_OPEN( obsfnameequincglo, NF_WRITE,
     &          fidglobal(num_file) )
          err = NF_INQ_VARID( fidglobal(num_file), obsfit_namemask,
     &          varID(2) )

         ENDIF
C Initialize mask values to 0
         DO obs_num = 1, ObsNo(num_file)
          err = NF_PUT_VARA_DOUBLE( fidglobal(num_file), varID(2),
     &          obs_num, 1, 0. _d 0 )
          CALL OBSFIT_NF_ERROR(
     &         'INIT_FIXED: NF_PUT_VARA_DOUBLE zero',err,0,0,myThid )
         ENDDO

#ifdef ALLOW_ADJOINT_RUN
         INQUIRE( file=adobsfnameequincglo, exist=exst )
         IF (.NOT.exst) THEN
          err = NF_CREATE( adobsfnameequincglo,NF_CLOBBER,
     &          fidadglobal(num_file) )
          err = NF_DEF_DIM( fidadglobal(num_file),'iOBS',
     &          ObsNo(num_file),dimid )
          err = NF_DEF_VAR( fidadglobal(num_file),
     &          'mod_val',NF_DOUBLE,1,dimid,varID(1) )
          err = NF_PUT_ATT_DOUBLE( fidadglobal(num_file),
     &          varID(1),'_FillValue', NF_DOUBLE,1, 0. _d 0 )
          err = NF_DEF_VAR( fidadglobal(num_file),
     &          'mod_mask',NF_DOUBLE, 1,dimid,varID(2) )
          err = NF_PUT_ATT_DOUBLE( fidadglobal(num_file),
     &          varID(2),'_FillValue',NF_DOUBLE,1, 0. _d 0 )
          err = NF_ENDDEF( fidadglobal(num_file) )
          err = NF_CLOSE( fidadglobal(num_file) )
          err = NF_OPEN( adobsfnameequincglo, NF_WRITE,
     &          fidadglobal(num_file) )
         ELSE
          err = NF_OPEN( adobsfnameequincglo, NF_WRITE,
     &          fidadglobal(num_file) )
         ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
         INQUIRE( file=tlobsfnameequincglo, exist=exst )
         IF (.NOT.exst) THEN
          err = NF_CREATE( tlobsfnameequincglo,NF_CLOBBER,
     &          fidtanglobal(num_file) )
          err = NF_DEF_DIM( fidtanglobal(num_file),'iOBS',
     &          ObsNo(num_file),dimid )
          err = NF_DEF_VAR( fidtanglobal(num_file),
     &          'mod_val',NF_DOUBLE,1,dimid,varID(1) )
          err = NF_PUT_ATT_DOUBLE( fidtanglobal(num_file),
     &          varID(1),'_FillValue',NF_DOUBLE,1, 0. _d 0 )
          err = NF_DEF_VAR( fidtanglobal(num_file),
     &          'mod_mask',NF_DOUBLE,1,dimid,varID(2))
          err = NF_PUT_ATT_DOUBLE( fidtanglobal(num_file),
     &          varID(2),'_FillValue',NF_DOUBLE,1, 0. _d 0 )
          err = NF_ENDDEF( fidtanglobal(num_file) )
          err = NF_CLOSE( fidtanglobal(num_file) )
          err = NF_OPEN( tlobsfnameequincglo, NF_WRITE,
     &          fidtanglobal(num_file) )
         ELSE
          err = NF_OPEN( tlobsfnameequincglo, NF_WRITE,
     &          fidtanglobal(num_file) )
         ENDIF
#endif

C Global file for misfits
         WRITE( fnamemisfit,'(3A)' )
     &        obsfitDir(1:JL),obsfitfile(1:IL),'.misfit.nc'
         INQUIRE( file=fnamemisfit, exist=exst )
         IF (.NOT.exst) THEN
          err = NF_CREATE( fnamemisfit,NF_CLOBBER,fidmisfit(num_file) )
          err = NF_DEF_DIM( fidmisfit(num_file),'iOBS',ObsNo(num_file),
     &          dimid )
          err = NF_DEF_VAR( fidmisfit(num_file),'misfit',
     &          NF_DOUBLE,1,dimid,varID(1) )
          err = NF_PUT_ATT_DOUBLE( fidmisfit(num_file),varID(1),
     &          '_FillValue',NF_DOUBLE,1,-9999. _d 0 )
          err = NF_ENDDEF( fidmisfit(num_file) )
          err = NF_CLOSE( fidmisfit(num_file) )
         ELSE
          err = NF_OPEN( fnamemisfit, NF_WRITE,fidmisfit(num_file) )
         ENDIF

        ENDIF !myProcId

C===========================================================
C First: loop through observations
C===========================================================
C Default values
        DO obs_num = 1, NOBSMAX_OBS
         obs_np(num_file,obs_num) = 1
         obs_ind_glob(num_file,obs_num) = 0
        ENDDO

C Main loop: look for valid obs (within model run time)
        ObsNo_valid = 0
        Obsno_div1000 = max(0,int(ObsNo(num_file)/1000))

C Counter tracking position of first sample number for each obs
        sample_cnt = 1

        DO chunkObs = 1, Obsno_div1000+1
         chunk = 1000*(chunkObs-1)

         IF ( MIN(ObsNo(num_file), 1000*chunkObs ).GE.
     &      1+chunk ) THEN

C Read a chunk
          vec_start = 1+chunk
          vec_count = MIN(1000,ObsNo(num_file)-chunk)

          IF ( (vec_count.LE.0) .OR.
     &         (vec_count.GT.1000) .OR.
     &         (vec_start.LE.0) .OR.
     &         (vec_count+vec_start-1.GT.ObsNo(num_file)) )
     &    THEN
           IL  = ILNBLNK( obsfitFile )
           WRITE( msgBuf,'(3A)' )
     &          'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &          '.nc was not read properly (case 1).'
           CALL PRINT_ERROR( msgBuf, myThid )
           stopObsfit = 1
          ENDIF

C Read time
          err = NF_GET_VARA_DOUBLE( fid,varID1a,vec_start,
     &          vec_count, tmpyymmdd )
          err = NF_GET_VARA_DOUBLE( fid,varID1b,vec_start,
     &          vec_count, tmphhmmss )
C Read time interval
          IF ( readDelT.GT.0 ) THEN
           err = NF_GET_VARA_DOUBLE( fid,varID1c,vec_start,
     &           vec_count, tmpdelT )
          ENDIF
C Read number of samples per obs
          IF ( loopThroughSamples.EQ.1 ) THEN
           err = NF_GET_VARA_DOUBLE( fid,varID0,vec_start,
     &           vec_count, tmpnp )
          ENDIF

          IF (err.NE.NF_NOERR) THEN
           WRITE( msgBuf,'(3A)' )
     &          'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &          '.nc was not read properly (case 2).'
           CALL PRINT_ERROR( msgBuf, myThid )
           stopObsfit = 1
          ENDIF

C Loop through this chunk
          DO ic = 1, MIN(1000,ObsNo(num_file)-chunk)

           IF ( stopObsfit .EQ. 0 ) THEN
            obsIsInRunTime = 1

CAV if obs starts before model run, will be ignored even if it ends after
CAV model starts - ok for now
            IF (( ( tmpyymmdd(ic).GT.yymmddMin ) .OR.
     &           (( tmpyymmdd(ic).EQ.yymmddMin ) .AND.
     &            ( tmphhmmss(ic).GE.hhmmssMin )) ) .AND.
     &          ( ( tmpyymmdd(ic).LT.yymmddMax ) .OR.
     &           (( tmpyymmdd(ic).EQ.yymmddMax ) .AND.
     &            ( tmphhmmss(ic).LE.hhmmssMax )) )) THEN
             hh = INT(tmphhmmss(ic))/10000
             IF ( hh.LT.hoursPerDay ) THEN
              obsIsInRunTime = 1
              CALL CAL_FULLDATE( INT(tmpyymmdd(ic)),
     &             INT(tmphhmmss(ic)),tmpdate,myThid )
              CALL CAL_TIMEPASSED( modelstartdate,tmpdate,
     &             tmpdiff,myThid )
              CALL CAL_TOSECONDS( tmpdiff,diffsecs,myThid )
              diffsecs = diffsecs+nIter0*deltaTClock
             ELSE
C If tmphhmmss is out of range then disregard obs
              obsIsInRunTime = 0
              diffsecs = -deltaTClock
              ObsNo_hh = ObsNo_hh+1
             ENDIF
            ELSE
             obsIsInRunTime = 0
             diffsecs = -deltaTClock
            ENDIF

            IF ( loopThroughSamples.EQ.1 ) THEN
             np_cur = tmpnp(ic)
            ELSE
             np_cur = 1
            ENDIF

            IF ( readDelT.EQ.1 ) THEN
             IF ( tmpdelT(ic).GT.0. _d 0 ) THEN
C delT>0: time average
              delT_cur = tmpdelT(ic)
              obsfitOperation(num_file) = 1
             ELSEIF (tmpdelT(ic).LT.0. _d 0) THEN
C delT<0: time integral
              delT_cur = -tmpdelT(ic)
              obsfitOperation(num_file) = 2
             ELSE
C delT=0: instantaneous
              delT_cur = 0. _d 0
              obsfitOperation(num_file) = 0
             ENDIF
            ELSE
             delT_cur = 0. _d 0
             obsfitOperation(num_file) = 0
            ENDIF

            IF ( obsIsInRunTime.EQ.1 ) THEN
C If yes then store obs index position and number of samples:
             ObsNo_valid = ObsNo_valid+1
             obs_ind_glob(num_file,ObsNo_valid) = ic+chunk
C Number of samples in each valid observation
             obs_np(num_file,ObsNo_valid) = np_cur
             IF ( np_cur.GT.obs_np_max ) obs_np_max = np_cur
C Position of first sample
             obs_sample1_ind(num_file,ObsNo_valid) = sample_cnt
             obs_delT(num_file,ObsNo_valid) = delT_cur
            ENDIF !obsIsInRunTime.EQ.1

C Regardless of whether obs is in run time or not,
C keep track of sample index position
C and assign time to samples
            DO k = 1, np_cur
             tmp_sample_time(sample_cnt) = diffsecs
             tmp_sample_delT(sample_cnt) = delT_cur
             sample_cnt = sample_cnt+1
            ENDDO

C Check that maximum size was not reached:
            IF ( ObsNo_valid.GT.NOBSMAX_OBS ) THEN
             WRITE( msgBuf,'(3A)' )
     &            'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &            '.nc was not read properly (increase NOBSMAX_OBS).'
             CALL PRINT_ERROR( msgBuf, myThid )
             stopObsfit = 1
            ENDIF

C Check that maximum NP was not reached:
            IF ( obs_np_max.GT.NSAMP_PER_OBS_MAX ) THEN
             WRITE( msgBuf,'(3A)' )
     &           'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &           '.nc  was not read properly ',
     &           '(increase NSAMP_PER_OBS_MAX).'
             CALL PRINT_ERROR( msgBuf, myThid )
             stopObsfit = 1
            ENDIF

           ENDIF !stopObsfit
          ENDDO !ic
         ENDIF !chunk
        ENDDO !chunkObs

C Number of valid observations in the file
        ObsNo(num_file) = ObsNo_valid

        WRITE( msgbuf,'(A,I9)' )
     &       '  # of obs with erroneous HHMMSS values =', ObsNo_hh
        CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

        WRITE(msgbuf,'(A,I9)')
     &       '  # of obs within time period  =', ObsNo(num_file)
        CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

C===========================================================
C Second: loop through samples
C===========================================================
        DO bj = 1, nSy
         DO bi = 1, nSx

C Main loop: look for valid samples (in tile)
          err = NF_INQ_DIMID( fid,'iSAMPLE', dimid )
C If isample is not a dimension, use iobs for size of sampleno
          IF (err.NE.NF_NOERR) THEN
           sampleNo(num_file,bi,bj) = ObsNo(num_file)
          ELSE
           err = NF_INQ_DIMLEN( fid, dimid, sampleNo(num_file,bi,bj) )
          ENDIF

          DO ip = 1, NSAMP_PER_TILE_MAX
           sample_timeS(num_file,ip,bi,bj)  = -999. _d 0
           sample_timeE(num_file,ip,bi,bj)  = -999. _d 0
           sample_lon(num_file,ip,bi,bj)    = -999. _d 0
           sample_lat(num_file,ip,bi,bj)    = -999. _d 0
           sample_depth(num_file,ip,bi,bj)  = -999. _d 0
           sample_weight(num_file,ip,bi,bj) = 1. _d 0
           DO iq = 1, NUM_INTERP_PTS_OBS
            sample_interp_i(num_file,ip,iq,bi,bj)    = 1
            sample_interp_j(num_file,ip,iq,bi,bj)    = 1
            sample_interp_k(num_file,ip,iq,bi,bj)    = 1
            sample_interp_frac(num_file,ip,iq,bi,bj) = 0. _d 0
           ENDDO
           sample_interp_xC11(num_file,ip,bi,bj)   = -999. _d 0
           sample_interp_yC11(num_file,ip,bi,bj)   = -999. _d 0
           sample_interp_xCNINJ(num_file,ip,bi,bj) = -999. _d 0
           sample_interp_yCNINJ(num_file,ip,bi,bj) = -999. _d 0
          ENDDO

          sampleNo_tile = 0
          Obsno_div1000 = MAX(0,INT(sampleNo(num_file,bi,bj)/1000))

          ntype_ssh = 0
          ntype_other = 0

          DO chunkObs = 1, Obsno_div1000+1
           chunk = 1000*(chunkObs-1)

           IF ( MIN(sampleNo(num_file,bi,bj), 1000*chunkObs).GE.
     &          1+chunk ) THEN

C Read a chunk
            vec_start = 1+chunk
            vec_count = MIN(1000,sampleNo(num_file,bi,bj)-chunk)

            IF ( (vec_count.LE.0).OR.(vec_count.GT.1000).OR.
     &           (vec_start.LE.0).OR.
     &           (vec_count+vec_start-1.GT.
     &            sampleNo(num_file,bi,bj)) ) THEN
             IL  = ILNBLNK( obsfitFile )
             WRITE( msgBuf,'(3A)' )
     &            'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &            '.nc was not read properly (case 3).'
             CALL PRINT_ERROR( msgBuf, myThid )
             stopObsfit = 1
            ENDIF

            err = NF_GET_VARA_INT( fid,varID2,vec_start,
     &            vec_count, tmp_type2 )
            err = NF_GET_VARA_DOUBLE( fid,varID3a,vec_start,
     &            vec_count, tmp_lon2 )
            err = NF_GET_VARA_DOUBLE( fid,varID3b,vec_start,
     &            vec_count, tmp_lat2 )
            err = NF_GET_VARA_DOUBLE( fid,varID4,vec_start,
     &            vec_count, tmp_depth2 )
            IF ( weighSamples.EQ.1 ) THEN
             err = NF_GET_VARA_DOUBLE( fid,varID5,vec_start,
     &             vec_count, tmp_weight2 )
            ENDIF

C If obsfitDoGenGrid then also read in the interpolation fractions
C and indices
            IF ( obsfitDoGenGrid ) THEN
             err = NF_GET_VARA_DOUBLE( fid,varID_intp1,vec_start,
     &             vec_count, tmp_xC11 )
             err = NF_GET_VARA_DOUBLE( fid,varID_intp2,vec_start,
     &             vec_count, tmp_yC11 )
             err = NF_GET_VARA_DOUBLE( fid,varID_intp11,vec_start,
     &             vec_count, tmp_xCNINJ )
             err = NF_GET_VARA_DOUBLE( fid,varID_intp22,vec_start,
     &             vec_count, tmp_yCNINJ )
             DO iq = 1, iINTERP
              vec_start2(1) = iq
              vec_start2(2) = 1+chunk
              vec_count2(1) = 1
              vec_count2(2) = min(1000,
     &        sampleNo(num_file,bi,bj)-chunk)
              err = NF_GET_VARA_DOUBLE( fid,varID_intp3,vec_start2,
     &              vec_count2, tmp_frac(1,iq) )
              err = NF_GET_VARA_DOUBLE( fid,varID_intp4,vec_start2,
     &              vec_count2, tmp_i(1,iq) )
              err = NF_GET_VARA_DOUBLE( fid,varID_intp5,vec_start2,
     &              vec_count2, tmp_j(1,iq) )
              err = NF_GET_VARA_DOUBLE( fid,varID_intp6,vec_start2,
     &              vec_count2, tmp_k(1,iq) )
             ENDDO
            ENDIF

            IF ( err.NE.NF_NOERR ) THEN
             WRITE( msgBuf,'(3A)' )
     &            'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &            '.nc was not read properly (case 4).'
             CALL PRINT_ERROR( msgBuf, myThid )
             stopObsfit = 1
            ENDIF

C Loop through this chunk
            DO ic = 1, MIN(1000,sampleNo(num_file,bi,bj)-chunk)

             IF ( stopObsfit .EQ. 0) THEN

              sampleIsValid = 1
              sampleIsInTile = 1

C Check that sample time is valid
              IF ( tmp_sample_time(ic+chunk).LT.0. _d 0 ) THEN
               sampleIsValid = 0
              ENDIF
              IF ( (tmp_sample_time(ic+chunk).GT.modelend) .OR.
     &             (tmp_sample_time(ic+chunk).LT.modelstart) ) THEN
               sampleIsValid = 0
              ENDIF

              type_cur = tmp_type2(ic)
              lon_cur = tmp_lon2(ic)
              lat_cur = tmp_lat2(ic)
              depth_cur = tmp_depth2(ic)

C Keep track of which variables in the file
              IF ( type_cur.EQ.5 ) THEN
               ntype_ssh = ntype_ssh+1
              ELSE
               ntype_other = ntype_other+1
              ENDIF

C Determine whether sample is in current tile domain
C Process only valid samples ( sampleIsValid.EQ.1 )

C===========================================================
C lat-lon grid case
C===========================================================
              IF ( (.NOT.obsfitDoGenGrid ).AND.
     &             ( sampleIsValid.EQ.1 ) ) THEN
               IF ( xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj) ) THEN
                tmp_lon = xC(sNx+1,1,bi,bj)+360. _d 0
               ELSE
                tmp_lon = xC(sNx+1,1,bi,bj)
               ENDIF

               IF ( (xC(1,1,bi,bj).LE.lon_cur).AND.
     &              (tmp_lon.GT.lon_cur).AND.
     &              (yC(1,1,bi,bj).LE.lat_cur).AND.
     &              (yC(1,sNy+1,bi,bj).GT.lat_cur) ) THEN
C Do nothing
               ELSEIF ( (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)).AND.
     &                  (xC(1,1,bi,bj).LE.lon_cur+360. _d 0).AND.
     &                  (tmp_lon.GT.lon_cur+360. _d 0).AND.
     &                  (yC(1,1,bi,bj).LE.lat_cur).AND.
     &                  (yC(1,sNy+1,bi,bj).GT.lat_cur) ) THEN
                lon_cur=lon_cur+360. _d 0
               ELSE
C Not in this tile
                sampleIsInTile = 0
               ENDIF

C Now determine value of i,j,k south-west-down of data point
               sample_i = -10
               sample_j = -10
               sample_k1 = -10
               sample_k2 = -10
               lon_1 = -10
               lon_2 = -10
               lat_1 = -10
               lat_2 = -10
               depth_1 = -10
               depth_2 = -10

               IF ( sampleIsInTile.EQ.1 ) THEN

C Value of k, below the data point:
C Case 1: above first depth level
                IF ( -rC(1).GT.depth_cur ) THEN
                 sample_k1 = 1
                 sample_k2 = 1
                 depth_fac = 1. _d 0
                ENDIF
C Case 2: below last depth level
                IF ( -rC(Nr).LE.depth_cur ) THEN
                 sample_k1 = Nr
                 sample_k2 = Nr
                 depth_fac = 1. _d 0
                ENDIF
C Case 3: between two depth levels
                DO kLev = 1, Nr-1
                 IF ( (-rC(kLev).LE.depth_cur).AND.
     &                (-rC(kLev+1).GT.depth_cur) ) THEN
                  sample_k1 = kLev
                  sample_k2 = kLev+1
                  depth_1 = -rC(kLev)
                  depth_2 = -rC(kLev+1)
                  depth_fac = (depth_cur-depth_1)/(depth_2-depth_1)
                 ENDIF
                ENDDO

                DO j = 1, sNy+1
                 DO i = 1, sNx+1

                  IF ( type_cur.EQ.3 ) THEN
C Zonal velocity
                   lon_tmp1 = xG(i,j,bi,bj)
                   lon_tmp2 = xG(i+1,j,bi,bj)
                   lat_tmp1 = yC(i,j,bi,bj)
                   lat_tmp2 = yC(i,j+1,bi,bj)
                  ELSEIF ( type_cur.EQ.4 ) THEN
c Meridional velocity
                   lon_tmp1 = xC(i,j,bi,bj)
                   lon_tmp2 = xC(i+1,j,bi,bj)
                   lat_tmp1 = yG(i,j,bi,bj)
                   lat_tmp2 = yG(i,j+1,bi,bj)
                  ELSE
                   lon_tmp1 = xC(i,j,bi,bj)
                   lon_tmp2 = xC(i+1,j,bi,bj)
                   lat_tmp1 = yC(i,j,bi,bj)
                   lat_tmp2 = yC(i,j+1,bi,bj)
                  ENDIF

C Value of j, south of the data point:
                  IF ( (lat_tmp1.LE.lat_cur).AND.
     &                 (lat_tmp2.GT.lat_cur) ) THEN
                   sample_j = j
                   lat_1 = lat_tmp1
                   lat_2 = lat_tmp2
                  ENDIF

C Value of i, west of the data point:
                  IF ( xC(i+1,j,bi,bj).LT.xC(1,j,bi,bj) ) THEN
                   lon_tmp2 = lon_tmp2+360
                  ENDIF
                  IF ( xC(i,j,bi,bj).LT.xC(1,j,bi,bj) ) THEN
                   lon_tmp1 = lon_tmp1+360
                  ENDIF
                  IF ( (lon_tmp1.LE.lon_cur).AND.
     &                 (lon_tmp2.GT.lon_cur) ) THEN
                   sample_i = i
                   lon_1 = lon_tmp1
                   lon_2 = lon_tmp2
                  ENDIF

                 ENDDO !i
                ENDDO !j
               ENDIF !sampleIsInTile

               IF ( (sample_i.EQ.-10).OR.(sample_j.EQ.-10) ) THEN
                sampleIsInTile = 0
               ENDIF
               IF ( (sample_k1.EQ.-10).OR.(sample_k2.EQ.-10) ) THEN
                sampleIsInTile = 0
               ENDIF

               IF ( sampleIsInTile.EQ.1 ) THEN
C If yes then store sample index position and number of samples:
                sampleNo_tile = sampleNo_tile+1
                sample_ind_glob(num_file,sampleNo_tile,bi,bj)
     &           = ic+chunk
                sample_timeS(num_file,sampleNo_tile,bi,bj)=
     &           tmp_sample_time(ic+chunk)
C "-1" to avoid counting in an extra time step
C when time averaging
                IF ( obsfitOperation(num_file).eq.0 ) THEN
                 sample_timeE(num_file,sampleNo_tile,bi,bj)=
     &            sample_timeS(num_file,sampleNo_tile,bi,bj)
     &            +tmp_sample_delT(ic+chunk)
                ELSE
                 sample_timeE(num_file,sampleNo_tile,bi,bj)=
     &            sample_timeS(num_file,sampleNo_tile,bi,bj)
     &            +tmp_sample_delT(ic+chunk)-1
                ENDIF
                sample_lon(num_file,sampleNo_tile,bi,bj)=lon_cur
                sample_lat(num_file,sampleNo_tile,bi,bj)=lat_cur
                sample_depth(num_file,sampleNo_tile,bi,bj)=depth_cur
                IF ( weighSamples.EQ.1 ) THEN
                 sample_weight(num_file,sampleNo_tile,bi,bj)=
     &            tmp_weight2(ic)
                ENDIF
                sample_type(num_file,sampleNo_tile,bi,bj)=type_cur

C Store interpolation fractions and indices
                lon_fac = (lon_cur-lon_1)/(lon_2-lon_1)
                lat_fac = (lat_cur-lat_1)/(lat_2-lat_1)

                w(1)=(1-lon_fac)*(1-lat_fac)*(1-depth_fac)
                sample_interp_i(num_file,sampleNo_tile,1,bi,bj)
     &           =sample_i
                sample_interp_j(num_file,sampleNo_tile,1,bi,bj)
     &           =sample_j
                sample_interp_k(num_file,sampleNo_tile,1,bi,bj)
     &           =sample_k1
                w(2)=lon_fac*(1-lat_fac)*(1-depth_fac)
                sample_interp_i(num_file,sampleNo_tile,2,bi,bj)
     &           =sample_i+1
                sample_interp_j(num_file,sampleNo_tile,2,bi,bj)
     &           =sample_j
                sample_interp_k(num_file,sampleNo_tile,2,bi,bj)
     &           =sample_k1
                w(3)=(1-lon_fac)*lat_fac*(1-depth_fac)
                sample_interp_i(num_file,sampleNo_tile,3,bi,bj)
     &           =sample_i
                sample_interp_j(num_file,sampleNo_tile,3,bi,bj)
     &           =sample_j+1
                sample_interp_k(num_file,sampleNo_tile,3,bi,bj)
     &           =sample_k1
                w(4)=lon_fac*lat_fac*(1-depth_fac)
                sample_interp_i(num_file,sampleNo_tile,4,bi,bj)
     &           =sample_i+1
                sample_interp_j(num_file,sampleNo_tile,4,bi,bj)
     &           =sample_j+1
                sample_interp_k(num_file,sampleNo_tile,4,bi,bj)
     &           =sample_k1
                w(5)=(1-lon_fac)*(1-lat_fac)*depth_fac
                sample_interp_i(num_file,sampleNo_tile,5,bi,bj)
     &           =sample_i
                sample_interp_j(num_file,sampleNo_tile,5,bi,bj)
     &           =sample_j
                sample_interp_k(num_file,sampleNo_tile,5,bi,bj)
     &           =sample_k2
                w(6)=lon_fac*(1-lat_fac)*depth_fac
                sample_interp_i(num_file,sampleNo_tile,6,bi,bj)
     &           =sample_i+1
                sample_interp_j(num_file,sampleNo_tile,6,bi,bj)
     &           =sample_j
                sample_interp_k(num_file,sampleNo_tile,6,bi,bj)
     &           =sample_k2
                w(7)=(1-lon_fac)*lat_fac*depth_fac
                sample_interp_i(num_file,sampleNo_tile,7,bi,bj)
     &           =sample_i
                sample_interp_j(num_file,sampleNo_tile,7,bi,bj)
     &           =sample_j+1
                sample_interp_k(num_file,sampleNo_tile,7,bi,bj)
     &           =sample_k2
                w(8)=lon_fac*lat_fac*depth_fac
                sample_interp_i(num_file,sampleNo_tile,8,bi,bj)
     &           =sample_i+1
                sample_interp_j(num_file,sampleNo_tile,8,bi,bj)
     &           =sample_j+1
                sample_interp_k(num_file,sampleNo_tile,8,bi,bj)
     &           =sample_k2

C Ignore points that are not "wet"
                IF ( type_cur.EQ.3 ) THEN
c Zonal velocity
                 mask(1)=maskW(sample_i,sample_j,sample_k1,bi,bj)
                 mask(2)=maskW(sample_i+1,sample_j,sample_k1,bi,bj)
                 mask(3)=maskW(sample_i,sample_j+1,sample_k1,bi,bj)
                 mask(4)=maskW(sample_i+1,sample_j+1,sample_k1,bi,bj)
                 mask(5)=maskW(sample_i,sample_j,sample_k2,bi,bj)
                 mask(6)=maskW(sample_i+1,sample_j,sample_k2,bi,bj)
                 mask(7)=maskW(sample_i,sample_j+1,sample_k2,bi,bj)
                 mask(8)=maskW(sample_i+1,sample_j+1,sample_k2,bi,bj)
                ELSEIF ( type_cur.EQ.4 ) THEN
c Meridional velocity
                 mask(1)=maskS(sample_i,sample_j,sample_k1,bi,bj)
                 mask(2)=maskS(sample_i+1,sample_j,sample_k1,bi,bj)
                 mask(3)=maskS(sample_i,sample_j+1,sample_k1,bi,bj)
                 mask(4)=maskS(sample_i+1,sample_j+1,sample_k1,bi,bj)
                 mask(5)=maskS(sample_i,sample_j,sample_k2,bi,bj)
                 mask(6)=maskS(sample_i+1,sample_j,sample_k2,bi,bj)
                 mask(7)=maskS(sample_i,sample_j+1,sample_k2,bi,bj)
                 mask(8)=maskS(sample_i+1,sample_j+1,sample_k2,bi,bj)
                ELSE
                 mask(1)=maskC(sample_i,sample_j,sample_k1,bi,bj)
                 mask(2)=maskC(sample_i+1,sample_j,sample_k1,bi,bj)
                 mask(3)=maskC(sample_i,sample_j+1,sample_k1,bi,bj)
                 mask(4)=maskC(sample_i+1,sample_j+1,sample_k1,bi,bj)
                 mask(5)=maskC(sample_i,sample_j,sample_k2,bi,bj)
                 mask(6)=maskC(sample_i+1,sample_j,sample_k2,bi,bj)
                 mask(7)=maskC(sample_i,sample_j+1,sample_k2,bi,bj)
                 mask(8)=maskC(sample_i+1,sample_j+1,sample_k2,bi,bj)
                ENDIF !type_cur

                wtot = 0
                DO iq = 1, NUM_INTERP_PTS_OBS
                 IF (mask(iq).EQ.0) w(iq)=0
                 wtot = wtot+w(iq)
                ENDDO
                DO iq = 1, NUM_INTERP_PTS_OBS
                 sample_interp_frac(num_file,sampleNo_tile,iq,bi,bj) =
     &            w(iq)/(wtot+1. _d -10)
                ENDDO

               ENDIF !sampleIsInTile

C===========================================================
C Generic grid case
C===========================================================
C Determine whether sample is in current tile domain
              ELSEIF ( sampleIsValid.EQ.1 ) THEN

               IF ( stopGenericGrid.EQ.0 ) THEN

                IF ( (abs(tmp_xC11(ic) - xC(1,1,bi,bj))
     &               .LT.0.0001 _d 0 ).AND.
     &               (abs(tmp_yC11(ic) - yC(1,1,bi,bj))
     &               .LT.0.0001 _d 0).AND.
     &               (abs(tmp_xCNINJ(ic) - xC(sNx,sNy,bi,bj))
     &               .LT.0.0001 _d 0).AND.
     &               (abs(tmp_yCNINJ(ic) - yC(sNx,sNy,bi,bj))
     &               .LT.0.0001 _d 0) ) THEN
C Do nothing
                ELSE
                 sampleIsInTile = 0
                ENDIF

                IF ( sampleIsInTile.EQ.1 ) THEN
C If yes then store sample_time and interpolation fractions and indices:
                 sampleNo_tile = sampleNo_tile+1
                 sample_type(num_file,sampleNo_tile,bi,bj)=type_cur
                 sample_timeS(num_file,sampleNo_tile,bi,bj)=
     &            tmp_sample_time(ic+chunk)
C "-1" to avoid counting in an extra time step
C when time averaging
                 IF ( obsfitOperation(num_file).EQ.0 ) THEN
                  sample_timeE(num_file,sampleNo_tile,bi,bj)=
     &             sample_timeS(num_file,sampleNo_tile,bi,bj)
     &             +tmp_sample_delT(ic+chunk)
                 ELSE
                  sample_timeE(num_file,sampleNo_tile,bi,bj)=
     &             sample_timeS(num_file,sampleNo_tile,bi,bj)
     &             +tmp_sample_delT(ic+chunk)-1
                 ENDIF
                 IF ( weighSamples.EQ.1 ) THEN
                  sample_weight(num_file,sampleNo_tile,bi,bj)=
     &             tmp_weight2(ic)
                 ENDIF
                 sample_interp_xC11(num_file,sampleNo_tile,
     &            bi,bj)=tmp_xC11(ic)
                 sample_interp_yC11(num_file,sampleNo_tile,
     &            bi,bj)=tmp_yC11(ic)
                 sample_interp_xCNINJ(num_file,sampleNo_tile,
     &            bi,bj)=tmp_xCNINJ(ic)
                 sample_interp_yCNINJ(num_file,sampleNo_tile,
     &            bi,bj)=tmp_yCNINJ(ic)
                 tmp_sum_fracs = 0. _d 0

C Loop over interpolation points
                 DO iq = 1, iINTERP
                  sample_interp_frac(num_file,sampleNo_tile,iq,
     &             bi,bj)=tmp_frac(ic,iq)
                  sample_interp_i(num_file,sampleNo_tile,iq,
     &             bi,bj)=tmp_i(ic,iq)
                  sample_interp_j(num_file,sampleNo_tile,iq,
     &             bi,bj)=tmp_j(ic,iq)
                  sample_interp_k(num_file,sampleNo_tile,iq,
     &             bi,bj)=tmp_k(ic,iq)
                  tmp_sum_fracs = tmp_sum_fracs+tmp_frac(ic,iq)

C More test of the inputs: is the offline-computed
C interpolation information consistent (self and with grid)
                  IF ( (tmp_i(ic,iq).LT.0).OR.
     &                 (tmp_j(ic,iq).LT.0).OR.
     &                 (tmp_i(ic,iq).GT.sNx+1).OR.
     &                 (tmp_j(ic,iq).GT.sNy+1) ) THEN
                   WRITE( msgBuf,'(4A)' )
     &                  'OBSFIT_INIT_FIXED: file ',
     &                  obsfitFile(1:IL),
     &                  '.nc includes inconsistent interpolation ',
     &                  ' points (obsfitDoGenGrid; out of tile)'
                   CALL PRINT_ERROR( msgBuf, myThid )
                    stopGenericGrid = 1
                  ENDIF
#ifdef ALLOW_OBSFIT_EXCLUDE_CORNERS
                  IF ( tmp_frac(ic,iq) .NE. 0. _d 0) THEN
                   IF ( ((tmp_i(ic,iq).EQ.0) .AND.
     &                   (tmp_j(ic,iq).EQ.0)) .OR.
     &                  ((tmp_i(ic,iq).EQ.sNx+1) .AND.
     &                   (tmp_j(ic,iq).EQ.sNy+1)) .OR.
     &                  ((tmp_i(ic,iq).EQ.0) .AND.
     &                   (tmp_j(ic,iq).EQ.sNy+1)) .OR.
     &                  ((tmp_i(ic,iq).EQ.sNx+1) .AND.
     &                   (tmp_j(ic,iq).EQ.0)) ) THEN
                    WRITE( msgBuf,'(4A)' )
     &                   'OBSFIT_INIT_FIXED: file ',
     &                   obsfitFile(1:IL),
     &                   '.nc includes inconsistent interpolation ',
     &                   ' points (obsfitDoGenGrid; overlap corners)'
                    CALL PRINT_ERROR( msgBuf, myThid )
                     stopGenericGrid = 1
                   ENDIF
                  ENDIF
#endif /* ALLOW_OBSFIT_EXCLUDE_CORNERS */
                  IF ( (tmp_frac(ic,iq).LT.0. _d 0).OR.
     &                 (tmp_frac(ic,iq).GT.1. _d 0) ) THEN
                   WRITE( msgBuf,'(4A)' )
     &                  'OBSFIT_INIT_FIXED: file ',
     &                   obsfitFile(1:IL),
     &                   '.nc includes inconsistent interpolation ',
     &                   ' points (obsfitDoGenGrid; sum not 0-1)'
                   CALL PRINT_ERROR( msgBuf, myThid )
                   stopGenericGrid = 1
                  ENDIF
                 ENDDO! iq

                 IF ( abs(tmp_sum_fracs -1. _d 0 ) .GT.
     &                0.0001 _d 0) THEN
                  WRITE( msgBuf,'(4A)' )
     &                 'OBSFIT_INIT_FIXED: file ',
     &                 obsfitFile(1:IL),
     &                 '.nc includes inconsistent interpolation ',
     &                 ' weights (obsfitDoGenGrid; sum not 1)'
                  CALL PRINT_ERROR( msgBuf, myThid )
                  stopGenericGrid = 1
                 ENDIF

                 sample_ind_glob(num_file,sampleNo_tile,bi,bj)
     &            =ic+chunk

                ENDIF !sampleIsInTile
               ENDIF !stopGenericGrid
              ENDIF !obsfitDoGenGrid)

C ==============================================================================

C Check that maximum size was not reached:
              IF ( sampleNo_tile.GT.NSAMP_PER_TILE_MAX ) THEN
               WRITE( msgBuf,'(3A)' )
     &              'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &              '.nc was not read properly ',
     &              '(increase NSAMP_PER_TILE_MAX).'
               CALL PRINT_ERROR( msgBuf, myThid )
               stopObsfit = 1
              ENDIF

             ENDIF !stopObsfit
            ENDDO !ic
           ENDIF !chunk
          ENDDO !chunkObs

C Number of valid samples in the file
          sampleNo(num_file,bi,bj) = sampleNo_tile

          WRITE( msgbuf,'(A,I4,A,I4)' )
     &         '  current tile is bi,bj                      =',
     &         bi,',',bj
          CALL PRINT_MESSAGE(
     &         msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

          WRITE( msgbuf,'(A,I9)' )
     &         '  # of samples within tile and time period  =',
     &         sampleNo(num_file,bi,bj)
          CALL PRINT_MESSAGE(
     &         msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

          IF ( ntype_ssh.GT.0 ) THEN
           IF ( ntype_other.GT.0 ) THEN
            WRITE( msgBuf,'(4A)' )
     &           'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &           'includes both SSH and non-SSH observations; ',
     &           'this is not currently supported.'
            CALL PRINT_ERROR( msgBuf, myThid )
            stopObsfit = 1
           ELSE
            obs_is_ssh_loc = obs_is_ssh_loc+1
           ENDIF
          ENDIF

C===========================================================
C Create files for model counterparts to samples
C===========================================================

          IF ( sampleNo(num_file,bi,bj).GT.0 ) THEN
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles

           JL  = ILNBLNK( obsfitDir )

           IF ( obsfitDoNcOutput ) THEN

            WRITE( obsfnameequinc,'(3A,I3.3,A,I3.3,A)' )
     &           obsfitDir(1:JL),obsfitFile(1:IL),
     &           '.',iG,'.',jG,'.equi.nc'
#ifdef ALLOW_ADJOINT_RUN
            WRITE( adobsfnameequinc,'(4A,I3.3,A,I3.3,A)' )
     &           obsfitDir(1:JL),'ad',obsfitFile(1:IL),
     &           '.',iG,'.',jG,'.equi.nc'
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
            WRITE( tlobsfnameequinc,'(4A,I3.3,A,I3.3,A)' )
     &           obsfitDir(1:JL),'tl',obsfitFile(1:IL),
     &           '.',iG,'.',jG,'.equi.nc'
#endif
            INQUIRE( file=obsfnameequinc, exist=exst )
            IF (.NOT.exst) THEN
             CALL OBSFIT_INIT_EQUIFILES( num_file,
     &            fiddata_obs(num_file),obsfnameequinc,
     &            fidfwd_obs(num_file,bi,bj),
     &            sampleNo(num_file,bi,bj),bi,bj,myThid )
             ELSE
              err = NF_OPEN( obsfnameequinc,NF_WRITE,
     &              fidfwd_obs(num_file,bi,bj) )
             ENDIF
#ifdef ALLOW_ADJOINT_RUN
             INQUIRE( file=adobsfnameequinc, exist=exst )
             IF (.NOT.exst) THEN
              CALL OBSFIT_INIT_EQUIFILES( num_file,
     &             fiddata_obs(num_file),adobsfnameequinc,
     &             fidadj_obs(num_file,bi,bj),
     &             sampleNo(num_file,bi,bj),bi,bj,myThid )
             ELSE
              err = NF_OPEN( adobsfnameequinc,NF_WRITE,
     &              fidadj_obs(num_file,bi,bj) )
             ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
             INQUIRE( file=tlobsfnameequinc, exist=exst )
             IF (.NOT.exst) THEN
              CALL OBSFIT_INIT_EQUIFILES( num_file,
     &             fiddata_obs(num_file),tlobsfnameequinc,
     &             fidtan_obs(num_file,bi,bj),
     &             sampleNo(num_file,bi,bj),bi,bj,myThid )
             ELSE
              err = NF_OPEN( tlobsfnameequinc,NF_WRITE,
     &              fidtan_obs(num_file,bi,bj) )
             ENDIF
#endif
            ELSE !obsfitDoNcOutput

             WRITE( obsfnameequinc,'(3A,I3.3,A,I3.3,A)' )
     &            obsfitDir(1:JL),obsfitFile(1:IL),'.',iG,'.',jG,
     &            '.equi.data'
#ifdef ALLOW_ADJOINT_RUN
             WRITE( adobsfnameequinc,'(4A,I3.3,A,I3.3,A)' )
     &            obsfitDir(1:JL),'ad',
     &            obsfitFile(1:IL),'.',iG,'.',jG,'.equi.data'
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
             WRITE( tlobsfnameequinc,'(4A,I3.3,A,I3.3,A)' )
     &            obsfitDir(1:JL),'tl',
     &            obsfitFile(1:IL),'.',iG,'.',jG,'.equi.data'
#endif

             INQUIRE( file=obsfnameequinc, exist=exst )
#ifdef OBSFIT_USE_MDSFINDUNITS
             CALL MDSFINDUNIT( fidfwd_obs(num_file,bi,bj), myThid )
#else
             CALL OBSFIT_FINDUNIT( fidfwd_obs(num_file,bi,bj),
     &            myThid )
#endif
             IF (.NOT.exst) THEN
              CALL OBSFIT_INIT_EQUIFILES( num_file,
     &             fiddata_obs(num_file),obsfnameequinc,
     &             fidfwd_obs(num_file,bi,bj),
     &             sampleNo(num_file,bi,bj),bi,bj,myThid )
             ELSE
              OPEN( fidfwd_obs(num_file,bi,bj),
     &              file=obsfnameequinc,form ='unformatted',
     &              status='unknown',access='direct',
     &              recl=2*WORDLENGTH*2 )
             ENDIF
#ifdef ALLOW_ADJOINT_RUN
             INQUIRE( file=adobsfnameequinc, exist=exst )
#ifdef OBSFIT_USE_MDSFINDUNITS
             CALL MDSFINDUNIT( fidadj_obs(num_file,bi,bj), myThid )
#else
             CALL OBSFIT_FINDUNIT( fidadj_obs(num_file,bi,bj),
     &            myThid )
#endif
             IF (.NOT.exst) THEN
              CALL OBSFIT_INIT_EQUIFILES( num_file,
     &             fiddata_obs(num_file),adobsfnameequinc,
     &             fidadj_obs(num_file,bi,bj),
     &             sampleNo(num_file,bi,bj),bi,bj,myThid )
             ELSE
              OPEN( fidadj_obs(num_file,bi,bj),
     &              file=adobsfnameequinc,form ='unformatted',
     &              status='unknown', access='direct',
     &              recl=2*WORDLENGTH*2 )
             ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
             INQUIRE( file=tlobsfnameequinc, exist=exst )
#ifdef OBSFIT_USE_MDSFINDUNITS
             CALL MDSFINDUNIT( fidtan_obs(num_file,bi,bj), myThid )
#else
             CALL OBSFIT_FINDUNIT( fidtan_obs(num_file,bi,bj),
     &            myThid )
#endif
             IF (.NOT.exst) THEN
              CALL OBSFIT_INIT_EQUIFILES( num_file,
     &             fiddata_obs(num_file),tlobsfnameequinc,
     &             fidtan_obs(num_file,bi,bj),
     &             sampleNo(num_file,bi,bj),bi,bj,myThid )
              ELSE
               OPEN( fidtan_obs(num_file,bi,bj),file=
     &               tlobsfnameequinc,form ='unformatted',
     &               status='unknown', access='direct',
     &               recl=2*WORDLENGTH*2 )
            ENDIF
#endif

           ENDIF !ObsfitDoNcOutput
          ENDIF !sampleNo
         ENDDO !bj
        ENDDO !bi

        CALL GLOBAL_SUM_INT(obs_is_ssh_loc, myThid)
        obs_is_ssh(num_file)=obs_is_ssh_loc

C===========================================================

       ELSE !if (IL.NE.0)

        DO bj = 1, nSy
         DO bi = 1, nSx
          ObsNo(num_file) = 0
          sampleNo(num_file,bi,bj) = 0
          DO ip = 1, NOBSMAX_OBS
           obs_np(num_file,ip) = -999. _d 0
          ENDDO
          DO ip = 1, NSAMP_PER_TILE_MAX
           sample_timeS(num_file,ip,bi,bj)    = -999. _d 0
           sample_timeE(num_file,ip,bi,bj)    = -999. _d 0
           sample_lon(num_file,ip,bi,bj)      = -999. _d 0
           sample_lat(num_file,ip,bi,bj)      = -999. _d 0
           sample_depth(num_file,ip,bi,bj)    = -999. _d 0
           sample_ind_glob(num_file,ip,bi,bj) = 0
           DO iq = 1,NUM_INTERP_PTS_OBS
            sample_interp_i(num_file,ip,iq,bi,bj)    = 1
            sample_interp_j(num_file,ip,iq,bi,bj)    = 1
            sample_interp_k(num_file,ip,iq,bi,bj)    = 1
            sample_interp_frac(num_file,ip,iq,bi,bj) = 0. _d 0
           ENDDO
           sample_interp_xC11(num_file,ip,bi,bj)   = -999. _d 0
           sample_interp_yC11(num_file,ip,bi,bj)   = -999. _d 0
           sample_interp_xCNINJ(num_file,ip,bi,bj) = -999. _d 0
           sample_interp_yCNINJ(num_file,ip,bi,bj) = -999. _d 0
          ENDDO
         ENDDO !bi
        ENDDO !bj

       ENDIF !if (IL.NE.0)
      ENDDO !num_file

C===========================================================
C error cases:
C===========================================================

c1) you want to provide interpolation information

C For generic grid. Needs to go inside bi,bj loop
      IF ( stopGenericGrid.EQ.2 ) THEN
c         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
c         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
ccgf XC grid
c       CALL MDSFINDUNIT( fid , myThid )
c       WRITE(obsfnameequinc(1:80),'(a,i3.3,a,i3.3,a,i4.4,a,i4.4,a)')
c     & 'profilesXCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
c         k=MDS_RECLEN(64,(sNx+2)*(sNy+2),myThid)
c            WRITE(standardMessageUnit,'(A,/,2A)')
c     & 'OBSFIT_INIT_FIXED: creating grid from profiles; file:',
c     & obsfnameequinc
c       OPEN( fid, file= obsfnameequinc, form ='unformatted',
c     &      status='unknown',access='direct', recl= k)
c        DO m=0,sNy+1
c         DO l=0,sNx+1
c          xy_buffer_r8(l,m)=xC(l,m,bi,bj)
c         ENDDO
c        ENDDO
c#ifdef _BYTESWAPIO
c            CALL MDS_BYTESWAPR8((sNx+2)*(sNy+2),xy_buffer_r8)
c#endif
c       WRITE(fid,rec=1) xy_buffer_r8
c       close(fid)
ccgf YC grid
c       CALL MDSFINDUNIT( fid , myThid )
c       WRITE(obsfnameequinc(1:80),'(a,i3.3,a,i3.3,a,i4.4,a,i4.4,a)')
c     & 'profilesYCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
c         k=MDS_RECLEN(64,(sNx+2)*(sNy+2),myThid)
c            WRITE(standardMessageUnit,'(A,/,A)')
c     & 'OBSFIT_INIT_FIXED: creating grid from profiles; file:',
c     & obsfnameequinc
c       OPEN( fid, file= obsfnameequinc, form ='unformatted',
c     & status='unknown', access='direct', recl= k)
c        DO m=0,sNy+1
c         DO l=0,sNx+1
c                xy_buffer_r8(l,m)=yC(l,m,bi,bj)
c         ENDDO
c        ENDDO
c#ifdef _BYTESWAPIO
c            CALL MDS_BYTESWAPR8((sNx+2)*(sNy+2),xy_buffer_r8)
c#endif
c       WRITE(fid,rec=1) xy_buffer_r8
c       close(fid)

       WRITE( msgBuf,'(3A)' )
     &      'OBSFIT_INIT_FIXED : ',
     &      'when using obsfitDoGenGrid ',
     &      'you have to provide interpolation fractions etc. '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE( msgBuf,'(2A)' )
     &      'and some of your nc files dont have them. ',
     &      'You could use profiles_prep_mygrid.m and/or'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE( msgBuf,'(A)' )
     &      'use the grid info in profiles*incl1PointOverlap*data'
       CALL PRINT_ERROR( msgBuf, myThid )
       stopObsfit=1

      ENDIF

      _END_MASTER( myThid )
      _BARRIER

c2) stop after other kind of errors
      CALL GLOBAL_SUM_INT( stopObsfit , myThid )
      IF ( stopObsfit.GE.1) THEN
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R OBSFIT_INIT_FIXED'
      ENDIF

      CALL GLOBAL_SUM_INT( stopGenericGrid , myThid )
      IF ( stopGenericGrid.GE.1) THEN
       CALL ALL_PROC_DIE( myThid )
       STOP 'ABNORMAL END: S/R OBSFIT_INIT_FIXED'
      ENDIF

      WRITE(msgbuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )
      WRITE( msgbuf,'(A)' )
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

      WRITE( msgbuf,'(A)' )
     &'// obsfit model sampling >>> END <<<'
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

      WRITE( msgbuf,'(A)' )
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

      WRITE( msgbuf,'(A)' ) ' '
      CALL PRINT_MESSAGE( msgbuf, iUnit, SQUEEZE_RIGHT, myThid )

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
