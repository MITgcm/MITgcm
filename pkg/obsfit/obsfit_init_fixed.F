#include "OBSFIT_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: OBSFIT_INIT_FIXED

C !INTERFACE:
      SUBROUTINE OBSFIT_INIT_FIXED( myThid )

C !DESCRIPTION:
C Initialize OBSFIT variables that are kept fixed during the run.

C !USES:
      IMPLICIT NONE
C == Global variables ===
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

C !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_OBSFIT

C !LOCAL VARIABLES:
      INTEGER i,j,k,l,m,bi,bj,iG,jG,num_file,sampleNo_tile
      INTEGER stopObsfit
      INTEGER fid, dimid, varid0, varid1, varid1a, varid1b, varid1c
      INTEGER varid2, varid3a, varid3b, varid4, varid5
      INTEGER varid6, varid7
      _RL tmpyymmdd(1000),tmphhmmss(1000),diffsecs
      _RL yymmddMin,yymmddMax
      _RL hhmmssMin,hhmmssMax
      INTEGER tmpdate(4),tmpdiff(4),obsIsInRunTime,sampleIsInTile
      INTEGER loopThroughSamples, weighSamples, readDelT
      INTEGER tmp_type, tmp_type2(1000), type_cur
      _RL tmpnp(1000), tmpdelT(1000)
      _RL tmp_lon, tmp_lon2(1000), tmp_lat2(1000), lon_cur, lat_cur
      _RL tmp_depth2(1000), depth_cur, delT_cur
      _RL tmp_weight2(1000)
      _RL tmp_sample_time(NSAMPLESMAX)
      _RL tmp_sample_delT(NSAMPLESMAX)
      _RL lon_1, lon_2, lat_1, lat_2
      _RL depth_1, depth_2
      _RL lon_tmp1, lon_tmp2, lat_tmp1, lat_tmp2
      _RL lat_fac, lon_fac, depth_fac
      _RL mask(NUM_INTERP_PTS_OBS)
      _RL w(NUM_INTERP_PTS_OBS), wtot
      INTEGER sample_i, sample_j, sample_k1, sample_k2
      INTEGER np_cur, np_valid
      INTEGER vec_start, vec_count, sampleno_div1000, kk
      INTEGER ObsNo_valid, ObsNo_div1000
      INTEGER sample_cnt, sample_valid_cnt, ns
      INTEGER vec_start3, vec_count3
      CHARACTER*(MAX_LEN_FNAM) obsfitfile, fnamedatanc
      CHARACTER*(MAX_LEN_FNAM) fnameequinc
      CHARACTER*(MAX_LEN_FNAM) adfnameequinc, tanfnameequinc
      CHARACTER*(MAX_LEN_FNAM) fnameequincglo
      CHARACTER*(MAX_LEN_FNAM) adfnameequincglo, tanfnameequincglo
      CHARACTER*(MAX_LEN_FNAM) fnamemisfit
      INTEGER IL, JL, KL, err, err_s
      INTEGER varid(2)
      INTEGER varid_intp1, varid_intp2, varid_intp11, varid_intp22
      INTEGER varid_intp3, varid_intp4, varid_intp5, q, iINTERP
      _RL tmp_i(1000,NUM_INTERP_PTS_OBS)
      _RL tmp_j(1000,NUM_INTERP_PTS_OBS)
      _RL tmp_weights(1000,NUM_INTERP_PTS_OBS),tmp_sum_weights
      _RL tmp_xC11(1000),tmp_yC11(1000)
      _RL tmp_xCNINJ(1000),tmp_yCNINJ(1000)
      INTEGER stopGenericGrid
      Real*8 xy_buffer_r8(0:sNx+1,0:sNy+1)
      INTEGER vec_start2(2), vec_count2(2)
      INTEGER hh, obsNo_hh, obs_np_max, ii
      INTEGER ntype_ssh,ntype_other, obs_is_ssh_loc
      LOGICAL exst

C !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN
      CHARACTER*(MAX_LEN_MBUF) msgbuf

      WRITE(msgbuf,'(a)') ' '
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)')
     &'// ======================================================='
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)')
     &'// insitu obsfit model sampling >>> START <<<'
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)')
     &'// ======================================================='
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)') ' '
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)

      stopObsfit=0
      stopGenericGrid=0

      IF ( (.NOT.obsfitDoGenGrid).AND.
     &     (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
        WRITE(msgBuf,'(2A)') 'OBSFIT_INIT_FIXED: ',
     &  'obsfitDoGenGrid=.true. is required'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'OBSFIT_INIT_FIXED: ',
     &  'unless usingSphericalGrid=.TRUE. and rotateGrid=.FALSE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R OBSFIT_INIT_FIXED'
      ENDIF

c      IF ( obsfitDoGenGrid ) THEN
c        WRITE(msgBuf,'(2A)') 'OBSFIT_INIT_FIXED: ',
c     &  'obsfitDoGenGrid has not been tested yet'
c        CALL PRINT_ERROR( msgBuf, myThid )
c        CALL ALL_PROC_DIE( myThid )
c        STOP 'ABNORMAL END: S/R OBSFIT_INIT_FIXED'
c      ENDIF

      WRITE(msgbuf,'(a)') ' '
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)') 'general packages parameters :'
      JL  = ILNBLNK( obsfitDir )
      IF (JL.NE.0) THEN
        WRITE(msgbuf,'(a,a)') '  obsfitDir ',obsfitDir(1:JL)
      ELSE
        WRITE(msgbuf,'(a,a)') '  obsfitDir ','./'
      ENDIF
      CALL print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a,l5)') '  obsfitDoGenGrid  ',obsfitDoGenGrid
      CALL print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a,l5)') '  obsfitDoNcOutput ',obsfitDoNcOutput
      CALL print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)') ' '
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)

      _BEGIN_MASTER( myThid )

C Initialize global variables for obsfit_read_obs.F
      obsfit_curfile_buff=0
      DO l=1,1000
       obsfit_data_buff(l)=0. _d 0
       obsfit_uncert_buff(l)=0. _d 0
      ENDDO

      yymmddMin=modelstartdate(1)
      yymmddMax=modelenddate(1)
      hhmmssMin=modelstartdate(2)
      hhmmssMax=modelenddate(2)

      DO num_file=1,NFILESMAX_OBS

       ObsNo_hh=0
       obs_np_max=0
       obs_is_ssh_loc=0
       obs_is_ssh(num_file)=0

       obsfitFile=' '
       IL  = ILNBLNK( obsfitFiles(num_file) )
       IF (IL.NE.0) THEN
        WRITE(obsfitFile,'(1a)')
     &     obsfitFiles(num_file)(1:IL)
        WRITE(msgbuf,'(a)') ' '
        CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
        WRITE(msgbuf,'(a,i3,a,a)')
     &     'obsfit file #',num_file,' is ', obsfitFile(1:IL)
        CALL print_message(
     &     msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)
       ENDIF

       IL  = ILNBLNK( obsfitFile )
       IF (IL.NE.0) THEN

C===========================================================
C open data files and read information
C===========================================================

        WRITE(fnamedatanc,'(2a)') obsfitFile(1:IL),'.nc'
        err = NF_OPEN(fnamedatanc, 0, fiddata_obs(num_file))

C Read number of observations:
        fid = fiddata_obs(num_file)
        err = NF_INQ_DIMID(fid,'iOBS', dimid )
        err = NF_INQ_DIMLEN(fid, dimid, obsNo(num_file) )
      
        WRITE(msgbuf,'(a,i9)')
     &     '  # of observations in file                   =',
     &     obsNo(num_file)
        CALL print_message(
     &     msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

C Read number of samples (if missing, assume it's 1):
        loopThroughSamples=1
        err = NF_INQ_VARID(fid,'obs_np', varid0 )
        IF (err.NE.NF_NOERR) THEN
         loopThroughSamples=0

         WRITE(msgbuf,'(a,a)')
     &     '      input obsfit file does not have NP (num. samples);',
     &     '      assume NP=1 for all obs '
         CALL print_message(
     &      msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

        ENDIF

C Read the info for observations:
        err = NF_INQ_VARID(fid,'obs_YYYYMMDD', varid1a )
        err = err + NF_INQ_VARID(fid,'obs_HHMMSS', varid1b )
        err = err + NF_INQ_VARID(fid,'obs_val', varid6 )
        err = err + NF_INQ_VARID(fid,'obs_uncert', varid7 )

        IF (err.NE.NF_NOERR) THEN
         IL  = ILNBLNK( obsfitFile )
         WRITE(msgBuf,'(3A)')
     &    'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &    '.nc is not in the pkg/obsfit format'
         CALL PRINT_ERROR( msgBuf, myThid)
         stopObsfit=1
        ENDIF

        readDelT=1
        err = NF_INQ_VARID(fid,'obs_delT', varid1c )
C Time interval (if missing, assume instanteneous):
        IF (err.NE.NF_NOERR) THEN
         readDelT=0

         WRITE(msgbuf,'(a,a)')
     &     '      input obsfit file does not have time interval;',
     &     '      assume observations are instantaneous '
         CALL print_message(
     &      msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

        ENDIF
                
C Read the info for samples:
        err = NF_INQ_VARID(fid,'sample_type',varid2)
        err = err + NF_INQ_VARID(fid,'sample_lon', varid3a )
        err = err + NF_INQ_VARID(fid,'sample_lat', varid3b )
        err = err + NF_INQ_VARID(fid,'sample_depth', varid4 )
        IF (err.NE.NF_NOERR) THEN
         IL  = ILNBLNK( obsfitFile )
         WRITE(msgBuf,'(3A)')
     &    'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &    '.nc is not in the pkg/obsfit format'
         CALL PRINT_ERROR( msgBuf, myThid)
         stopObsfit=1
        ENDIF

        weighSamples=1
        err = NF_INQ_VARID(fid,'sample_weight', varid5 )
C Weight for each sample (if missing, assume all samples weigh equally):
        IF (err.NE.NF_NOERR) THEN
         weighSamples=0

         WRITE(msgbuf,'(a,a)')
     &     '      input obsfit file does not have sample weight;',
     &     '      assume all samples are weighed evenly '
         CALL print_message(
     &      msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

        ENDIF

        IF (obsfitDoGenGrid) THEN
C Read interpolation information (grid points, coeffs, etc.)
         err = NF_INQ_VARID(fid,'sample_interp_XC11',varid_intp1)
         err = NF_INQ_VARID(fid,'sample_interp_YC11',varid_intp2)
         err = NF_INQ_VARID(fid,'sample_interp_XCNINJ',varid_intp11)
         err = NF_INQ_VARID(fid,'sample_interp_YCNINJ',varid_intp22)
         err = NF_INQ_VARID(fid,'sample_interp_w',varid_intp3)
         err = NF_INQ_VARID(fid,'sample_interp_i',varid_intp4)
         err = NF_INQ_VARID(fid,'sample_interp_j',varid_intp5)
         IF (err.NE.NF_NOERR) THEN
          IL  = ILNBLNK( obsfitFile )
          WRITE(msgBuf,'(3A)')
     &   'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &   '.nc is missing interpolation information (obsfitDoGenGrid)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=2
         ENDIF
        ENDIF

C Variables names for equi files
        obsfit_nameequi = 'mod_val'
        obsfit_namemask = 'mod_mask'
        obsfit_nameval = 'obs_val'
        obsfit_nameuncert = 'obs_uncert'

C Create new netcdf global file for obs-equivalent 
        IF ( myProcId .eq. 0 ) THEN
         JL  = ILNBLNK( obsfitDir )

         WRITE(fnameequincglo,'(3a)')
     &    obsfitDir(1:JL),obsfitFile(1:IL),'.equi.nc'
         WRITE(adfnameequincglo,'(4a)')
     &    obsfitDir(1:JL),'ad',
     &    obsfitFile(1:IL),'.equi.nc'
         WRITE(tanfnameequincglo,'(4a)')
     &    obsfitDir(1:JL),'tl',
     &    obsfitFile(1:IL),'.equi.nc'

         inquire( file=fnameequincglo, exist=exst )
         IF (.NOT.exst) THEN

          err = NF_CREATE(fnameequincglo,NF_CLOBBER,fidglobal(num_file))
          err = NF_DEF_DIM(fidglobal(num_file),'iOBS',ObsNo(num_file),
     &           dimid)

          err = NF_DEF_VAR(fidglobal(num_file),
     &         'mod_val',NF_DOUBLE,
     &         1,dimid,varid(1))

          err = NF_PUT_ATT_DOUBLE(fidglobal(num_file),
     &         varid(1),'_FillValue',
     &         NF_DOUBLE,1, -9999. _d 0 )

          err = NF_DEF_VAR(fidglobal(num_file),
     &         'mod_mask',NF_DOUBLE,
     &         1,dimid,varid(2))

          err = NF_PUT_ATT_DOUBLE(fidglobal(num_file),
     &         varid(2),'_FillValue',
     &         NF_DOUBLE,1, -9999. _d 0 )

          err = NF_ENDDEF(fidglobal(num_file))
          err = NF_CLOSE(fidglobal(num_file))
          err = NF_OPEN(fnameequincglo, NF_WRITE, fidglobal(num_file))

         ELSE
          err = NF_OPEN(fnameequincglo, NF_WRITE, fidglobal(num_file))
         ENDIF

C Global file for misfits
         WRITE(fnamemisfit,'(3a)')
     &     obsfitDir(1:JL),obsfitfile(1:IL),'.misfit.nc'

         err = NF_CREATE(fnamemisfit,NF_CLOBBER,fidmisfit(num_file))
         err = NF_DEF_DIM(fidmisfit(num_file),'iOBS',ObsNo(num_file),
     &           dimid)
         err = NF_DEF_VAR(fidmisfit(num_file),'misfit',
     &     NF_DOUBLE,1,dimid,varid(1))
         err = NF_PUT_ATT_DOUBLE(fidmisfit(num_file),varid(1),
     &     '_FillValue',NF_DOUBLE,1,-9999. _d 0)

         err = NF_ENDDEF(fidmisfit(num_file))
         err = NF_CLOSE(fidmisfit(num_file))

#ifdef ALLOW_ADJOINT_RUN
         inquire( file=adfnameequincglo, exist=exst )
         IF (.NOT.exst) THEN

          err = NF_CREATE(adfnameequincglo,NF_CLOBBER,
     &           fidadglobal(num_file))
          err = NF_DEF_DIM(fidadglobal(num_file),'iOBS',
     &           ObsNo(num_file),dimid)

          err = NF_DEF_VAR(fidadglobal(num_file),
     &         'mod_val',NF_DOUBLE,
     &         1,dimid,varid(1))

          err = NF_PUT_ATT_DOUBLE(fidadglobal(num_file),
     &         varid(1),'_FillValue',
     &         NF_DOUBLE,1, 0. _d 0 )

          err = NF_DEF_VAR(fidglobal(num_file),
     &         'mod_mask',NF_DOUBLE,
     &         1,dimid,varid(2))

          err = NF_PUT_ATT_DOUBLE(fidglobal(num_file),
     &         varid(2),'_FillValue',
     &         NF_DOUBLE,1, 0. _d 0 )

          err = NF_ENDDEF(fidadglobal(num_file))
          err = NF_CLOSE(fidadglobal(num_file))
          err = NF_OPEN(adfnameequincglo, NF_WRITE, 
     &         fidadglobal(num_file))

         ELSE
          err = NF_OPEN(adfnameequincglo, NF_WRITE, 
     &         fidadglobal(num_file))
         ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
         inquire( file=tanfnameequincglo, exist=exst )
         IF (.NOT.exst) THEN

          err = NF_CREATE(tanfnameequincglo,NF_CLOBBER,
     &           fidtanglobal(num_file))
          err = NF_DEF_DIM(fidtanglobal(num_file),'iOBS',
     &           ObsNo(num_file),dimid)

          err = NF_DEF_VAR(fidtanglobal(num_file),
     &         'mod_val',NF_DOUBLE,
     &         1,dimid,varid(1))

          err = NF_PUT_ATT_DOUBLE(fidtanglobal(num_file),
     &         varid(1),'_FillValue',
     &         NF_DOUBLE,1, 0. _d 0 )

          err = NF_DEF_VAR(fidglobal(num_file),
     &         'mod_mask',NF_DOUBLE,
     &         1,dimid,varid(2))

          err = NF_PUT_ATT_DOUBLE(fidglobal(num_file),
     &         varid(2),'_FillValue',
     &         NF_DOUBLE,1, 0. _d 0 )

          err = NF_ENDDEF(fidtanglobal(num_file))
          err = NF_CLOSE(fidtanglobal(num_file))
          err = NF_OPEN(tanfnameequincglo, NF_WRITE, 
     &         fidtanglobal(num_file))

         ELSE
          err = NF_OPEN(tanfnameequincglo, NF_WRITE, 
     &         fidtanglobal(num_file))
         ENDIF
#endif
        ENDIF

C Loop through obs 

c default values
        DO k=1,NOBSMAX_OBS
         obs_np(num_file,k)=1
         obs_ind_glob(num_file,k)=0
        ENDDO

c main loop: look for valid obs (within model run time)
        ObsNo_valid=0
        Obsno_div1000=max(0,int(ObsNo(num_file)/1000))

c counter to keep track of what sample number each obs starts
        sample_cnt=1
      
        DO kk=1,Obsno_div1000+1

         IF (min(ObsNo(num_file), 1000*kk).GE.
     &    1+1000*(kk-1)) THEN

c read a chunk
          vec_start=1+1000*(kk-1)
          vec_count=min(1000,ObsNo(num_file)-1000*(kk-1))

          IF ( (vec_count.LE.0).OR.(vec_count.GT.1000).OR.
     &     (vec_start.LE.0).OR.
     &     (vec_count+vec_start-1.GT.ObsNo(num_file)) )
     &    THEN
           IL  = ILNBLNK( obsfitFile )
           WRITE(msgBuf,'(3A)')
     &     'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &     '.nc was not read properly (case 1).'
           CALL PRINT_ERROR( msgBuf, myThid)
           stopObsfit=1
          ENDIF

c read time
          err = NF_GET_VARA_DOUBLE(fid,varid1a,vec_start,
     &     vec_count, tmpyymmdd)
          err = NF_GET_VARA_DOUBLE(fid,varid1b,vec_start,
     &     vec_count, tmphhmmss)
c read time interval
          IF (readDelT.GT.0) THEN      
           err = NF_GET_VARA_DOUBLE(fid,varid1c,vec_start,
     &      vec_count, tmpdelT)
          ENDIF
c read number of samples per obs
          IF (loopThroughSamples.EQ.1) THEN      
           err = NF_GET_VARA_DOUBLE(fid,varid0,vec_start,
     &      vec_count, tmpnp)
          ENDIF
          
          IF (err.NE.NF_NOERR) THEN
           WRITE(msgBuf,'(3A)')
     &     'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &     '.nc was not read properly (case 2).'
           CALL PRINT_ERROR( msgBuf, myThid)
           stopObsfit=1
          ENDIF

c if obsfitDoGenGrid then also read in the interpolation coeffs and indices
          IF (obsfitDoGenGrid) THEN
           err = NF_GET_VARA_DOUBLE(fid,varid_intp1,vec_start,
     &      vec_count, tmp_xC11)
           err = NF_GET_VARA_DOUBLE(fid,varid_intp2,vec_start,
     &      vec_count, tmp_yC11)
           err = NF_GET_VARA_DOUBLE(fid,varid_intp11,vec_start,
     &      vec_count, tmp_xCNINJ)
           err = NF_GET_VARA_DOUBLE(fid,varid_intp22,vec_start,
     &      vec_count, tmp_yCNINJ)
           DO q=1,NUM_INTERP_PTS_OBS
            vec_start2(1)=q
            vec_start2(2)=1+1000*(kk-1)
            vec_count2(1)=1
            vec_count2(2)=min(1000,ObsNo(num_file)-1000*(kk-1))
            err = NF_GET_VARA_DOUBLE(fid,varid_intp3,vec_start2,
     &      vec_count2, tmp_weights(1,q))
            err = NF_GET_VARA_DOUBLE(fid,varid_intp4,vec_start2,
     &      vec_count2, tmp_i(1,q))
            err = NF_GET_VARA_DOUBLE(fid,varid_intp5,vec_start2,
     &      vec_count2, tmp_j(1,q))
           ENDDO
          ENDIF

c loop through this chunk
          DO m=1,min(1000,ObsNo(num_file)-1000*(kk-1))
           IF ( stopObsfit .EQ. 0) THEN 
            obsIsInRunTime=1

CAV if obs starts before model run, will be ignored even if it ends after 
CAV model starts - ok for now      
            IF (( ( tmpyymmdd(m).GT.yymmddMin ).OR.(( tmpyymmdd(m).EQ.
     &        yymmddMin ).AND.( tmphhmmss(m).GE.hhmmssMin ))).AND.
     &      ( ( tmpyymmdd(m).LT.yymmddMax ).OR.(( tmpyymmdd(m).EQ.
     &        yymmddMax ).AND.( tmphhmmss(m).LE.hhmmssMax ))) ) THEN
             hh = int(tmphhmmss(m))/10000
             IF ( hh.LT.hoursPerDay ) THEN
              obsIsInRunTime=1
              CALL cal_FullDate( int(tmpyymmdd(m)),int(tmphhmmss(m)),
     &         tmpdate,myThid )
              CALL cal_TimePassed( modelstartdate,tmpdate,tmpdiff,
     &         myThid )
              CALL cal_ToSeconds (tmpdiff,diffsecs,myThid)
              diffsecs=diffsecs+nIter0*deltaTclock
             ELSE
c if tmphhmmss is out of range then disregard obs
              obsIsInRunTime=0
              diffsecs=-deltaTclock
              ObsNo_hh=ObsNo_hh+1
             ENDIF
            ELSE
             obsIsInRunTime=0
             diffsecs=-deltaTclock
            ENDIF

            IF (loopThroughSamples.EQ.1) THEN      
             np_cur = tmpnp(m)
            ELSE
             np_cur = 1
            ENDIF

            IF (readDelT.EQ.1) THEN      
             IF (tmpdelT(m).GT.0. _d 0) THEN      
c delT>0: time average
              delT_cur = tmpdelT(m)
              obsfitOperation(num_file)=1
             ELSEIF (tmpdelT(m).LT.0. _d 0) THEN
c delT<0: time integral      
              delT_cur = -tmpdelT(m)
              obsfitOperation(num_file)=2
             ELSE
c delT=0: instantaneous
              delT_cur = 0. _d 0
              obsfitOperation(num_file)=0
             ENDIF
            ELSE
             delT_cur = 0. _d 0
             obsfitOperation(num_file)=0
            ENDIF

            IF (obsIsInRunTime.EQ.1) THEN
c if yes then store obs index position and number of samples:
             ObsNo_valid=ObsNo_valid+1
             obs_ind_glob(num_file,ObsNo_valid)=m+1000*(kk-1)

c number of samples in each valid observation            
             obs_np(num_file,ObsNo_valid)=np_cur
             IF (np_cur.GT.obs_np_max) obs_np_max = np_cur
c position of first sample            
             obs_sample1_ind(num_file,ObsNo_valid)=sample_cnt
             obs_delT(num_file,ObsNo_valid) = delT_cur
      
            ENDIF !if (obsIsInRunTime.EQ.1) then
      
c regardless of whether obs is in run time or not, 
c keep track of sample index position
c and assign time to samples
            DO k=1,np_cur
             tmp_sample_time(sample_cnt)=diffsecs
             tmp_sample_delT(sample_cnt)=delT_cur
             sample_cnt=sample_cnt+1
            ENDDO

c check that maximum size was not reached:
            IF (ObsNo_valid.GT.NOBSMAX_OBS) THEN
             WRITE(msgBuf,'(3A)')
     &        'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &        '.nc was not read properly (increase NOBSMAX_OBS).'
             CALL PRINT_ERROR( msgBuf, myThid)
             stopObsfit=1
            ENDIF

c check that maximum NP was not reached:
            IF (obs_np_max.GT.NSAMP_PER_OBS_MAX) THEN
             WRITE(msgBuf,'(3A)')
     &        'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &        '.nc was not read properly (increase NSAMP_PER_OBS_MAX).'
             CALL PRINT_ERROR( msgBuf, myThid)
             stopObsfit=1
            ENDIF

           ENDIF
          ENDDO !m
         ENDIF
        ENDDO !kk

c number of valid observations in the file
        ObsNo(num_file)=ObsNo_valid

        WRITE(msgbuf,'(a,i9)')
     &     '  # of obs with erroneous HHMMSS values =',
     &     ObsNo_hh
        CALL print_message(
     &     msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

        WRITE(msgbuf,'(a,i9)')
     &     '  # of obs within time period  =',
     &     ObsNo(num_file)
        CALL print_message(
     &     msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

C loop through samples
        DO bj=1,nSy
         DO bi=1,nSx

c main loop: look for valid samples (in tile)
          err = NF_INQ_DIMID(fid,'iSAMPLE', dimid )
C if isample is not a dimension, use iobs for size of sampleno
          IF (err.NE.NF_NOERR) THEN
           SampleNo(num_file,bi,bj) = ObsNo(num_file)
          ELSE
           err = NF_INQ_DIMLEN(fid, dimid, SampleNo(num_file,bi,bj) )
          ENDIF
         
          DO k=1,NSAMPLESMAX
           sample_timeS(num_file,k,bi,bj)=-999. _d 0
           sample_timeE(num_file,k,bi,bj)=-999. _d 0
           sample_lon(num_file,k,bi,bj)=-999. _d 0
           sample_lat(num_file,k,bi,bj)=-999. _d 0
           sample_depth(num_file,k,bi,bj)=-999. _d 0
           sample_weight(num_file,k,bi,bj)=1. _d 0
           DO q = 1,NUM_INTERP_PTS_OBS
            sample_interp_i(num_file,k,q,bi,bj) = 1
            sample_interp_j(num_file,k,q,bi,bj) = 1
            sample_interp_k(num_file,k,q,bi,bj) = 1
            sample_interp_w(num_file,k,q,bi,bj) = 0. _d 0
           ENDDO
           sample_interp_xC11(num_file,k,bi,bj)=-999. _d 0
           sample_interp_yC11(num_file,k,bi,bj)=-999. _d 0
           sample_interp_xCNINJ(num_file,k,bi,bj)=-999. _d 0
           sample_interp_yCNINJ(num_file,k,bi,bj)=-999. _d 0
          ENDDO

          SampleNo_tile=0
          Obsno_div1000=max(0,int(SampleNo(num_file,bi,bj)/1000))
      
          ntype_ssh=0
          ntype_other=0

          DO kk=1,Obsno_div1000+1

           IF (min(sampleNo(num_file,bi,bj), 1000*kk).GE.
     &      1+1000*(kk-1)) THEN

c read a chunk
            vec_start=1+1000*(kk-1)
            vec_count=min(1000,SampleNo(num_file,bi,bj)-1000*(kk-1))

            IF ( (vec_count.LE.0).OR.(vec_count.GT.1000).OR.
     &        (vec_start.LE.0).OR.
     &        (vec_count+vec_start-1.GT.SampleNo(num_file,bi,bj)) )
     &        THEN
             IL  = ILNBLNK( obsfitFile )
             WRITE(msgBuf,'(3A)')
     &        'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &        '.nc was not read properly (case 3).'
             CALL PRINT_ERROR( msgBuf, myThid)
             stopObsfit=1
            ENDIF

            err = NF_GET_VARA_INT(fid,varid2,vec_start,
     &       vec_count, tmp_type2)
            err = NF_GET_VARA_DOUBLE(fid,varid3a,vec_start,
     &       vec_count, tmp_lon2)
            err = NF_GET_VARA_DOUBLE(fid,varid3b,vec_start,
     &       vec_count, tmp_lat2)
            err = NF_GET_VARA_DOUBLE(fid,varid4,vec_start,
     &       vec_count, tmp_depth2)
            IF (weighSamples.EQ.1) THEN  
             err = NF_GET_VARA_DOUBLE(fid,varid5,vec_start,
     &        vec_count, tmp_weight2)
            ENDIF
            
            IF (err.NE.NF_NOERR) THEN
             WRITE(msgBuf,'(3A)')
     &        'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &        '.nc was not read properly (case 4).'
             CALL PRINT_ERROR( msgBuf, myThid)
             stopObsfit=1
            ENDIF

C Loop through this chunk
            DO m=1,min(1000,SampleNo(num_file,bi,bj)-1000*(kk-1))
             IF ( stopObsfit .EQ. 0) THEN
      
              obsIsInRunTime=1
              sampleIsInTile=1
      
              IF (tmp_sample_time(m+1000*(kk-1)).LT.0. _d 0) THEN
               sampleIsInTile=0
              ENDIF

              IF (tmp_sample_time(m+1000*(kk-1)).GT.
     &                        modelend-modelstart) THEN
               sampleIsInTile=0
              ENDIF

              type_cur=tmp_type2(m)
              lon_cur=tmp_lon2(m)
              lat_cur=tmp_lat2(m)
              depth_cur=tmp_depth2(m)
              
C Determine whether sample is in current tile domain (lat-lon grid case)
              IF ((.NOT.obsfitDoGenGrid).AND.
     &          (sampleIsInTile.EQ.1)) THEN
               IF (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) THEN
                tmp_lon=xC(sNx+1,1,bi,bj)+360. _d 0
               ELSE
                tmp_lon=xC(sNx+1,1,bi,bj)
               ENDIF

               IF ((xC(1,1,bi,bj).LE.lon_cur).AND.
     &          (tmp_lon.GT.lon_cur).AND.
     &          (yC(1,1,bi,bj).LE.lat_cur).AND.
     &          (yC(1,sNy+1,bi,bj).GT.lat_cur) ) THEN
C Ddo nothing
               ELSEIF ((xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)).AND.
     &          (xC(1,1,bi,bj).LE.lon_cur+360. _d 0).AND.
     &          (tmp_lon.GT.lon_cur+360. _d 0).AND.
     &          (yC(1,1,bi,bj).LE.lat_cur).AND.
     &          (yC(1,sNy+1,bi,bj).GT.lat_cur)
     &          ) THEN
                lon_cur=lon_cur+360. _d 0         
               ELSE
C Not in this tile
                sampleIsInTile=0
               ENDIF
 
C Now determine value of i,j,k to the south-west-down of data point
               sample_i=-10
               sample_j=-10
               sample_k1=-10
               sample_k2=-10
               lon_1=-10
               lon_2=-10
               lat_1=-10
               lat_2=-10
               depth_1=-10
               depth_2=-10

               IF (sampleIsInTile.EQ.1) THEN
       
C Value of k, below the data point:
C case 1: above first depth level
                IF (-rC(1).GT.depth_cur) THEN
                 sample_k1=1
                 sample_k2=1
                 depth_fac=1. _d 0
                ENDIF
C case 2: below last depth level
                IF (-rC(Nr).LE.depth_cur) THEN
                 sample_k1=Nr
                 sample_k2=Nr
                 depth_fac=1. _d 0
                ENDIF
C case 3: between two depth levels
                DO k=1,Nr-1
                 IF ((-rC(k).LE.depth_cur).AND.
     &             (-rC(k+1).GT.depth_cur)) THEN
                  sample_k1=k
                  sample_k2=k+1
                  depth_1=-rC(k)
                  depth_2=-rC(k+1)
                  depth_fac=(depth_cur-depth_1)/(depth_2-depth_1)
                 ENDIF
                ENDDO
        
                DO j=1,sNy+1
                 DO i=1,sNx+1

                  IF (type_cur.EQ.3) THEN
C zonal velocity
                    lon_tmp1=xG(i,j,bi,bj)
                    lon_tmp2=xG(i+1,j,bi,bj)
                    lat_tmp1=yC(i,j,bi,bj)
                    lat_tmp2=yC(i,j+1,bi,bj)
                  ELSEIF (type_cur.EQ.4) THEN
c meridional velocity
                    lon_tmp1=xC(i,j,bi,bj)
                    lon_tmp2=xC(i+1,j,bi,bj)
                    lat_tmp1=yG(i,j,bi,bj)
                    lat_tmp2=yG(i,j+1,bi,bj)
                  ELSE
                    lon_tmp1=xC(i,j,bi,bj)
                    lon_tmp2=xC(i+1,j,bi,bj)
                    lat_tmp1=yC(i,j,bi,bj)
                    lat_tmp2=yC(i,j+1,bi,bj)
                  ENDIF
          
C value of j, south of the data point:
                  IF ((lat_tmp1.LE.lat_cur).AND.
     &             (lat_tmp2.GT.lat_cur)) THEN
                   sample_j=j
                   lat_1=lat_tmp1
                   lat_2=lat_tmp2
                  ENDIF

C value of i, west of the data point:
                  IF (xC(i+1,j,bi,bj).LT.xC(1,j,bi,bj)) THEN
                   lon_tmp2=lon_tmp2+360
                  ENDIF
                  IF (xC(i,j,bi,bj).LT.xC(1,j,bi,bj)) THEN
                   lon_tmp1=lon_tmp1+360
                  ENDIF

                  IF ((lon_tmp1.LE.lon_cur).AND.
     &             (lon_tmp2.GT.lon_cur)) THEN
                   sample_i=i
                   lon_1=lon_tmp1
                   lon_2=lon_tmp2
                  ENDIF

                 ENDDO
                ENDDO
               ENDIF

               IF ((sample_i.EQ.-10).OR.(sample_j.EQ.-10)) THEN
                sampleIsInTile=0
               ENDIF
               IF ((sample_k1.EQ.-10).OR.(sample_k2.EQ.-10)) THEN
                sampleIsInTile=0
               ENDIF

               IF (sampleIsInTile.EQ.1) THEN
C if yes then store sample index position and number of samples:
                SampleNo_tile=SampleNo_tile+1
                sample_ind_glob(num_file,SampleNo_tile,bi,bj)
     &           =m+1000*(kk-1)

                sample_timeS(num_file,SampleNo_tile,bi,bj)=
     &           tmp_sample_time(m+1000*(kk-1))
C "-1" to avoid counting in an extra time step 
C when time averaging
               IF (obsfitOperation(num_file).eq.0) THEN 
                sample_timeE(num_file,SampleNo_tile,bi,bj)=
     &           sample_timeS(num_file,SampleNo_tile,bi,bj)
     &           +tmp_sample_delT(m+1000*(kk-1))
               ELSE
                sample_timeE(num_file,SampleNo_tile,bi,bj)=
     &           sample_timeS(num_file,SampleNo_tile,bi,bj)
     &           +tmp_sample_delT(m+1000*(kk-1))-1
               ENDIF
                sample_lon(num_file,SampleNo_tile,bi,bj)=lon_cur
                sample_lat(num_file,SampleNo_tile,bi,bj)=lat_cur
                sample_depth(num_file,SampleNo_tile,bi,bj)=depth_cur
                IF (weighSamples.EQ.1) THEN  
                 sample_weight(num_file,SampleNo_tile,bi,bj)=
     &            tmp_weight2(m)
                ENDIF

                sample_type(num_file,SampleNo_tile,bi,bj)=type_cur
C Keep track of which variables in the file
              IF (type_cur.EQ.5) THEN
                 ntype_ssh=ntype_ssh+1
                ELSE
                 ntype_other=ntype_other+1
                ENDIF

C then store interpolation coeffs and indices
                lon_fac=(lon_cur-lon_1)/(lon_2-lon_1)
                lat_fac=(lat_cur-lat_1)/(lat_2-lat_1)
                
                w(1)=(1-lon_fac)*(1-lat_fac)*(1-depth_fac)
                sample_interp_i(num_file,SampleNo_tile,1,bi,bj)
     &              =sample_i
                sample_interp_j(num_file,SampleNo_tile,1,bi,bj)
     &              =sample_j
                sample_interp_k(num_file,SampleNo_tile,1,bi,bj)
     &              =sample_k1
                w(2)=lon_fac*(1-lat_fac)*(1-depth_fac)
                sample_interp_i(num_file,SampleNo_tile,2,bi,bj)
     &              =sample_i+1
                sample_interp_j(num_file,SampleNo_tile,2,bi,bj)
     &              =sample_j
                sample_interp_k(num_file,SampleNo_tile,2,bi,bj)
     &              =sample_k1
                w(3)=(1-lon_fac)*lat_fac*(1-depth_fac)
                sample_interp_i(num_file,SampleNo_tile,3,bi,bj)
     &              =sample_i
                sample_interp_j(num_file,SampleNo_tile,3,bi,bj)
     &              =sample_j+1
                sample_interp_k(num_file,SampleNo_tile,3,bi,bj)
     &              =sample_k1
                w(4)=lon_fac*lat_fac*(1-depth_fac)
                sample_interp_i(num_file,SampleNo_tile,4,bi,bj)
     &              =sample_i+1
                sample_interp_j(num_file,SampleNo_tile,4,bi,bj)
     &              =sample_j+1
                sample_interp_k(num_file,SampleNo_tile,4,bi,bj)
     &              =sample_k1
                w(5)=(1-lon_fac)*(1-lat_fac)*depth_fac
                sample_interp_i(num_file,SampleNo_tile,5,bi,bj)
     &              =sample_i
                sample_interp_j(num_file,SampleNo_tile,5,bi,bj)
     &              =sample_j
                sample_interp_k(num_file,SampleNo_tile,5,bi,bj)
     &              =sample_k2
                w(6)=lon_fac*(1-lat_fac)*depth_fac
                sample_interp_i(num_file,SampleNo_tile,6,bi,bj)
     &              =sample_i+1
                sample_interp_j(num_file,SampleNo_tile,6,bi,bj)
     &              =sample_j
                sample_interp_k(num_file,SampleNo_tile,6,bi,bj)
     &              =sample_k2
                w(7)=(1-lon_fac)*lat_fac*depth_fac
                sample_interp_i(num_file,SampleNo_tile,7,bi,bj)
     &              =sample_i
                sample_interp_j(num_file,SampleNo_tile,7,bi,bj)
     &              =sample_j+1
                sample_interp_k(num_file,SampleNo_tile,7,bi,bj)
     &              =sample_k2
                w(8)=lon_fac*lat_fac*depth_fac
                sample_interp_i(num_file,SampleNo_tile,8,bi,bj)
     &              =sample_i+1
                sample_interp_j(num_file,SampleNo_tile,8,bi,bj)
     &              =sample_j+1
                sample_interp_k(num_file,SampleNo_tile,8,bi,bj)
     &              =sample_k2
      
C ignore points that are in not "wet"

                IF (tmp_type.EQ.3) THEN
c zonal velocity
                 mask(1)=maskW(sample_i,sample_j,sample_k1,bi,bj)
                 mask(2)=maskW(sample_i+1,sample_j,sample_k1,bi,bj)
                 mask(3)=maskW(sample_i,sample_j+1,sample_k1,bi,bj)
                 mask(4)=maskW(sample_i+1,sample_j+1,sample_k1,bi,bj)
                 mask(5)=maskW(sample_i,sample_j,sample_k2,bi,bj)
                 mask(6)=maskW(sample_i+1,sample_j,sample_k2,bi,bj)
                 mask(7)=maskW(sample_i,sample_j+1,sample_k2,bi,bj)
                 mask(8)=maskW(sample_i+1,sample_j+1,sample_k2,bi,bj)
                ELSEIF (tmp_type.EQ.4) THEN
c meridional velocity
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
                ENDIF
                wtot=0
                DO q = 1,NUM_INTERP_PTS_OBS
                 IF (mask(q).EQ.0) w(q)=0
                 wtot=wtot+w(q)
                ENDDO
                DO q = 1,NUM_INTERP_PTS_OBS
                 sample_interp_w(num_file,SampleNo_tile,q,bi,bj)
     &               =w(q)/wtot
                ENDDO

               ENDIF


C Determine whether sample is in current tile domain (generic grid case)
               ELSEIF (obsIsInRunTime.EQ.1) THEN
c               ELSEIF (sampleIsInTile.EQ.1) THEN

                IF (stopGenericGrid.EQ.0) THEN

             IF ( ( abs( tmp_xC11(m) - xC(1,1,bi,bj) ).LT.0.0001 _d 0 )
     & .AND.( abs( tmp_yC11(m) - yC(1,1,bi,bj) ).LT.0.0001 _d 0) 
     & .AND.( abs( tmp_xCNINJ(m) - xC(sNx,sNy,bi,bj) ).LT.0.0001 _d 0 ) 
     & .AND.( abs( tmp_yCNINJ(m) - yC(sNx,sNy,bi,bj) ).LT.0.0001 _d 0 )
     & .AND.(obsIsInRunTime.EQ.1)) THEN
c     & .AND.(sampleIsInTile.EQ.1)) THEN

C if yes then store sample_time and interpolation coeffs and indices:
c determine value of k down of data point
                sample_k1 = -10
                sample_k2 = -10
                depth_1 = -10
                depth_2 = -10
c case 1: above first depth level
                IF (-rC(1).GT.depth_cur) THEN
                 sample_k1=1
                 sample_k2=1
                 depth_fac=1. _d 0
                ENDIF
c case 2: below last depth level
                IF (-rC(Nr).LE.depth_cur) THEN
                 sample_k1=Nr
                 sample_k2=Nr
                 depth_fac=1. _d 0
                ENDIF
c case 3: between two depth levels
                DO k=1,Nr-1
                IF ((-rC(k).LE.depth_cur).AND.
     &             (-rC(k+1).GT.depth_cur)) THEN
                 sample_k1=k
                 sample_k2=k+1
                 depth_1=-rC(k)
                 depth_2=-rC(k+1)
                 depth_fac=(depth_cur-depth_1)/(depth_2-depth_1)
                ENDIF
                ENDDO
c if yes then store sample_time and interpolation coeffs and indices:
       SampleNo_tile=SampleNo_tile+1
       sample_type(num_file,SampleNo_tile,bi,bj)=type_cur
c       sample_timeS(num_file,SampleNo_tile,bi,bj)=diffsecs

       sample_timeS(num_file,SampleNo_tile,bi,bj)=
     &           tmp_sample_time(m+1000*(kk-1))
cav "-1" to avoid counting in an extra time step 
cav when time averaging
       IF (obsfitOperation(num_file).eq.0) THEN
          sample_timeE(num_file,SampleNo_tile,bi,bj)=
     &     sample_timeS(num_file,SampleNo_tile,bi,bj)
     &     +tmp_sample_delT(m+1000*(kk-1))
       ELSE
          sample_timeE(num_file,SampleNo_tile,bi,bj)=
     &     sample_timeS(num_file,SampleNo_tile,bi,bj)
     &     +tmp_sample_delT(m+1000*(kk-1))-1
       ENDIF
       sample_interp_xC11(num_file,SampleNo_tile,bi,bj)=tmp_xC11(m)
       sample_interp_yC11(num_file,SampleNo_tile,bi,bj)=tmp_yC11(m)
       sample_interp_xCNINJ(num_file,SampleNo_tile,bi,bj)=tmp_xCNINJ(m)
       sample_interp_yCNINJ(num_file,SampleNo_tile,bi,bj)=tmp_yCNINJ(m)
       tmp_sum_weights=0. _d 0
            DO q = 1,NUM_INTERP_PTS_OBS
             sample_interp_w(num_file,SampleNo_tile,q,bi,bj)
     &       =tmp_weights(m,q)
             sample_interp_i(num_file,SampleNo_tile,q,bi,bj)
     &       =tmp_i(m,q)
             sample_interp_j(num_file,SampleNo_tile,q,bi,bj)
     &       =tmp_j(m,q)
             tmp_sum_weights=tmp_sum_weights+tmp_weights(m,q)

C more test of the inputs: is the offline-computed
C interpolation information consistent (self and with grid)
              IF ( (tmp_i(m,q).LT.0).OR.(tmp_j(m,q).LT.0)
     & .OR.(tmp_i(m,q).GT.sNx+1).OR.(tmp_j(m,q).GT.sNy+1) ) THEN
               WRITE(msgBuf,'(4A)')
     &     'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'points (obsfitDoGenGrid; out of tile)'
               CALL PRINT_ERROR( msgBuf, myThid)
               stopGenericGrid=1
              ENDIF
#ifdef ALLOW_OBSFIT_EXCLUDE_CORNERS
              IF ( tmp_weights(m,q) .NE. 0. _d 0) THEN
               IF ( ((tmp_i(m,q).EQ.0).AND.(tmp_j(m,q).EQ.0))
     & .OR.((tmp_i(m,q).EQ.sNx+1).AND.(tmp_j(m,q).EQ.sNy+1))
     & .OR.((tmp_i(m,q).EQ.0).AND.(tmp_j(m,q).EQ.sNy+1))
     & .OR.((tmp_i(m,q).EQ.sNx+1).AND.(tmp_j(m,q).EQ.0)) ) THEN
                WRITE(msgBuf,'(4A)')
     &     'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'points (obsfitDoGenGrid; using overlap corners)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1
               ENDIF
              ENDIF
#endif /* ALLOW_OBSFIT_EXCLUDE_CORNERS */
              IF ( (tmp_weights(m,q).LT.0. _d 0).OR.
     &    (tmp_weights(m,q).GT.1. _d 0) ) THEN
               WRITE(msgBuf,'(4A)')
     &     'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'weights (obsfitDoGenGrid; sum oustide 0-1)'
               CALL PRINT_ERROR( msgBuf, myThid)
               stopGenericGrid=1
              ENDIF

             ENDDO

             IF ( abs(tmp_sum_weights -1. _d 0 ) .GT. 0.0001 _d 0) THEN
              WRITE(msgBuf,'(4A)')
     &     'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'weights (obsfitDoGenGrid; dont add up to 1)'
              CALL PRINT_ERROR( msgBuf, myThid)
              stopGenericGrid=1
             ENDIF

             sample_ind_glob(num_file,SampleNo_tile,bi,bj)
     &        =m+1000*(kk-1)

                ENDIF
               ENDIF
              ENDIF   !if (.NOT.obsfitDoGenGrid) then

c ==============================================================================
 
C Check that maximum size was not reached:
              IF (SampleNo_tile.GE.NSAMPLESMAX) then
               WRITE(msgBuf,'(3A)')
     &          'OBSFIT_INIT_FIXED: file ', obsfitFile(1:IL),
     &          '.nc was not read properly (increase NSAMPLESMAX).'
               CALL PRINT_ERROR( msgBuf, myThid)
               stopObsfit=1
              ENDIF

             ENDIF     
            ENDDO !m
           ENDIF
          ENDDO !kk

C Number of valid samples in the file
          SampleNo(num_file,bi,bj)=SampleNo_tile
       
          WRITE(msgbuf,'(a,i4,a,i4)')
     &       '  current tile is bi,bj                      =',
     &       bi,',',bj
          CALL print_message(
     &       msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

          WRITE(msgbuf,'(a,i9)')
     &       '  # of samples within tile and time period  =',
     &       SampleNo(num_file,bi,bj)
          CALL print_message(
     &       msgbuf, standardmessageunit, SQUEEZE_RIGHT, myThid)

         IF (ntype_ssh.GT.0) THEN
          IF (ntype_other.GT.0) THEN
CAV add error message
          ELSE
           obs_is_ssh_loc=obs_is_ssh_loc+1
          ENDIF
         ENDIF

C===========================================================
C Create files for model counterparts to samples
C===========================================================

          IF (SampleNo(num_file,bi,bj).GT.0) THEN
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles

           JL  = ILNBLNK( obsfitDir )

           IF (obsfitDoNcOutput) THEN

            WRITE(fnameequinc,'(3a,i3.3,a,i3.3,a)')
     &       obsfitDir(1:JL),obsfitFile(1:IL),'.',iG,'.',jG,'.equi.nc'
            WRITE(adfnameequinc,'(4a,i3.3,a,i3.3,a)')
     &       obsfitDir(1:JL),'ad',
     &       obsfitFile(1:IL),'.',iG,'.',jG,'.equi.nc'
            WRITE(tanfnameequinc,'(4a,i3.3,a,i3.3,a)')
     &       obsfitDir(1:JL),'tl',
     &       obsfitFile(1:IL),'.',iG,'.',jG,'.equi.nc'

            inquire( file=fnameequinc, exist=exst )
            IF (.NOT.exst) THEN
      
             CALL obsfit_init_equifiles(num_file,
     &        fiddata_obs(num_file),fnameequinc,
     &        fidfwd_obs(num_file,bi,bj),SampleNo(num_file,bi,bj),
     &        bi,bj,myThid)
     
            ELSE
      
             err = NF_OPEN(fnameequinc,NF_WRITE,
     &         fidfwd_obs(num_file,bi,bj))
            ENDIF
#ifdef ALLOW_ADJOINT_RUN
            inquire( file=adfnameequinc, exist=exst )
            IF (.NOT.exst) THEN
             CALL obsfit_init_equifiles(num_file,fiddata_obs(num_file),
     &        adfnameequinc, fidadj_obs(num_file,bi,bj),
     &        SampleNo(num_file,bi,bj),
     &        bi,bj,myThid)
            ELSE
             err = NF_OPEN(adfnameequinc,NF_WRITE,
     &        fidadj_obs(num_file,bi,bj))
            ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
            inquire( file=tanfnameequinc, exist=exst )
            IF (.NOT.exst) THEN
             CALL obsfit_init_equifiles(num_file,fiddata_obs(num_file),
     &        tanfnameequinc, fidtan_obs(num_file,bi,bj),
     &        SampleNo(num_file,bi,bj),
     &        bi,bj,myThid)
            ELSE
             err = NF_OPEN(tanfnameequinc,NF_WRITE,
     &        fidtan_obs(num_file,bi,bj))
            ENDIF
#endif
           ELSE

            WRITE(fnameequinc,'(3a,i3.3,a,i3.3,a)')
     &        obsfitDir(1:JL),obsfitFile(1:IL),'.',iG,'.',jG,
     &        '.equi.data'
            WRITE(adfnameequinc,'(4a,i3.3,a,i3.3,a)')
     &         obsfitDir(1:JL),'ad',
     &         obsfitFile(1:IL),'.',iG,'.',jG,'.equi.data'
            WRITE(tanfnameequinc,'(4a,i3.3,a,i3.3,a)')
     &         obsfitDir(1:JL),'tl',
     &         obsfitFile(1:IL),'.',iG,'.',jG,'.equi.data'

            inquire( file=fnameequinc, exist=exst )
#ifdef OBSFIT_USE_MDSFINDUNITS
            CALL MDSFINDUNIT( fidfwd_obs(num_file,bi,bj), myThid )
#else
            CALL OBSFIT_FINDUNIT( fidfwd_obs(num_file,bi,bj), myThid )
#endif
            IF (.NOT.exst) THEN
             CALL obsfit_init_equifiles(num_file,fiddata_obs(num_file),
     &        fnameequinc,fidfwd_obs(num_file,bi,bj),
     &        SampleNo(num_file,bi,bj),
     &        bi,bj,myThid)
            ELSE
             open( fidfwd_obs(num_file,bi,bj),file=fnameequinc,
     &        form ='unformatted',status='unknown', access='direct',
     &        recl=  2*WORDLENGTH*2 )
            ENDIF
#ifdef ALLOW_ADJOINT_RUN
            inquire( file=adfnameequinc, exist=exst )
#ifdef OBSFIT_USE_MDSFINDUNITS
            CALL MDSFINDUNIT( fidadj_obs(num_file,bi,bj), myThid )
#else
            CALL OBSFIT_FINDUNIT( fidadj_obs(num_file,bi,bj), myThid )
#endif
            IF (.NOT.exst) THEN

             CALL obsfit_init_equifiles(num_file,fiddata_obs(num_file),
     &         adfnameequinc, fidadj_obs(num_file,bi,bj),
     &         SampleNo(num_file,bi,bj),
     &         bi,bj,myThid)
            ELSE
             open( fidadj_obs(num_file,bi,bj),file=adfnameequinc,
     &         form ='unformatted',status='unknown', access='direct',
     &         recl=  2*WORDLENGTH*2 )
            ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
            inquire( file=tanfnameequinc, exist=exst )
#ifdef OBSFIT_USE_MDSFINDUNITS
            CALL MDSFINDUNIT( fidtan_obs(num_file,bi,bj), myThid )
#else
            CALL OBSFIT_FINDUNIT( fidtan_obs(num_file,bi,bj), myThid )
#endif
            IF (.NOT.exst) THEN

             CALL obsfit_init_equifiles(num_file,fiddata_obs(num_file),
     &         tanfnameequinc, fidtan_obs(num_file,bi,bj),
     &         SampleNo(num_file,bi,bj),
     &         bi,bj,myThid)
            ELSE
             open( fidtan_obs(num_file,bi,bj),file=tanfnameequinc,
     &         form ='unformatted',status='unknown', access='direct',
     &         recl=  2*WORDLENGTH*2 )
            ENDIF
#endif

           ENDIF

          ENDIF

         ENDDO !bj
        ENDDO !bi

        CALL GLOBAL_SUM_INT(obs_is_ssh_loc, myThid)
        obs_is_ssh(num_file)=obs_is_ssh_loc

C===========================================================
       ELSE

        DO bj=1,nSy
         DO bi=1,nSx
          ObsNo(num_file)=0
          SampleNo(num_file,bi,bj)=0
          DO k=1,NOBSMAX_OBS
           obs_np(num_file,k)=-999. _d 0
          ENDDO
          DO k=1,NSAMPLESMAX
           sample_timeS(num_file,k,bi,bj)=-999. _d 0
           sample_timeE(num_file,k,bi,bj)=-999. _d 0
           sample_lon(num_file,k,bi,bj)=-999. _d 0
           sample_lat(num_file,k,bi,bj)=-999. _d 0
           sample_depth(num_file,k,bi,bj)=-999. _d 0
           sample_ind_glob(num_file,k,bi,bj)=0
           DO q = 1,NUM_INTERP_PTS_OBS
            sample_interp_i(num_file,k,q,bi,bj) = 1
            sample_interp_j(num_file,k,q,bi,bj) = 1
            sample_interp_k(num_file,k,q,bi,bj) = 1
            sample_interp_w(num_file,k,q,bi,bj) = 0. _d 0
           ENDDO
           sample_interp_xC11(num_file,k,bi,bj)=-999. _d 0
           sample_interp_yC11(num_file,k,bi,bj)=-999. _d 0
           sample_interp_xCNINJ(num_file,k,bi,bj)=-999. _d 0
           sample_interp_yCNINJ(num_file,k,bi,bj)=-999. _d 0
          ENDDO
         ENDDO !bj
        ENDDO !bi

       ENDIF !if (IL.NE.0) then
      ENDDO !      do num_file=1,NFILESMAX_OBS

C===========================================================
C error cases:
C===========================================================

c1) you want to provide interpolation information

C For generic grid. Needs to go inside bi,bj loop
       IF ( stopGenericGrid.EQ.2) THEN
c         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
c         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
ccgf XC grid
c       CALL MDSFINDUNIT( fid , myThid )
c       WRITE(fnameequinc(1:80),'(a,i3.3,a,i3.3,a,i4.4,a,i4.4,a)')
c     & 'profilesXCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
c         k=MDS_RECLEN(64,(sNx+2)*(sNy+2),myThid)
c            WRITE(standardMessageUnit,'(A,/,2A)')
c     & 'OBSFIT_INIT_FIXED: creating grid from profiles; file:',
c     & fnameequinc
c       open( fid, file= fnameequinc, form ='unformatted',
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
c       WRITE(fnameequinc(1:80),'(a,i3.3,a,i3.3,a,i4.4,a,i4.4,a)')
c     & 'profilesYCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
c         k=MDS_RECLEN(64,(sNx+2)*(sNy+2),myThid)
c            WRITE(standardMessageUnit,'(A,/,A)')
c     & 'OBSFIT_INIT_FIXED: creating grid from profiles; file:',
c     & fnameequinc
c       open( fid, file= fnameequinc, form ='unformatted',
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

       WRITE(msgBuf,'(3A)')
     & 'OBSFIT_INIT_FIXED : ',
     & 'when using obsfitDoGenGrid ',
     & 'you have to provide interpolation coeffs etc. '
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(2A)')
     & 'and some of your nc files dont have them. ',
     & 'You could use profiles_prep_mygrid.m and/or'
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(A)')
     & 'use the grid info in profiles*incl1PointOverlap*data'
       CALL PRINT_ERROR( msgBuf, myThid)
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

      WRITE(msgbuf,'(a)') ' '
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)')
     &'// ======================================================='
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)')
     &'// obsfit model sampling >>> END <<<'
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)')
     &'// ======================================================='
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)
      WRITE(msgbuf,'(a)') ' '
      CALL print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid)

#endif

C     ==================================================================

      RETURN
      END
