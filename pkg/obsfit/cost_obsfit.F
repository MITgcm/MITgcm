#include "OBSFIT_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: COST_OBSFIT

C !INTERFACE:
      SUBROUTINE COST_OBSFIT(
     I                     myIter, myTime, myThid )

C !DESCRIPTION:
C Computes the cost function for obsfit data

C !USES:
      IMPLICIT NONE
C == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
# include "netcdf.inc"
#endif
#ifdef ALLOW_CTRL
# include "OPTIMCYCLE.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
#endif

C !INPUT PARAMETERS:
C myTime    :: Current time in simulation
C myIter    :: Current time-step number
C myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_OBSFIT
C !LOCAL VARIABLES:
C bi,bj     :: Tile indices
      INTEGER num_file,sample_num
      INTEGER bi,bj,iG,jG
      _RL sample_modval
      _RL obs_modval
      _RL obs_data, obs_uncert, obs_weight
#ifndef ALLOW_CTRL
      INTEGER optimcycle
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) obsfitfile, fnameequinc
      CHARACTER*(MAX_LEN_FNAM) fnamemisfit
      INTEGER IL, JL, err
      INTEGER irec, ii, varid
      INTEGER obs_num, num_valid_samples
      _RL sample_mask_sum
      _RL objf_obsfit_glo
      _RL num_obsfit_glo
      _RL samples_buff(NSAMPLESMAX)
      _RL samples_mask_buff(NSAMPLESMAX)
      _RL samples_modval_glob(NSAMPLESMAX)
      _RL samples_mask_glob(NSAMPLESMAX)
      _RL tmpgs
      INTEGER nmodmean, nobsmean
      _RL offset, mod_mean, obs_mean, misfit


C !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &  standardMessageUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(a)') '== cost_obsfit: begin =='
      CALL PRINT_MESSAGE( msgBuf,
     &  standardMessageUnit,SQUEEZE_RIGHT, myThid)

      _BEGIN_MASTER( myThid )

      DO num_file=1,NFILESMAX_OBS

C File maintenance
       DO bj=1,nSy
        DO bi=1,nSx

         IF ( (sampleNo(num_file,bi,bj).GT.0).AND.
     &         (obsfitDoNcOutput) ) THEN
C need to close the file so that the data is not lost when run finishes
           err = NF_CLOSE(fidfwd_obs(num_file,bi,bj))
C then re-open it to compute cost function
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
           IL  = ILNBLNK( obsfitfiles(num_file) )
           write(obsfitfile,'(1a)')
     &     obsfitfiles(num_file)(1:IL)
           IL  = ILNBLNK( obsfitfile )
           JL  = ILNBLNK( obsfitDir )
           write(fnameequinc,'(3a,i3.3,a,i3.3,a)')
     &     obsfitDir(1:JL),obsfitfile(1:IL),'.',iG,'.',jG,'.equi.nc'
           err = NF_OPEN(fnameequinc,NF_NOWRITE,
     &     fidfwd_obs(num_file,bi,bj))
         ENDIF

        ENDDO
       ENDDO


C Loop over samples
       DO ii=1,NSAMPLESMAX
        samples_buff(ii) = 0.
        samples_mask_buff(ii) = 0.
       ENDDO

       DO bj=1,nSy
        DO bi=1,nSx

C Open tiled files and read to buffer
         DO sample_num=1,NSAMPLESMAX
          IF (sample_num.LE.sampleNo(num_file,bi,bj)) THEN

            sample_modval=0.

            CALL ACTIVE_READ_OBS_TILE(num_file,
     &           sample_modval,
     &           sample_num,.false.,optimcycle,bi,bj,myThid,
     &           obsfit_dummy(num_file,bi,bj))

C Save model equi (of samples) and masks in buffer
C Combine all threads here
             irec = sample_ind_glob(num_file,sample_num,bi,bj)
             samples_buff(irec) = samples_buff(irec)
     &                                 +sample_modval
             samples_mask_buff(irec) = samples_mask_buff(irec)
     &                                 +sample_modmask(bi,bj)

          ENDIF !if (sample_num.LE.sampleNo(num_file,bi,bj)) then
         ENDDO !do sample_num=..

        ENDDO !bj
       ENDDO !bi

C Combine all processes
       DO ii=1,NSAMPLESMAX
        tmpgs = samples_buff(ii)
        _GLOBAL_SUM_RL(tmpgs, myThid)
        samples_modval_glob(ii) = tmpgs
        tmpgs = samples_mask_buff(ii)
        _GLOBAL_SUM_RL(tmpgs, myThid)
        samples_mask_glob(ii) = tmpgs
       ENDDO

       IF ( myProcId .eq. 0 ) THEN

C Loop over obs
        DO obs_num=1,NOBSMAX_OBS
         IF (obs_num.LE.ObsNo(num_file)) THEN

          obs_modval=0.
          sample_mask_sum=0
          num_valid_samples=0

C Calculate model equi of each obs by averaging NP samples
          DO sample_num=1,NSAMP_PER_OBS_MAX
           IF (sample_num.LE.obs_np(num_file,obs_num)) THEN

            irec = obs_sample1_ind(num_file,obs_num) + sample_num-1
            obs_modval = obs_modval + samples_modval_glob(irec)
     &           *samples_mask_glob(irec)
            sample_mask_sum = sample_mask_sum + samples_mask_glob(irec)
            IF (samples_mask_glob(irec) .GT. 0. _d 0) THEN
             num_valid_samples = num_valid_samples+1
            ENDIF

           ENDIF
          ENDDO

C Time averaging
          IF (obsfitOperation(num_file).eq.1) THEN
           obs_modval = obs_modval/obs_delT(num_file, obs_num)
          ENDIF

C Spatial averaging
          IF (num_valid_samples.gt.0) THEN
           obs_modval = obs_modval/sample_mask_sum
           obs_modmask = 1
          ELSE
           obs_modval = -9999.
           obs_modmask = 0
          ENDIF

          IF (obs_modval.eq.-9999) THEN
           obs_modmask = 0
          ENDIF

C Write to global netcdf file
          CALL ACTIVE_WRITE_OBS_GLOB(num_file,
     &         obs_modval,
     &         obs_num,optimcycle,myThid,
     &         obsfit_globaldummy(num_file))

         ENDIF !if (obs_num.LE.ObsNo(num_file)) then
        ENDDO !do obs_num=..

        err = NF_CLOSE(fidglobal(num_file))
        IL  = ILNBLNK( obsfitfiles(num_file) )
        write(obsfitfile,'(1a)') obsfitfiles(num_file)(1:IL)
        IL  = ILNBLNK( obsfitfile )
        JL  = ILNBLNK( obsfitDir )
        write(fnameequinc,'(3a)')
     &      obsfitDir(1:JL),obsfitfile(1:IL),'.equi.nc'
        err = NF_OPEN(fnameequinc,NF_NOWRITE,fidglobal(num_file))

        IF (obs_is_ssh(num_file).GT.0) THEN
C Read data to calculate the mean offset between model and obs
         offset=0.
         mod_mean=0.
         obs_mean=0.
         nmodmean=0
         nobsmean=0.

C Loop over obs
         DO obs_num=1,NOBSMAX_OBS
          IF (obs_num.LE.ObsNo(num_file)) THEN

           obs_modval=0.
           obs_data=0.
           obs_uncert=0.

C Read observation and uncertainty
           CALL OBSFIT_READ_OBS(num_file,1,
     &           obs_ind_glob(num_file,obs_num),
     &           obs_data,myThid)

           CALL OBSFIT_READ_OBS(num_file,-1,
     &           obs_ind_glob(num_file,obs_num),
     &           obs_uncert,myThid)

           IF (obs_data.GT.-9999) THEN
            obs_mean=obs_mean+obs_data
            nobsmean=nobsmean+1
           ENDIF
           IF (obs_modval.GT.-9999) THEN
            mod_mean=mod_mean+obs_modval
            nmodmean=nmodmean+1
           ENDIF

          ENDIF !if (obs_num.LE.ObsNo(num_file)) then
         ENDDO !do obs_num...
         mod_mean=mod_mean/nmodmean
         obs_mean=obs_mean/nobsmean
         offset=mod_mean-obs_mean
        ELSE
         offset=0.
        ENDIF

C Read data to calculate the cost
C and write misfits to global file

C Global file for misfits
        IL  = ILNBLNK( obsfitfiles(num_file) )
        WRITE(obsfitfile,'(1a)')
     &      obsfitfiles(num_file)(1:IL)
        IL  = ILNBLNK( obsfitfile )
        JL  = ILNBLNK( obsfitDir )
        WRITE(fnamemisfit,'(3a)')
     &      obsfitDir(1:JL),obsfitfile(1:IL),'.misfit.nc'
        err = NF_OPEN(fnamemisfit,NF_WRITE,fidmisfit(num_file))
        err = NF_INQ_VARID(fidmisfit(num_file),'misfit',varid)
C Loop over obs
        DO obs_num=1,NOBSMAX_OBS
         IF (obs_num.LE.ObsNo(num_file)) THEN

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE obs_modmask         = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE obsfit_globaldummy  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif
          obs_modval=0.
          obs_data  =0.
          obs_uncert=0.

C Read observation and uncertainty
          CALL OBSFIT_READ_OBS(num_file,1,
     &           obs_ind_glob(num_file,obs_num),
     &           obs_data,myThid)

          CALL OBSFIT_READ_OBS(num_file,-1,
     &           obs_ind_glob(num_file,obs_num),
     &           obs_uncert,myThid)

          IF (obs_data.eq.-9999) THEN
           obs_uncert=0
          ENDIF

C Read model equivalent from global file
          CALL ACTIVE_READ_OBS_GLOB(num_file,
     &         obs_modval,
     &         obs_num,.FALSE.,optimcycle,myThid,
     &         obsfit_globaldummy(num_file))

          IF (obs_uncert.GT.0.) THEN
           obs_weight = 1. _d 0 / (obs_uncert*obs_uncert)
           misfit     = obs_modval-obs_data-offset
           objf_obsfit(num_file) = objf_obsfit(num_file)
     &          + obs_modmask* obs_weight * misfit * misfit
           num_obsfit(num_file)  = num_obsfit(num_file)
     &          + obs_modmask
C Write misfit to global netcdf file
           err = NF_PUT_VARA_DOUBLE(fidmisfit(num_file),varid,
     &          obs_ind_glob(num_file,obs_num),1,misfit)
          ENDIF

         ENDIF !if (obs_num.LE.ObsNo(num_file)) then
        ENDDO !do obs_num...

        err = NF_CLOSE(fidmisfit(num_file))

       ENDIF !if myprocid = 0

      ENDDO !do num_file=1,NFILESMAX_OBS

      _END_MASTER( myThid )

C Print cost function values
      DO num_file=1,NFILESMAX_OBS

       objf_obsfit_glo = objf_obsfit(num_file)
       num_obsfit_glo  = num_obsfit(num_file)

       WRITE(msgBuf,'(a,I2,a,2D12.5)')
     &  ' cost_obsfit(',num_file,')= ', objf_obsfit_glo, num_obsfit_glo

       IF ( num_obsfit_glo .GT. 0. ) CALL PRINT_MESSAGE( msgBuf,
     &  standardMessageUnit,SQUEEZE_RIGHT, myThid)

      ENDDO

      WRITE(msgBuf,'(a)') '== cost_obsfit: end   =='
      CALL PRINT_MESSAGE( msgBuf,
     &  standardMessageUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &  standardMessageUnit,SQUEEZE_RIGHT, myThid)

#endif /* ALLOW_OBSFIT */

      RETURN
      END
