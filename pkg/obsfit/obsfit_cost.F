#include "OBSFIT_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OBSFIT_COST

C     !INTERFACE:
      SUBROUTINE OBSFIT_COST( myTime, myIter, myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Computes the cost function for ObsFit data
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

C     !INPUT PARAMETERS:
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: my thread ID number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OBSFIT
C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER num_file,sample_num
      INTEGER bi,bj
      _RL sample_modval
      _RL obs_modval, obs_modmask
      _RL obs_data, obs_uncert, obs_weight
#ifndef ALLOW_CTRL
      INTEGER optimcycle
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) obsfitfile
      CHARACTER*(MAX_LEN_FNAM) fnamemisfit
      INTEGER IL, JL, err
      INTEGER irec, ii, varID
      INTEGER obs_num, num_valid_samples
      _RL sample_mask_sum
      _RL objf_obsfit_glo
      _RL num_obsfit_glo
      _RL samples_buff(NSAMPLESMAX)
      _RL samples_mask_buff(NSAMPLESMAX)
      _RL samples_modval_glob(NSAMPLESMAX)
      _RL samples_mask_glob(NSAMPLESMAX)
      _RL tmpgs
      INTEGER nobsmean
      _RL offset, mod_mean, obs_mean, misfit
      _RL spval
      PARAMETER ( spval = -9999. _d 0 )

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

      WRITE( msgBuf,'(A)' ) ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit,SQUEEZE_RIGHT, myThid )
      WRITE( msgBuf,'(A)' ) '== obsfit_cost: begin =='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit,SQUEEZE_RIGHT, myThid )

C Initialise local storage
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tapelev_obsfit = COMMON, NFILESMAX_OBS
#endif

      _BEGIN_MASTER( myThid )

      DO num_file = 1, NFILESMAX_OBS

C File maintenance
        DO bj = 1, nSy
          DO bi= 1, nSx

            IF ( ( sampleNo(num_file,bi,bj).GT.0 ).AND.
     &           (obsfitDoNcOutput) ) THEN
C Need to sync the file so that the data is not lost when run finishes
              err = NF_SYNC( fidfwd_obs(num_file,bi,bj) )
              CALL OBSFIT_NF_ERROR( 'OBSFIT_COST: NF_SYNC fidfwd_obs',
     &             err,bi,bj,myThid )
            ENDIF

          ENDDO
        ENDDO

C Loop over samples
        DO ii = 1, NSAMPLESMAX
          samples_buff(ii)      = zeroRL
          samples_mask_buff(ii) = zeroRL
        ENDDO

        DO bj = 1, nSy
          DO bi = 1, nSx

C Open tiled files and read to buffer
            DO sample_num = 1, NSAMPLESMAX
              IF ( sample_num.LE.sampleNo(num_file,bi,bj) ) THEN

                sample_modval = zeroRL

                CALL ACTIVE_READ_OBS_TILE(
     I               num_file,
     O               sample_modval,
     I               sample_num,.false.,
     I               optimcycle,bi,bj,myThid,
     I               obsfit_dummy(num_file,bi,bj) )

C Save model equi (of samples) and masks in buffer
C Combine all threads here
                irec = sample_ind_glob(num_file,sample_num,bi,bj)
                samples_buff(irec) = samples_buff(irec)+sample_modval
                samples_mask_buff(irec) = samples_mask_buff(irec)
     &           +sample_modmask(bi,bj)

              ENDIF !IF (sample_num.LE.sampleNo(num_file,bi,bj))
            ENDDO !DO sample_num

          ENDDO !DO bj
        ENDDO !DO bi

C Combine all processes
        DO ii = 1, NSAMPLESMAX
          tmpgs = samples_buff(ii)
          _GLOBAL_SUM_RL(tmpgs, myThid)
          samples_modval_glob(ii) = tmpgs
          tmpgs = samples_mask_buff(ii)
          _GLOBAL_SUM_RL(tmpgs, myThid)
          samples_mask_glob(ii) = tmpgs
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
C     Not really necessary, but cheap (for small NFILESMAX_OBS)
C     and avoids some hidden recomputations.
CADJ STORE samples_modval_glob, samples_mask_glob
CADJ &     = tapelev_obsfit, key = num_file, kind = isbyte
#endif

        IF ( myProcId .EQ. 0 ) THEN

C Loop over obs
          DO obs_num = 1, NOBSMAX_OBS
            IF ( obs_num.LE.ObsNo(num_file) ) THEN

              obs_modval        = zeroRL
              sample_mask_sum   = zeroRL
              num_valid_samples = 0

C Calculate model equi of each obs by averaging NP samples
              DO sample_num = 1, NSAMP_PER_OBS_MAX
                IF ( sample_num.LE.obs_np(num_file,obs_num) ) THEN
                  irec = obs_sample1_ind(num_file,obs_num)+sample_num-1
                  obs_modval = obs_modval+samples_modval_glob(irec)
     &             *samples_mask_glob(irec)
                  sample_mask_sum = sample_mask_sum
     &             +samples_mask_glob(irec)
                  IF ( samples_mask_glob(irec).GT.zeroRL ) THEN
                    num_valid_samples = num_valid_samples+1
                  ENDIF

                ENDIF
              ENDDO

C Time averaging
              IF ( obsfitOperation(num_file).EQ.1 ) THEN
                obs_modval = obs_modval/obs_delT(num_file, obs_num)
              ENDIF

C Spatial averaging
              IF (sample_mask_sum.GT.zeroRL) THEN
                obs_modval = obs_modval/sample_mask_sum
                obs_modmask = oneRL
              ELSE
                obs_modval = spval
                obs_modmask = zeroRL
              ENDIF

C Write to global netcdf file
              CALL ACTIVE_WRITE_OBS_GLOB(
     I             num_file, obs_modval, obs_modmask, obs_num,
     I             optimcycle, myThid, obsfit_globaldummy(num_file) )

            ENDIF !IF (obs_num.LE.ObsNo(num_file))
          ENDDO !DO obs_num
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE obsfit_globaldummy(num_file)
CADJ &     = tapelev_obsfit, key = num_file, kind = isbyte
#endif

C Need to sync the file so that the data is not lost when run finishes
          err = NF_SYNC( fidglobal(num_file) )
          CALL OBSFIT_NF_ERROR(
     &         'OBSFIT_COST: NF_SYNC fidglobal',err,0,0,myThid )
          IF ( obs_is_ssh(num_file).GT.0 ) THEN
C Read data to calculate the mean offset between model and obs
            offset   = zeroRL
            mod_mean = zeroRL
            obs_mean = zeroRL
            nobsmean = 0

C Loop over obs
            DO obs_num = 1, NOBSMAX_OBS
              IF ( obs_num.LE.ObsNo(num_file) ) THEN

                obs_modval = zeroRL
                obs_data   = zeroRL
                obs_uncert = zeroRL

C Read model equivalent from global file
                CALL ACTIVE_READ_OBS_GLOB(
     I               num_file,
     O               obs_modval, obs_modmask,
     I               obs_num, .FALSE., optimcycle, myThid,
     I               obsfit_globaldummy(num_file) )

C Read observation and uncertainty
                CALL OBSFIT_READ_OBS(
     I               num_file,1,obs_ind_glob(num_file,obs_num),
     O               obs_data,
     I               myThid )

                IF ( obs_data.GT.spval .AND. obs_modval.GT.spval ) THEN
                  obs_mean = obs_mean + obs_data
                  mod_mean = mod_mean + obs_modval
                  nobsmean = nobsmean + 1
                ENDIF

              ENDIF !IF (obs_num.LE.ObsNo(num_file))
            ENDDO !DO obs_num

            obs_mean = obs_mean/nobsmean
            mod_mean = mod_mean/nobsmean
            offset   = mod_mean-obs_mean

          ELSE
            offset = zeroRL
          ENDIF

C Read data to calculate the cost
C and write misfits to global file

C Global file for misfits
          IL = ILNBLNK( obsfitfiles(num_file) )
          WRITE( obsfitfile,'(1A)' ) obsfitfiles(num_file)(1:IL)
          IL = ILNBLNK( obsfitfile )
          JL = ILNBLNK( obsfitDir )
          WRITE( fnamemisfit,'(3A)' )
     &     obsfitDir(1:JL),obsfitfile(1:IL),'.misfit.nc'
          err = NF_OPEN( fnamemisfit,NF_WRITE,fidmisfit(num_file) )
          err = NF_INQ_VARID( fidmisfit(num_file),'misfit',varID )
          CALL OBSFIT_NF_ERROR(
     &         'OBSFIT_COST: NF_INQ_VARID misfit',err,0,0,myThid )
C Loop over obs
          DO obs_num = 1, NOBSMAX_OBS
            IF ( obs_num.LE.ObsNo(num_file) ) THEN

              obs_modval = zeroRL
              obs_data   = zeroRL
              obs_uncert = zeroRL

C Read observation and uncertainty
              CALL OBSFIT_READ_OBS(
     I             num_file, 1, obs_ind_glob(num_file,obs_num),
     O             obs_data,
     I             myThid )

              CALL OBSFIT_READ_OBS(
     I             num_file, -1, obs_ind_glob(num_file,obs_num),
     O             obs_uncert,
     I             myThid )

              IF ( obs_data .EQ. spval ) obs_uncert = zeroRL

C Read model equivalent from global file
              CALL ACTIVE_READ_OBS_GLOB(
     I             num_file,
     O             obs_modval,obs_modmask,
     I             obs_num,.FALSE.,optimcycle,myThid,
     I             obsfit_globaldummy(num_file) )

              IF ( obs_uncert.GT.zeroRL ) THEN
                obs_weight = 1. _d 0 / (obs_uncert*obs_uncert)
                misfit = obs_modval-obs_data-offset
                objf_obsfit(num_file) = objf_obsfit(num_file)
     &           + obs_modmask* obs_weight * misfit * misfit
                num_obsfit(num_file)  = num_obsfit(num_file)
     &           + obs_modmask
C Write misfit to global netcdf file
                err = NF_PUT_VARA_DOUBLE( fidmisfit(num_file),varID,
     &                obs_ind_glob(num_file,obs_num),1,misfit )
                CALL OBSFIT_NF_ERROR(
     &               'OBSFIT_COST: NF_PUT_VARA_DOUBLE misfit',
     &               err,0,0,myThid )
              ENDIF

            ENDIF !IF (obs_num.LE.ObsNo(num_file))
          ENDDO !DO obs_num

          err = NF_CLOSE( fidmisfit(num_file) )

        ENDIF !IF myprocid = 0

      ENDDO !DO num_file

      _END_MASTER( myThid )

C Print cost function values
      DO num_file = 1, NFILESMAX_OBS

        objf_obsfit_glo = objf_obsfit(num_file)
        num_obsfit_glo = num_obsfit(num_file)

        WRITE( msgBuf,'(A,I2,A,E20.13,1X,D12.5)' )
     &       ' CAV obsfit_cost(',num_file,') = ',
     &       objf_obsfit_glo, num_obsfit_glo

        IF ( num_obsfit_glo.GT.zeroRL )
     &    CALL PRINT_MESSAGE( msgBuf,
     &         standardMessageUnit,SQUEEZE_RIGHT, myThid )

      ENDDO

      WRITE( msgBuf,'(A)') '== obsfit_cost: end   =='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit,SQUEEZE_RIGHT, myThid )
      WRITE( msgBuf,'(A)' ) ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit,SQUEEZE_RIGHT, myThid )

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
