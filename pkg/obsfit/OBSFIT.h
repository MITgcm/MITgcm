CBOP
C     !ROUTINE: OBSFIT.h

C     !INTERFACE:
C     #include "OBSFIT.h"

C     !DESCRIPTION:
C     ==================================================================
C     | Header file defining ObsFit parameters and variables
C     ==================================================================
CEOP

C ObsFit LOGICAL parameters
      LOGICAL obsfitDoNcOutput
      LOGICAL obsfitDoGenGrid

      COMMON /OBSFIT_PACKAGE/
     & obsfitDoNcOutput, obsfitDoGenGrid

C ObsFit real parameters
      _RL sample_timeS(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_timeE(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_lon(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_lat(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_depth(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_weight(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_modmask(nsx,nsy)
      _RL obs_modmask
      _RL obs_delT(NFILESMAX_OBS,NOBSMAX_OBS)

      COMMON /OBSFIT_R/ sample_type, sample_timeS, 
     & sample_timeE, sample_lon, sample_lat,
     & sample_depth, sample_weight, sample_modmask,
     & obs_modmask, obs_delT

C ObsFit integer parameters
      INTEGER sample_type(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      INTEGER obs_ind_glob(NFILESMAX_OBS,NOBSMAX_OBS)
      INTEGER obs_np(NFILESMAX_OBS,NOBSMAX_OBS)
      INTEGER obsfitOperation(NFILESMAX_OBS)
      INTEGER sample_ind_glob(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      INTEGER ObsNo(NFILESMAX_OBS)
      INTEGER sampleNo(NFILESMAX_OBS,nsx,nsy)
      INTEGER fidfwd_obs(NFILESMAX_OBS,nsx,nsy)
      INTEGER fidadj_obs(NFILESMAX_OBS,nsx,nsy)
      INTEGER fidtan_obs(NFILESMAX_OBS,nsx,nsy)
      INTEGER fiddata_obs(NFILESMAX_OBS)
      INTEGER fidglobal(NFILESMAX_OBS)
      INTEGER fidadglobal(NFILESMAX_OBS)
      INTEGER fidtanglobal(NFILESMAX_OBS)
      INTEGER fidmisfit(NFILESMAX_OBS)
      INTEGER obs_sample1_ind(NFILESMAX_OBS,NOBSMAX_OBS)
      INTEGER obs_is_ssh(NFILESMAX_OBS)

      COMMON /OBSFIT_I/ obs_ind_glob, obs_np, obsfitOperation,
     & sample_ind_glob, ObsNo, sampleNo,
     & fidfwd_obs, fidadj_obs, fidtan_obs, fiddata_obs, 
     & fidglobal, fidadglobal, fidtanglobal, fidmisfit,
     & obs_sample1_ind, obs_is_ssh

C ObsFit character strings
      CHARACTER*(8)  obsfit_nameval
      CHARACTER*(12) obsfit_namemask
      CHARACTER*(14) obsfit_nameuncert
      CHARACTER*(8)  obsfit_nameequi
      
      COMMON /OBSFIT_C/ obsfit_nameval, obsfit_namemask,
     & obsfit_nameuncert, obsfit_nameequi

C Grid parameters
      INTEGER sample_interp_i(NFILESMAX_OBS,NSAMPLESMAX,
     &     NUM_INTERP_PTS_OBS,nsx,nsy)
      INTEGER sample_interp_j(NFILESMAX_OBS,NSAMPLESMAX,
     &     NUM_INTERP_PTS_OBS,nsx,nsy)
      INTEGER sample_interp_k(NFILESMAX_OBS,NSAMPLESMAX,
     &     NUM_INTERP_PTS_OBS,nsx,nsy)
      _RL sample_interp_frac(NFILESMAX_OBS,NSAMPLESMAX,
     &     NUM_INTERP_PTS_OBS,nsx,nsy)
C Generic grid
      _RL sample_interp_xC11(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_interp_yC11(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_interp_xCNINJ(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)
      _RL sample_interp_yCNINJ(NFILESMAX_OBS,NSAMPLESMAX,nsx,nsy)

      COMMON /OBSFIT_GRID_I/ 
     & sample_interp_i, sample_interp_j, sample_interp_k
      COMMON /OBSFIT_GRID_R/ sample_interp_frac,
     & sample_interp_xC11, sample_interp_yC11,
     & sample_interp_xCNINJ, sample_interp_yCNINJ  

C ObsFit buffers
      _RL obsfit_data_buff(1000)
      _RL obsfit_uncert_buff(1000)
      INTEGER obsfit_minind_buff
      INTEGER obsfit_maxind_buff
      INTEGER obsfit_curfile_buff
      
      COMMON /OBSFIT_BUFF_R/ obsfit_data_buff, obsfit_uncert_buff
      COMMON /OBSFIT_BUFF_I/
     & obsfit_minind_buff, obsfit_maxind_buff, obsfit_curfile_buff

C Cost
      _RL objf_obsfit(NFILESMAX_OBS)
      _RL num_obsfit(NFILESMAX_OBS)
      _RL mult_obsfit(NFILESMAX_OBS)
      _RL obsfit_facmod(NFILESMAX_OBS)
      
      COMMON /OBSFIT_COST_R/
     & objf_obsfit, num_obsfit,
     & mult_obsfit, obsfit_facmod
     
C Input files     
      CHARACTER*(MAX_LEN_FNAM) obsfitDir
      CHARACTER*(MAX_LEN_FNAM) obsfitFiles(NFILESMAX_OBS)
      
      COMMON /OBSFIT_COST_C/
     & obsfitDir, obsfitFiles

C File reading     
      _RL obsfit_dummy(NFILESMAX_OBS,nsx,nsy)
      _RL obsfit_globaldummy(NFILESMAX_OBS)
      
      COMMON /OBSFIT_CTRL_DUMMY/
     & obsfit_dummy, obsfit_globaldummy

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
