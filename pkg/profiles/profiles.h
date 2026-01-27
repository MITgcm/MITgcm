C===========================================================
C variables
C===========================================================
      _RL prof_time(NFILESPROFMAX,NOBSGLOB,nsx,nsy),
     & prof_lon(NFILESPROFMAX,NOBSGLOB,nsx,nsy),
     & prof_lat(NFILESPROFMAX,NOBSGLOB,nsx,nsy)

      _RL prof_interp_xC11(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      _RL prof_interp_yC11(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      _RL prof_interp_xCNINJ(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      _RL prof_interp_yCNINJ(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      _RL prof_interp_weights(NFILESPROFMAX,NOBSGLOB,
     &     NUM_INTERP_POINTS,nsx,nsy)
      integer prof_interp_i(NFILESPROFMAX,NOBSGLOB,
     &     NUM_INTERP_POINTS,nsx,nsy)
      integer prof_interp_j(NFILESPROFMAX,NOBSGLOB,
     &     NUM_INTERP_POINTS,nsx,nsy)

      integer prof_ind_glob(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      _RL prof_depth(NFILESPROFMAX,NLEVELMAX,nsx,nsy)
      _RL prof_mask1D_cur(NLEVELMAX,nsx,nsy)
      _RL prof_etan_mean(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL prof_theta_mean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL prof_salt_mean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#ifndef ALLOW_ECCO
      _RL m_UE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nsx,nsy)
      _RL m_VN(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nsx,nsy)
#endif

      integer profNo(NFILESPROFMAX,nsx,nsy)
      integer profDepthNo(NFILESPROFMAX,nsx,nsy)

      logical profilesWriteCostFunction
      logical vec_quantities(NFILESPROFMAX,NVARMAX,nsx,nsy)
      integer fidforward(NFILESPROFMAX,nsx,nsy),
     & fidadjoint(NFILESPROFMAX,nsx,nsy),
     & fidtangent(NFILESPROFMAX,nsx,nsy)
      integer fiddata(NFILESPROFMAX,nsx,nsy)
      character*(8) prof_names(NFILESPROFMAX,NVARMAX)
      character*(8) prof_namesmod(NFILESPROFMAX,NVARMAX)
      character*(12) prof_namesmask(NFILESPROFMAX,NVARMAX)
      character*(14) prof_namesweight(NFILESPROFMAX,NVARMAX)
#ifdef ALLOW_PROFILES_CLIMMASK
      character*(12) prof_namesclim(NFILESPROFMAX,NVARMAX)
#endif
      integer prof_itracer(NFILESPROFMAX,NVARMAX)

      _RL profiles_data_buff(NLEVELMAX,1000,NVARMAX,nsx,nsy)
      _RL profiles_weight_buff(NLEVELMAX,1000,NVARMAX,nsx,nsy)
      integer profiles_minind_buff(nsx,nsy)
      integer profiles_maxind_buff(nsx,nsy)
      integer profiles_curfile_buff(nsx,nsy)

      logical profilesDoNcOutput, profilesDoGenGrid
      logical prof_make_nc
      integer prof_num_var_tot(NFILESPROFMAX,nsx,nsy)
      integer prof_num_var_cur(NFILESPROFMAX,NVARMAX,nsx,nsy)

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      integer prof_ind_avgbin(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      integer NLEVELCOMB, NAVGBIN
      _RL NLEVELCOMBRL, NAVGBINRL
C number of independent samples
      integer profiles_mean_indsamples(NVARMAX)
      _RL prof_depth_comb(NLEVELCOMBMAX,nsx,nsy)
      integer prof_lev_comb(NLEVELMAX,NFILESPROFMAX,nsx,nsy)
      integer avgbinglbsum(NAVGBINMAX)
      _RL prof_data1D_all_mean(NAVGBINMAX,NLEVELCOMBMAX,
     &NVARMAX)
      _RL prof_traj1D_all_mean(NAVGBINMAX,NLEVELCOMBMAX,
     &NVARMAX)
      _RL prof_weights1D_all_mean(NAVGBINMAX,NLEVELCOMBMAX,
     &NVARMAX)
      _RL prof_count1D_all_mean(NAVGBINMAX,NLEVELCOMBMAX,
     &NVARMAX)
#endif

C===========================================================
C Common Blocks
C===========================================================

      COMMON /profiles_r/ prof_time, prof_lon, prof_lat,
     & prof_depth, prof_mask1D_cur,
     & prof_etan_mean, prof_theta_mean, prof_salt_mean
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &,prof_depth_comb
     &,prof_data1D_all_mean
     &,prof_traj1D_all_mean
     &,prof_weights1D_all_mean
     &,prof_count1D_all_mean
     &,NLEVELCOMBRL, NAVGBINRL
#endif
#ifndef ALLOW_ECCO
     &,m_UE,m_VN
#endif /* ALLOW_ECCO */
      COMMON /profiles_i/ prof_ind_glob, profNo, profDepthNo,
     & fidforward, fidadjoint, fidtangent, fiddata,
     & prof_num_var_tot, prof_num_var_cur, prof_itracer
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &,prof_ind_avgbin, NLEVELCOMB, NAVGBIN
     &,prof_lev_comb
     &,avgbinglbsum
     &,profiles_mean_indsamples
#endif

      COMMON /profiles_l/ vec_quantities, profilesDoNcOutput,
     & profilesDoGenGrid, prof_make_nc,
     & profilesWriteCostFunction
      COMMON /profiles_c/ prof_names, prof_namesmask,
#ifdef ALLOW_PROFILES_CLIMMASK
     & prof_namesclim,
#endif
     & prof_namesweight, prof_namesmod

      COMMON /profiles_GenericGrid_r/ prof_interp_weights,
     & prof_interp_xC11, prof_interp_yC11,
     & prof_interp_xCNINJ, prof_interp_yCNINJ
      COMMON /profiles_GenericGrid_i/
     & prof_interp_i, prof_interp_j

      COMMON /profiles_buff_r/ profiles_data_buff, profiles_weight_buff
      COMMON /profiles_buff_i/
     & profiles_minind_buff, profiles_maxind_buff, profiles_curfile_buff


      COMMON /profiles_cost_r/
     &                objf_profiles,
     &                num_profiles,
     &                mult_profiles,
     &                prof_facmod
     &               ,objf_profiles_mean,
     &                num_profiles_mean,
     &                mult_profiles_mean

      _RL  objf_profiles(NFILESPROFMAX,NVARMAX,nsx,nsy)
      _RL  num_profiles(NFILESPROFMAX,NVARMAX,nsx,nsy)
      _RL  mult_profiles(NFILESPROFMAX,NVARMAX)
      _RL  prof_facmod(NFILESPROFMAX,NVARMAX)
      _RL  objf_profiles_mean(NVARMAX,nsx,nsy)
      _RL  num_profiles_mean(NVARMAX,nsx,nsy)
      _RL  mult_profiles_mean(NVARMAX)

      COMMON /profiles_cost_c/
     &        profilesDir, profilesfiles
      character*(MAX_LEN_FNAM) profilesDir
      character*(MAX_LEN_FNAM) profilesfiles(NFILESPROFMAX)

      COMMON /profiles_ctrl_dummy/
     &                profiles_dummy
      _RL profiles_dummy(NFILESPROFMAX,NVARMAX,nsx,nsy)
