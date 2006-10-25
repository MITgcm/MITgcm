C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles.h,v 1.6 2006/10/25 22:19:57 gforget Exp $
C $Name:  $

C============================================================
C NOBSMAX : maximal number of profiles
C============================================================
      INTEGER  NOBSGLOB
      PARAMETER ( NOBSGLOB = 100000  )
      INTEGER NFILESPROFMAX
      PARAMETER ( NFILESPROFMAX=10 )
      INTEGER NVARMAX
      PARAMETER ( NVARMAX=6 )
      INTEGER NLEVELMAX
      PARAMETER ( NLEVELMAX=100 )

C===========================================================
C variables
C===========================================================
      _RL prof_time(NFILESPROFMAX,NOBSGLOB,nsx,nsy),
     & prof_lon(NFILESPROFMAX,NOBSGLOB,nsx,nsy),
     & prof_lat(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      integer prof_ind_glob(NFILESPROFMAX,NOBSGLOB,nsx,nsy)
      _RL prof_depth(NFILESPROFMAX,NLEVELMAX,nsx,nsy)
      _RL prof_mask1D_cur(NLEVELMAX,nsx,nsy)
      _RL prof_etan_mean(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      integer profNo(NFILESPROFMAX,nsx,nsy)
      integer profDepthNo(NFILESPROFMAX,nsx,nsy)

      logical vec_quantities(NFILESPROFMAX,NVARMAX,nsx,nsy)
      integer fidforward(NFILESPROFMAX,nsx,nsy), 
     & fidadjoint(NFILESPROFMAX,nsx,nsy), 
     & fidtangent(NFILESPROFMAX,nsx,nsy)
      integer fiddata(NFILESPROFMAX,nsx,nsy)
      character*(8) prof_names(NVARMAX)
      character*(12) prof_namesmask(NVARMAX)
      character*(14) prof_namesweight(NVARMAX)

      _RL profiles_data_buff(NLEVELMAX,1000,NVARMAX,nsx,nsy)
      _RL profiles_weight_buff(NLEVELMAX,1000,NVARMAX,nsx,nsy)
      integer profiles_minind_buff(nsx,nsy)
      integer profiles_maxind_buff(nsx,nsy)
      integer profiles_curfile_buff(nsx,nsy)

      integer profilesfile_equi_type
      integer prof_num_var_tot(NFILESPROFMAX,nsx,nsy)
      integer prof_num_var_cur(NFILESPROFMAX,NVARMAX,nsx,nsy)

C===========================================================
C Common Blocks
C===========================================================

      COMMON /profiles_r/ prof_time, prof_lon, prof_lat,
     & prof_depth, prof_mask1D_cur, prof_etan_mean 
      COMMON /profiles_i/ prof_ind_glob, profNo, profDepthNo,
     & fidforward, fidadjoint, fidtangent, fiddata,
     & prof_num_var_tot, prof_num_var_cur, profilesfile_equi_type
      COMMON /profiles_l/ vec_quantities
      COMMON /profiles_c/ prof_names, prof_namesmask, prof_namesweight

      COMMON /profiles_buff_r/ profiles_data_buff, profiles_weight_buff
      COMMON /profiles_buff_i/
     & profiles_minind_buff, profiles_maxind_buff, profiles_curfile_buff

     
      COMMON /profiles_cost_r/
     &                objf_profiles,
     &                num_profiles,
     &                mult_profiles
      _RL  objf_profiles(NFILESPROFMAX,NVARMAX,nsx,nsy)
      _RL  num_profiles(NFILESPROFMAX,NVARMAX,nsx,nsy)
      _RL  mult_profiles(NFILESPROFMAX,NVARMAX)

      COMMON /profiles_cost_c/
     &                     profilesfiles
      character*(MAX_LEN_FNAM) profilesfiles(50)

      COMMON /profiles_ctrl_dummy/
     &                profiles_dummy
      _RL profiles_dummy(NFILESPROFMAX,NVARMAX,nsx,nsy)


