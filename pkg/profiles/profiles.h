C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles.h,v 1.3 2006/07/14 20:19:36 gforget Exp $
C $Name:  $

C============================================================
C NOBSMAX : maximal number of profiles
C============================================================
      INTEGER  NOBSGLOB
      PARAMETER ( NOBSGLOB = 100000  )
      INTEGER NFILESPROFMAX
      PARAMETER ( NFILESPROFMAX=10 )
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

      integer profNo(NFILESPROFMAX,nsx,nsy)
      integer profDepthNo(NFILESPROFMAX,nsx,nsy)

      logical vec_quantities(NFILESPROFMAX,4,nsx,nsy)
      integer fidforward(NFILESPROFMAX,nsx,nsy), 
     & fidadjoint(NFILESPROFMAX,nsx,nsy), 
     & fidtangent(NFILESPROFMAX,nsx,nsy)
      integer fiddata(NFILESPROFMAX,nsx,nsy)
      character*(6) prof_names(4)
      character*(10) prof_namesmask(4)
      character*(12) prof_namesweight(4)

      _RL profiles_data_buff(NLEVELMAX,1000,4,nsx,nsy)
      _RL profiles_weight_buff(NLEVELMAX,1000,4,nsx,nsy)
      integer profiles_minind_buff(nsx,nsy)
      integer profiles_maxind_buff(nsx,nsy)
      integer profiles_curfile_buff(nsx,nsy)

      integer profilesfile_equi_type
      integer prof_num_var_tot(NFILESPROFMAX,nsx,nsy)
      integer prof_num_var_cur(NFILESPROFMAX,4,nsx,nsy)

C===========================================================
C Common Blocks
C===========================================================
      COMMON /profiles_common/ prof_time,prof_lon,prof_lat,
     &prof_ind_glob,profNo,profDepthNo, prof_depth,
     &prof_names,prof_namesmask,prof_namesweight,
     &vec_quantities, fidforward, fidadjoint, fidtangent,fiddata,
     & prof_mask1D_cur, profilesfile_equi_type,
     & prof_num_var_tot,prof_num_var_cur
      COMMON /profiles_buff/ profiles_data_buff, profiles_weight_buff,
     & profiles_minind_buff,profiles_maxind_buff,profiles_curfile_buff

     
      COMMON /profiles_cost_r/
     &                objf_profiles,
     &                num_profiles,
     &                mult_profiles
      _RL  objf_profiles(50,10,nsx,nsy)
      _RL  num_profiles(50,10,nsx,nsy)
      _RL  mult_profiles(50,10)

      COMMON /profiles_cost_c/
     &                     profilesfiles
      character*(MAX_LEN_FNAM) profilesfiles(50)

      COMMON /profiles_ctrl_dummy/
     &                profiles_dummy
      _RL profiles_dummy(50,10,nsx,nsy)
