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
      _RL prof_time(NFILESPROFMAX,NOBSGLOB),
     & prof_lon(NFILESPROFMAX,NOBSGLOB),
     & prof_lat(NFILESPROFMAX,NOBSGLOB)
      integer prof_ind_glob(NFILESPROFMAX,NOBSGLOB)
      _RL prof_depth(NFILESPROFMAX,NLEVELMAX)
      _RL prof_mask1D_cur(NLEVELMAX)

      integer profno(NFILESPROFMAX)
      integer profdepthno(NFILESPROFMAX)

      logical vec_quantities(NFILESPROFMAX,4)
      integer fidforward(NFILESPROFMAX), 
     & fidadjoint(NFILESPROFMAX), fidtangent(NFILESPROFMAX)
      integer fiddata(NFILESPROFMAX)
      character*(6) prof_names(4)
      character*(10) prof_namesmask(4)
      character*(12) prof_namesweight(4)

      _RL profiles_data_buff(NLEVELMAX,1000,4)
      _RL profiles_weight_buff(NLEVELMAX,1000,4)
      integer profiles_minind_buff, profiles_maxind_buff
      integer profiles_curfile_buff

      integer profilesfile_equi_type
      integer prof_num_var_tot(NFILESPROFMAX)
      integer prof_num_var_cur(NFILESPROFMAX,4)

C===========================================================
C Common Blocks
C===========================================================
      COMMON /profiles_common/ prof_time,prof_lon,prof_lat,
     &prof_ind_glob,profno,profdepthno, prof_depth,
     &prof_names,prof_namesmask,prof_namesweight,
     &vec_quantities, fidforward, fidadjoint, fidtangent,fiddata,
     & prof_mask1D_cur, profilesfile_equi_type,
     & prof_num_var_tot,prof_num_var_cur
      COMMON /profiles_buff/ profiles_data_buff, profiles_weight_buff,
     & profiles_minind_buff,profiles_maxind_buff,profiles_curfile_buff

     
