C $Header: /u/gcmpack/MITgcm_contrib/ecco_utils/ecco_v4_release3_devel/code/PROFILES_SIZE.h,v 1.1 2017/05/04 17:46:37 ou.wang Exp $
C $Name:  $

C============================================================
C NOBSGLOB            :: maximum number of profiles per file and tile
C NFILESPROFMAX       :: maximum number of files
C NVARMAX             :: maximum number of variables 
C NLEVELMAX           :: maximum number of vertical levels 
C NUM_INTERP_POINTS   :: number of points used in horizontal averaging
C============================================================

      INTEGER  NOBSGLOB
      PARAMETER ( NOBSGLOB = 100000  )
      INTEGER NFILESPROFMAX
      PARAMETER ( NFILESPROFMAX=35 )
      INTEGER NVARMAX
      PARAMETER ( NVARMAX=6 )
      INTEGER NLEVELMAX
      PARAMETER ( NLEVELMAX=140 )
      INTEGER NUM_INTERP_POINTS
      PARAMETER (NUM_INTERP_POINTS = 4)
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      INTEGER NLEVELCOMBMAX
      PARAMETER ( NLEVELCOMBMAX=200 )
      INTEGER NAVGBINMAX
      PARAMETER ( NAVGBINMAX=10245 )
#endif


