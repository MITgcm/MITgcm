C $Header: /u/gcmpack/MITgcm/pkg/profiles/PROFILES_SIZE.h,v 1.1 2015/08/06 15:48:20 gforget Exp $
C $Name:  $

C============================================================
C NOBSGLOB            :: maximum number of profiles per file and tile
C NFILESPROFMAX       :: maximum number of files
C NVARMAX             :: maximum number of variables 
C NLEVELMAX           :: maximum number of vertical levels 
C NUM_INTERP_POINTS   :: number of points used in horizontal averaging
C============================================================

      INTEGER  NOBSGLOB
      PARAMETER ( NOBSGLOB = 500  )
      INTEGER NFILESPROFMAX
      PARAMETER ( NFILESPROFMAX=20 )
      INTEGER NVARMAX
      PARAMETER ( NVARMAX=6 )
      INTEGER NLEVELMAX
      PARAMETER ( NLEVELMAX=110 )
      INTEGER NUM_INTERP_POINTS
#ifndef ALLOW_PROFILES_GENERICGRID
      PARAMETER (NUM_INTERP_POINTS = 4)
#else
      PARAMETER (NUM_INTERP_POINTS = 1)
#endif

