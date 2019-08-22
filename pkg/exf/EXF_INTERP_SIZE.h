CBOP
C    !ROUTINE: EXF_INTERP_SIZE.h
C    !INTERFACE:
C #include EXF_INTERP_SIZE.h

C     ==================================================================
C     HEADER EXF_INTERP_SIZE.h
C     ==================================================================

C    !DESCRIPTION:
C   Contains maximum size of original input grid from which interpolation
C    to model grid is done.
C-  Note: This header file needs to be included before EXF_INTERP_PARAM.h
C    since it uses MAX_LAT_INC which is defined here.
CEOP

#ifdef USE_EXF_INTERPOLATION

C     INTEGER MAX_LAT_INC :: maximum length of latitude grid-spacing vector
C                            used for exf-interpolation input-grid
      INTEGER MAX_LAT_INC
      PARAMETER( MAX_LAT_INC = 1279 )

#ifndef EXF_INTERP_USE_DYNALLOC
C-  To read input data without dynamical allocation (INTERP_USE_DYNALLOC undef):
C     exf_max_nLon :: maximum size of original grid (longitudinal direction)
C     exf_max_nLat :: maximum size of original grid (latitudinal direction)
C     exf_interp_bufferSize :: buffer maximum size
      INTEGER    exf_max_nLon, exf_max_nLat
      INTEGER    exf_interp_bufferSize
      PARAMETER( exf_max_nLon = 520 )
      PARAMETER( exf_max_nLat = 260 )

C   Buffer size was set to 65000 (allowing to read-in a 1x1 global data set);
C   increased to 140000 to accommodate for ECMWF-INTERIM (512 x 256)
      PARAMETER( exf_interp_bufferSize = 140000 )
#endif /* ndef EXF_INTERP_USE_DYNALLOC */

#else /* USE_EXF_INTERPOLATION */

C-- Set dummy dimension
      INTEGER    MAX_LAT_INC
      INTEGER    exf_max_nLon, exf_max_nLat
      INTEGER    exf_interp_bufferSize
      PARAMETER( MAX_LAT_INC = 1 )
      PARAMETER( exf_max_nLon = 1 )
      PARAMETER( exf_max_nLat = 1 )
      PARAMETER( exf_interp_bufferSize = 1 )

#endif /* USE_EXF_INTERPOLATION */
