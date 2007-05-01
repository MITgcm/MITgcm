C $Header: /u/gcmpack/MITgcm/pkg/gchem/GCHEM.h,v 1.9 2007/05/01 20:48:41 stephd Exp $
C $Name:  $

#ifdef ALLOW_GCHEM

CBOP
C    !ROUTINE: GCHEM.h
C    !INTERFACE:
 
C    !DESCRIPTION:
C Contains tracer parameters and input files for chemical tracers.
C These can be read in from data.gchem
C
C  nsubtime   : : number of chemistry timesteps per deltaTtracer
C                 (default 1) 
C  WindFile   : : file name of wind speeds that may be needed for
C                 biogeochemical experiments
C  AtmospFile : : file name of atmospheric pressure that may be needed for
C                 biogeochemical experiments
C  IceFile    : : file name of seaice fraction that may be needed for
C                 biogeochemical experiments
C  IronFile   : : file name of aeolian iron flux that may be needed for
C                 biogeochemical experiments
C  SilicaFile : : file name of surface silica that may be needed for
C                 biogeochemical experiments
C  Filename*  : : various spare filenames 
C  gchem_int* : : place holder to read in a integer number, set at run time
C  gchem_rl*  : : place holder to read in a real number, set at run time

C  
      INTEGER nsubtime
      CHARACTER*(MAX_LEN_FNAM) WindFile
      CHARACTER*(MAX_LEN_FNAM) AtmospFile
      CHARACTER*(MAX_LEN_FNAM) IceFile
      CHARACTER*(MAX_LEN_FNAM) IronFile
      CHARACTER*(MAX_LEN_FNAM) SilicaFile
      CHARACTER*(MAX_LEN_FNAM) Filename1
      CHARACTER*(MAX_LEN_FNAM) Filename2
      CHARACTER*(MAX_LEN_FNAM) Filename3
      CHARACTER*(MAX_LEN_FNAM) Filename4
      CHARACTER*(MAX_LEN_FNAM) Filename5
      INTEGER gchem_int1
      INTEGER gchem_int2
      INTEGER gchem_int3
      INTEGER gchem_int4
      INTEGER gchem_int5
      _RL     gchem_rl1
      _RL     gchem_rl2
      _RL     gchem_rl3
      _RL     gchem_rl4
      _RL     gchem_rl5


      COMMON /GCHEM_PARAMS/
     &                   WindFile,
     &                   AtmospFile,
     &                   IceFile,
     &                   IronFile,
     &                   SilicaFile,
     &                   Filename1,
     &                   Filename2,
     &                   Filename3,
     &                   Filename4,
     &                   Filename5,
     &                   nsubtime,
     &           gchem_int1, gchem_int2, gchem_int3,
     &           gchem_int4, gchem_int5,
     &           gchem_rl1, gchem_rl2, gchem_rl3,
     &           gchem_rl4, gchem_rl5
CEOP

#endif /* ALLOW_GCHEM */
