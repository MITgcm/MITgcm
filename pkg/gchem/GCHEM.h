#ifdef ALLOW_PTRACERS

CBOP
C    !ROUTINE: GCHEM.h
C    !INTERFACE:
 
C    !DESCRIPTION:
C Contains tracer parameters and input files for chemical tracers.

      INTEGER tIter0
      INTEGER nsubtime
      CHARACTER*(MAX_LEN_FNAM) WindFile
      CHARACTER*(MAX_LEN_FNAM) AtmospFile
      CHARACTER*(MAX_LEN_FNAM) IceFile
      CHARACTER*(MAX_LEN_FNAM) IronFile
      CHARACTER*(MAX_LEN_FNAM) SilicaFile

      COMMON /GCHEM_PARAMS/
     &                   tIter0,
     &                   WindFile,
     &                   AtmospFile,
     &                   IceFile,
     &                   IronFile,
     &                   SilicaFile,
     &                   nsubtime
      NAMELIST /GCHEM_PARM01/
     &                   tIter0,
     &                   WindFile,
     &                   IceFile,
     &                   AtmospFile,
     &                   IronFile,
     &                   SilicaFile,
     &                   nsubtime
#endif /* ALLOW_PTRACERS */
