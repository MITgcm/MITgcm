C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/AIM_GRID.h,v 1.1 2002/09/27 20:01:57 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM
C     *==========================================================*
C     | AIM_GRID.h  define grid arrays required for AIM Physics  |
C     *==========================================================*

C--   COMMON /NB_LEVELS/ Number of non-empty levels 
C     NLEVxy  : Nb of levels at Cell Center
C     NLEVxyU : Nb of levels at U-point (Western  edge)
C     NLEVxyV : Nb of levels at V-point (Southern edge)

      COMMON /NB_LEVELS/ NLEVxy, NLEVxyU, NLEVxyV
      INTEGER NLEVxy  (NGP, MAX_NO_THREADS)
      INTEGER NLEVxyU (NGP, MAX_NO_THREADS)
      INTEGER NLEVxyV (NGP, MAX_NO_THREADS)

#endif /* ALLOW_AIM */ 
