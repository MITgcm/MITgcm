C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_PARAMS.h,v 1.1 2011/05/24 14:22:40 jmc Exp $
C $Name:  $

#ifdef ALLOW_OBCS

CBOP
C     !ROUTINE: OBCS_PARAMS.h
C     !INTERFACE:
C     #include "OBCS_PARAMS.h"

C     !DESCRIPTION:
C     *==========================================================*
C     | OBCS_PARAMS.h
C     | o Header file containing OBCS parameters
C     *==========================================================*
C     | o Note: does not (and should not) contain any conditional
C     |   statement that depends on OBCS options ; therefore
C     |   can be safely included without OBCS_OPTIONS.h
C     *==========================================================*
CEOP


C useOrlanskiNorth/South/East/West
C                  :: specify Orlanski boundary conditions for northern/
C                     southern/eastern/Western
C useStevensNorth/South/East/West
C                  :: use open boundary computations following Stevens (1990)
C T/SrelaxStevens  :: relaxation time scale (in seconds) for T/S-points
C                     for Stevens boundary conditions
C useStevensPhaseVel
C                  :: use phase velocity contribution for open boundary
C                     computations following Stevens (1990), default = true
C useStevensAdvection
C                  :: use advective contribution for open boundary
C                     computations following Stevens (1990), default = true
C useOBCSsponge    :: turns on sponge layer along boundary (def=false)
C spongeThickness  :: number grid points that make up the sponge layer (def=0)
C U/Vrelaxobcsinner/bound :: relaxation time scale (in seconds) for U/V-points
C                     on the boundary (bound) and at the innermost grid point
C                     of the sponge layer (inner); relaxation time scales
C                     in-between are linearly interpolated from these values
C useOBCSbalance   :: balance the volume flux through boundary
C                     at every time step
C useOBCSprescribe :: read boundary conditions from a file
C                      (overrides Orlanski and other boundary values)
C OBCSprintDiags   :: print boundary values to STDOUT (def=true)
C useOBCSYearlyFields :: when reading boundary values by exf, assume yearly
C                     climatology (def=false)
C OBCSfixTopo      :: check and adjust topography for problematic gradients
C                     across boundaries (def=true)
C tileHasOB[N,S,E,W] :: this tile has OB at Northern/Southern/Eastern/Western edge
C insideOBmaskFile   :: File to specify Inside OB region mask (zero beyond OB).
C OB[N,S,E,W][u,v,t,s,a,h,sn,sl,uice,vice]File :: Files with boundary conditions,
C                                                 the letter combinations mean:
C                     N/S/E/W   :: northern/southern/eastern/western boundary
C                     u/v/t/s   :: ocean u/v velocities, temperature/salinity
C                     a/h       :: sea ice concentration/effective thickness
C                     sn/sl     :: effective snow thickness/sea ice salinity
C                     uice/vice :: sea ice u/v drift velocities

      COMMON /OBC_PARM_I/
     & OBCS_monSelect,
     & spongeThickness
      INTEGER OBCS_monSelect
      INTEGER spongeThickness

      COMMON /OBC_PARM_L/
     & useOrlanskiNorth,useOrlanskiSouth,
     & useOrlanskiEast,useOrlanskiWest,
     & useStevensNorth,useStevensSouth,
     & useStevensEast,useStevensWest,
     & useStevensPhaseVel, useStevensAdvection,
     & useOBCSsponge, useOBCSbalance, useOBCSprescribe,
     & OBCSprintDiags, useOBCSYearlyFields,
     & OBCSfixTopo
      LOGICAL useOrlanskiNorth
      LOGICAL useOrlanskiSouth
      LOGICAL useOrlanskiEast
      LOGICAL useOrlanskiWest
      LOGICAL useStevensNorth
      LOGICAL useStevensSouth
      LOGICAL useStevensEast
      LOGICAL useStevensWest
      LOGICAL useStevensPhaseVel
      LOGICAL useStevensAdvection
      LOGICAL useOBCSsponge
      LOGICAL useOBCSbalance
      LOGICAL useOBCSprescribe
      LOGICAL OBCSprintDiags
      LOGICAL useOBCSYearlyFields
      LOGICAL OBCSfixTopo

C OBCS_balanceFacN/S/E/W :: weighting factor for balancing OB normal flow
C OBCS_uvApplyFac        :: multiplying factor to U,V normal comp. when applying
C                           OBC to 2nd column/row (for backward compatibility).
C OBCS_monitorFreq       :: monitor output frequency (s) for OB statistics
C OBCS_monSelect         :: select group of variables to monitor
      COMMON /OBC_PARM_R/
     &     OBCS_balanceFacN, OBCS_balanceFacS,
     &     OBCS_balanceFacE, OBCS_balanceFacW,
     &     OBCS_uvApplyFac,
     &     OBCS_monitorFreq,
     & Urelaxobcsinner,Urelaxobcsbound,
     & Vrelaxobcsinner,Vrelaxobcsbound,
     & TrelaxStevens, SrelaxStevens
      _RL OBCS_balanceFacN, OBCS_balanceFacS
      _RL OBCS_balanceFacE, OBCS_balanceFacW
      _RL OBCS_uvApplyFac
      _RL OBCS_monitorFreq
      _RS Urelaxobcsinner
      _RS Urelaxobcsbound
      _RS Vrelaxobcsinner
      _RS Vrelaxobcsbound
      _RS TrelaxStevens
      _RS SrelaxStevens

      COMMON /OBC_FILES/
     &      OBNuFile,  OBSuFile,  OBEuFile,  OBWuFile,
     &      OBNvFile,  OBSvFile,  OBEvFile,  OBWvFile,
     &      OBNwFile,  OBSwFile,  OBEwFile,  OBWwFile,
     &      OBNtFile,  OBStFile,  OBEtFile,  OBWtFile,
     &      OBNsFile,  OBSsFile,  OBEsFile,  OBWsFile,
     &      OBNetaFile,OBSetaFile,OBEetaFile,OBWetaFile,
     &      insideOBmaskFile
      CHARACTER*(MAX_LEN_FNAM)
     &      OBNuFile,  OBSuFile,  OBEuFile,  OBWuFile,
     &      OBNvFile,  OBSvFile,  OBEvFile,  OBWvFile,
     &      OBNwFile,  OBSwFile,  OBEwFile,  OBWwFile,
     &      OBNtFile,  OBStFile,  OBEtFile,  OBWtFile,
     &      OBNsFile,  OBSsFile,  OBEsFile,  OBWsFile,
     &      OBNetaFile,OBSetaFile,OBEetaFile,OBWetaFile,
     &      insideOBmaskFile

#endif /* ALLOW_OBCS */
