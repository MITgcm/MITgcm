C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_PARAMS.h,v 1.6 2014/11/25 01:07:23 jmc Exp $
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

C tidalComponents  :: number of tidal components to be applied
C OBCS_maxConnect  :: maximum number of connected parts
      INTEGER tidalComponents
      INTEGER OBCS_maxConnect
      PARAMETER ( tidalComponents = 10 )
      PARAMETER ( OBCS_maxConnect = sNx+sNy )

C--   COMMON /OBC_PARM_I/ OBCS integer-type parameter
C OBCS_u1_adv_T    :: >0: use 1rst O. upwind adv-scheme @ OB (=1: only if outflow)
C OBCS_u1_adv_S    :: >0: use 1rst O. upwind adv-scheme @ OB (=1: only if outflow)
C OBCS_monSelect   :: select group of variables to monitor
C spongeThickness  :: number grid points that make up the sponge layer (def=0)
      COMMON /OBC_PARM_I/
     & OBCS_u1_adv_T, OBCS_u1_adv_S,
     & OBCS_monSelect,
     & spongeThickness
      INTEGER OBCS_u1_adv_T, OBCS_u1_adv_S
      INTEGER OBCS_monSelect
      INTEGER spongeThickness

C--   COMMON /OBC_PARM_L/ OBCS logical-type parameter
C useOrlanskiNorth/South/East/West
C                  :: specify Orlanski boundary conditions for northern/
C                     southern/eastern/Western
C useStevensNorth/South/East/West
C                  :: use open boundary computations following Stevens (1990)
C useStevensPhaseVel
C                  :: use phase velocity contribution for open boundary
C                     computations following Stevens (1990), default = true
C useStevensAdvection
C                  :: use advective contribution for open boundary
C                     computations following Stevens (1990), default = true
C
C useOBCSsponge    :: turns on sponge layer along boundaries (def=false)
C OBCSsponge_N     :: turns on sponge layer along North boundary (def=true)
C OBCSsponge_S     :: turns on sponge layer along South boundary (def=true)
C OBCSsponge_E     :: turns on sponge layer along East boundary (def=true)
C OBCSsponge_W     :: turns on sponge layer along West boundary (def=true)
C OBCSsponge_UatNS :: turns on uVel sponge at North/South boundaries (def=true)
C OBCSsponge_UatEW :: turns on uVel sponge at East/West boundaries (def=true)
C OBCSsponge_VatNS :: turns on vVel sponge at North/South boundaries (def=true)
C OBCSsponge_VatEW :: turns on vVel sponge at East/West boundaries (def=true)
C OBCSsponge_Theta :: turns on Theta sponge along boundaries (def=true)
C OBCSsponge_Salt  :: turns on Salt sponge along boundaries (def=true)
C useLinearSponge  :: use linear instead of exponential sponge (def=false)
C
C useOBCSbalance   :: balance the volume flux through boundary
C                     at every time step
C useOBCStides     :: modify OB normal flow to add tidal forcing
C useOBCSprescribe :: read boundary conditions from a file
C                      (overrides Orlanski and other boundary values)
C OBCSprintDiags   :: print boundary values to STDOUT (def=true)
C OBCSfixTopo      :: check and adjust topography for problematic gradients
C                     across boundaries (def=true)

      COMMON /OBC_PARM_L/
     & useOrlanskiNorth,useOrlanskiSouth,
     & useOrlanskiEast,useOrlanskiWest,
     & useStevensNorth,useStevensSouth,
     & useStevensEast,useStevensWest,
     & useStevensPhaseVel, useStevensAdvection,
     & useOBCSsponge,
     & OBCSsponge_N, OBCSsponge_S,
     & OBCSsponge_E, OBCSsponge_W,
     & OBCSsponge_UatNS, OBCSsponge_UatEW,
     & OBCSsponge_VatNS, OBCSsponge_VatEW,
     & OBCSsponge_Theta, OBCSsponge_Salt, useLinearSponge,
     & useOBCSbalance, useOBCStides, useOBCSprescribe,
     & OBCSprintDiags,
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
      LOGICAL OBCSsponge_N
      LOGICAL OBCSsponge_S
      LOGICAL OBCSsponge_E
      LOGICAL OBCSsponge_W
      LOGICAL OBCSsponge_UatNS
      LOGICAL OBCSsponge_UatEW
      LOGICAL OBCSsponge_VatNS
      LOGICAL OBCSsponge_VatEW
      LOGICAL OBCSsponge_Theta
      LOGICAL OBCSsponge_Salt
      LOGICAL useLinearSponge
      LOGICAL useOBCSbalance
      LOGICAL useOBCStides
      LOGICAL useOBCSprescribe
      LOGICAL OBCSprintDiags
      LOGICAL OBCSfixTopo

C--   COMMON /OBC_PARM_R/ OBCS real-type parameter
C OBCS_balanceFacN/S/E/W  :: weighting factor for balancing OB normal flow
C OBCS_uvApplyFac         :: multiplying factor to U,V normal comp. when applying
C                            OBC to 2nd column/row (for backward compatibility).
C OBCS_monitorFreq        :: monitor output frequency (s) for OB statistics
C U/Vrelaxobcsinner/bound :: relaxation time scale (in seconds) on the boundary
C                            (bound) and at the innermost grid point of the sponge
C                            layer (inner); relaxation time scales in-between
C                            are linearly interpolated from these values
C T/SrelaxStevens         :: relaxation time scale (in seconds) for T/S-points
C                            for Stevens boundary conditions
C tidalPeriod             :: tidal period (s)
      COMMON /OBC_PARM_R/
     &     OBCS_balanceFacN, OBCS_balanceFacS,
     &     OBCS_balanceFacE, OBCS_balanceFacW,
     &     OBCS_uvApplyFac,
     &     OBCS_monitorFreq,
     &     tidalPeriod,
     & Urelaxobcsinner,Urelaxobcsbound,
     & Vrelaxobcsinner,Vrelaxobcsbound,
     & TrelaxStevens, SrelaxStevens
      _RL OBCS_balanceFacN, OBCS_balanceFacS
      _RL OBCS_balanceFacE, OBCS_balanceFacW
      _RL OBCS_uvApplyFac
      _RL OBCS_monitorFreq
      _RL tidalPeriod(tidalComponents)
      _RS Urelaxobcsinner
      _RS Urelaxobcsbound
      _RS Vrelaxobcsinner
      _RS Vrelaxobcsbound
      _RS TrelaxStevens
      _RS SrelaxStevens

C--   COMMON /OBC_FILES/ OBCS character-type parameter
C OB[N,S,E,W][u,v,w,t,s,eta,am,ph]File :: Files with boundary conditions,
C                                         the letter combinations mean:
C                     N/S/E/W   :: northern/southern/eastern/western boundary
C                     u/v/w/t/s :: ocean u/v/w velocities, temperature/salinity
C                     eta       :: sea surface height
C                     am/ph     :: tidal amplitude (m/s) / phase (s)
C OB[N,S,E,W]connectFile :: Files with connected piece Id for N/S/E/W OB grid pt
C insideOBmaskFile   :: File to specify Inside OB region mask (zero beyond OB)
      COMMON /OBC_FILES/
     &      OBNuFile,  OBSuFile,  OBEuFile,  OBWuFile,
     &      OBNvFile,  OBSvFile,  OBEvFile,  OBWvFile,
     &      OBNwFile,  OBSwFile,  OBEwFile,  OBWwFile,
     &      OBNtFile,  OBStFile,  OBEtFile,  OBWtFile,
     &      OBNsFile,  OBSsFile,  OBEsFile,  OBWsFile,
     &      OBNetaFile,OBSetaFile,OBEetaFile,OBWetaFile,
     &      OBNamFile, OBSamFile, OBEamFile, OBWamFile,
     &      OBNphFile, OBSphFile, OBEphFile, OBWphFile,
     &      OBNconnectFile, OBSconnectFile,
     &      OBEconnectFile, OBWconnectFile,
     &      insideOBmaskFile
      CHARACTER*(MAX_LEN_FNAM)
     &      OBNuFile,  OBSuFile,  OBEuFile,  OBWuFile,
     &      OBNvFile,  OBSvFile,  OBEvFile,  OBWvFile,
     &      OBNwFile,  OBSwFile,  OBEwFile,  OBWwFile,
     &      OBNtFile,  OBStFile,  OBEtFile,  OBWtFile,
     &      OBNsFile,  OBSsFile,  OBEsFile,  OBWsFile,
     &      OBNetaFile,OBSetaFile,OBEetaFile,OBWetaFile,
     &      OBNamFile, OBSamFile, OBEamFile, OBWamFile,
     &      OBNphFile, OBSphFile, OBEphFile, OBWphFile,
     &      OBNconnectFile, OBSconnectFile,
     &      OBEconnectFile, OBWconnectFile,
     &      insideOBmaskFile

#endif /* ALLOW_OBCS */
