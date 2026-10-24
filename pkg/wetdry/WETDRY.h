CBOP
C     !ROUTINE: WETDRY.h
C     !INTERFACE:
C     #include "WETDRY.h"

C     !DESCRIPTION:
C     *================================================================*
C     | WETDRY.h
C     | o Header file for pkg "wetdry" (wetting and drying): thin-film
C     |   scheme with dynamic face masking, for r* non-linear
C     |   free surface.
C     *================================================================*
CEOP

C--   wetdry parameters (namelist WETDRY_PARM01 in data.wetdry)
C     wdMinDepth    :: thickness of the water film kept in a dry column [m]
C     wdCritDepth   :: a face is open when the highest surface on either
C                       side stands more than this above the face bed
C                       crest [m] ; must be > wdMinDepth
C     wdDragDepth   :: below this face depth, velocity is relaxed
C                       linearly to zero at wdMinDepth [m] ;
C                       no relaxation if <= wdMinDepth (default 0)
C     wdAdvDepth    :: below this face flow depth, momentum advection
C                       (pkg/mom_vecinv) is ramped linearly to zero at
C                       wdCritDepth [m] ; off if <= wdCritDepth
C     wdGMDepth     :: below this column depth, the GM/Redi tensor
C                       (pkg/gmredi) is ramped linearly to zero at
C                       wdCritDepth [m] ; off if <= wdCritDepth
C     wdKPPDepth    :: below this column depth, the KPP non-local
C                       transport (KPPghat) is ramped linearly to zero at
C                       wdCritDepth [m] ; off if <= wdCritDepth
C                       (default)
C     wdKPPCapDepth :: in columns shallower than this [m], the KPP
C                       non-local flux fraction Kz*KPPghat is capped at 1
C                       (WETDRY_KPP_TAPER); deeper columns keep stock KPP.
C                       <= 0: no cap
C     wdSmoothWidth :: width of tanh face-mask ramp [m] (not implemented)
C     wdMonFreq     :: frequency of wetdry monitor output [s]
C     wdMaxSpeed    :: stop, and report where, when a velocity on an
C                       open face exceeds this [m/s] ; off if <= 0
C     wdConserveVol :: remove the volume added by the film floor from
C                       wet neighbours (through open faces)
C     wdUpwindFace  :: thickness of an open face from the
C                       upwind surface height (default ; if .FALSE.: the
C                       lower one with select_rStar=0, the mean with r*)
C     wdDryForcing  :: no surface heat/salt/fresh-water/short-wave or
C                       pTracer surface forcing on dry columns (ramp
C                       from wdCritDepth to 2*wdCritDepth)
C     wdManningN    :: Manning roughness [s m^-1/3] for a depth-dependent
C                       quadratic drag Cd = g n^2/D^(1/3), limited to
C                       bottomDragQuadratic .. wdDragMax (WETDRY_BOTDRAG,
C                       needs ALLOW_BOTTOMDRAG_ROUGHNESS) ; off if <= 0
C     wdDragMax     :: upper limit of that drag coefficient
C     wdManningFile :: optional 2-D extra Manning roughness [s m^-1/3] at
C                       tracer points (e.g. river ice cover and ice jams),
C                       added to wdManningN on each face (the larger of
C                       the two neighbours), times a factor 1 until
C                       wdManningT1, falling linearly to 0 at
C                       wdManningT2 [s, model time] (constant if
C                       wdManningT2 <= wdManningT1)
C     wdMaxFroude   :: cap the speed on each open interior face at
C                       wdMaxFroude*sqrt(g D) (D the face flow depth, as
C                       for the Manning drag), applied to u(n+1) before
C                       the outflow limiter, so volume and tracers stay
C                       conserved ; for fast thin sheets pouring off
C                       banks (breakup floods) ; off if <= 0
C     wdCarryVel    :: a face that opens takes the depth-mean u* of the
C                       upstream face of its wet cell (the advancing
C                       front then converges with resolution) ; with
C                       Nr > 1 it needs momImplVertAdv=.TRUE.
      _RL wdMinDepth
      _RL wdCritDepth
      _RL wdDragDepth
      _RL wdAdvDepth
      _RL wdGMDepth
      _RL wdKPPDepth, wdKPPCapDepth
      _RL wdSmoothWidth
      _RL wdMonFreq
      _RL wdMaxSpeed
      _RL wdManningN, wdDragMax, wdMaxFroude
      _RL wdManningT1, wdManningT2
      CHARACTER*(MAX_LEN_FNAM) wdManningFile
      LOGICAL wdConserveVol, wdCarryVel, wdUpwindFace
      LOGICAL wdDryForcing
      COMMON /WETDRY_PARAMS_R/
     &     wdMinDepth, wdCritDepth, wdDragDepth, wdAdvDepth,
     &     wdGMDepth,
     &     wdKPPDepth, wdKPPCapDepth,
     &     wdSmoothWidth, wdMonFreq, wdMaxSpeed,
     &     wdManningN, wdDragMax, wdMaxFroude,
     &     wdManningT1, wdManningT2
      COMMON /WETDRY_PARAMS_C/ wdManningFile
      COMMON /WETDRY_PARAMS_L/
     &     wdConserveVol, wdCarryVel, wdUpwindFace,
     &     wdDryForcing

C--   wetdry fields
C     wdMaskW/S  :: 1 = face open, 0 = face closed by wetdry
C     wdMaskW0/S0:: static (initial) maskW/S, before wetdry masking
C     wdDryC     :: 1 where the column is at the film floor (dry)
C     wdVolAdj   :: cumulative volume added by the film floor, net of
C                    what was taken from wet neighbours [m^3]
C     wdVolAdjStep :: same, for the latest time step [m^3]
C     wdNflips   :: number of face open/close changes, latest step
C     wdRelaxW/S :: thin-water velocity relaxation factor at U/V points
C                    (1 = no relaxation, 0 = velocity set to zero)
C     wdAdvFacW/S:: momentum-advection factor at U/V points (0..1)
C     wdOutFac   :: outflow limiter factor of the latest step: outgoing
C                    velocities of a column are scaled by this so that
C                    it keeps at least wdMinDepth (1 = not limited)
C     wdNlimit   :: number of columns limited, latest step
C     wdVol0     :: initial volume of the interior (maskInC) domain [m^3]
C     wdCumIn    :: cumulative volume that entered the interior domain
C                    through its lateral boundary (e.g. OBCS) [m^3]
      _RS wdMaskW  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wdMaskS  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wdDryC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wdMaskW0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS wdMaskS0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
C     wdManEx    :: extra Manning roughness at tracer points
C                    (wdManningFile; 0 without it)
      _RS wdManEx  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /WETDRY_FIELDS_RS/
     &     wdMaskW, wdMaskS, wdDryC, wdMaskW0, wdMaskS0,
     &     wdManEx
      _RL wdRelaxW (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wdRelaxS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wdAdvFacW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wdAdvFacS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wdOutFac (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wdVolAdj, wdVolAdjStep, wdVol0, wdCumIn
C     wdEvLim    :: net evaporation (EmPmR > 0) removed by the dry-column
C                    ramp and the outflow limiter, latest step [kg/m^2/s]
      _RL wdEvLim  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /WETDRY_FIELDS_RL/
     &     wdRelaxW, wdRelaxS, wdAdvFacW, wdAdvFacS,
     &     wdOutFac,
     &     wdVolAdj, wdVolAdjStep, wdVol0, wdCumIn, wdEvLim
C     wdUpW/S    :: sign (+1, -1, 0) of the surface velocity that chose
C                    the upwind side of each face (wdUpwindFace); set in
C                    the in-step WETDRY_UPDATE_MASKS and saved in the pickup
C                    (pickup_wetdryface) so that a restart uses the same side
      _RL wdUpW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL wdUpS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /WETDRY_UPWIND_RL/ wdUpW, wdUpS
C     wdQswIn/Out :: Qsw before / after the dry-column scaling of the
C                     last call of WETDRY_DRY_FORCING; Qsw is not reloaded
C                     every step, so an unchanged Qsw (= wdQswOut) is
C                     scaled again from wdQswIn, not from itself
      _RS wdQswIn  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wdQswOut (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /WETDRY_QSW_RS/ wdQswIn, wdQswOut
      INTEGER wdNflips, wdNlimit
      COMMON /WETDRY_FIELDS_I/
     &     wdNflips, wdNlimit
