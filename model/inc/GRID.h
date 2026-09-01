!
!BOP
!    !ROUTINE: GRID.h
!    !INTERFACE:
!    include GRID.h
!    !DESCRIPTION: \bv
! *==========================================================*
! | GRID.h
! | o Header file defining model grid.
! *==========================================================*
! | Model grid is defined for each process by reference to
! | the arrays set here.
! | Notes
! | =====
! | The standard MITgcm convention of westmost, southern most
! | and upper most having the (1,1,1) index is used here.
! | i.e.
! |----------------------------------------------------------
! | (1)  Plan view schematic of model grid (top layer i.e. )
! |      ================================= ( ocean surface )
! |                                        ( or top of     )
! |                                        ( atmosphere    )
! |      This diagram shows the location of the model
! |      prognostic variables on the model grid. The "T"
! |      location is used for all tracers. The figure also
! |      shows the southern most, western most indexing
! |      convention that is used for all model variables.
! |
! |
! |             V(i=1,                     V(i=Nx,
! |               j=Ny+1,                    j=Ny+1,
! |               k=1)                       k=1)
! |                /|\                       /|\  "PWX"
! |       |---------|------------------etc..  |---- *---
! |       |                     |                   *  |
! |"PWY"*******************************etc..  **********"PWY"
! |       |                     |                   *  |
! |       |                     |                   *  |
! |       |                     |                   *  |
! |U(i=1, ==>       x           |             x     *==>U
! |  j=Ny,|      T(i=1,         |          T(i=Nx,  *(i=Nx+1,
! |  k=1) |        j=Ny,        |            j=Ny,  *  |j=Ny,
! |       |        k=1)         |            k=1)   *  |k=1)
! |
! |       .                     .                      .
! |       .                     .                      .
! |       .                     .                      .
! |       e                     e                   *  e
! |       t                     t                   *  t
! |       c                     c                   *  c
! |       |                     |                   *  |
! |       |                     |                   *  |
! |U(i=1, ==>       x           |             x     *  |
! |  j=2, |      T(i=1,         |          T(i=Nx,  *  |
! |  k=1) |        j=2,         |            j=2,   *  |
! |       |        k=1)         |            k=1)   *  |
! |       |                     |                   *  |
! |       |        /|\          |            /|\    *  |
! |      -----------|------------------etc..  |-----*---
! |       |       V(i=1,        |           V(i=Nx, *  |
! |       |         j=2,        |             j=2,  *  |
! |       |         k=1)        |             k=1)  *  |
! |       |                     |                   *  |
! |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
! |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
! |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
! |       |        k=1)         |            k=1)   *  |k=1)
! |       |                     |                   *  |
! |       |        /|\          |            /|\    *  |
! |"SB"++>|---------|------------------etc..  |-----*---
! |      /+\      V(i=1,                    V(i=Nx, *
! |       +         j=1,                      j=1,  *
! |       +         k=1)                      k=1)  *
! |     "WB"                                      "PWX"
! |
! |   N, y increasing northwards
! |  /|\ j increasing northwards
! |   |
! |   |
! |   ======>E, x increasing eastwards
! |             i increasing eastwards
! |
! |    i: East-west index
! |    j: North-south index
!     |    k: up-down index
! |    U: x-velocity (m/s)
! |    V: y-velocity (m/s)
! |    T: potential temperature (oC)
! | "SB": Southern boundary
! | "WB": Western boundary
! |"PWX": Periodic wrap around in X.
! |"PWY": Periodic wrap around in Y.
! |----------------------------------------------------------
! | (2) South elevation schematic of model grid
! |     =======================================
! |     This diagram shows the location of the model
! |     prognostic variables on the model grid. The "T"
! |     location is used for all tracers. The figure also
! |     shows the upper most, western most indexing
! |     convention that is used for all model variables.
! |
! |      "WB"
! |       +
! |       +
! |      \+/       /|\                       /|\       .
! |"UB"++>|-------- | -----------------etc..  | ----*---
! |       |    rVel(i=1,        |        rVel(i=Nx, *  |
! |       |         j=1,        |             j=1,  *  |
! |       |         k=1)        |             k=1)  *  |
! |       |                     |                   *  |
! |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
! |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
! |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
! |       |        k=1)         |            k=1)   *  |k=1)
! |       |                     |                   *  |
! |       |        /|\          |            /|\    *  |
! |       |-------- | -----------------etc..  | ----*---
! |       |    rVel(i=1,        |        rVel(i=Nx, *  |
! |       |         j=1,        |             j=1,  *  |
! |       |         k=2)        |             k=2)  *  |
! |
! |       .                     .                      .
! |       .                     .                      .
! |       .                     .                      .
! |       e                     e                   *  e
! |       t                     t                   *  t
! |       c                     c                   *  c
! |       |                     |                   *  |
! |       |                     |                   *  |
! |       |                     |                   *  |
! |       |                     |                   *  |
! |       |        /|\          |            /|\    *  |
! |       |-------- | -----------------etc..  | ----*---
! |       |    rVel(i=1,        |        rVel(i=Nx, *  |
! |       |         j=1,        |             j=1,  *  |
! |       |         k=Nr)       |             k=Nr) *  |
! |U(i=1, ==>       x         ==>U(i=2,       x     *==>U
! |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,
! |  k=Nr)|        j=1,         |  k=Nr)     j=1,   *  |j=1,
! |       |        k=Nr)        |            k=Nr)  *  |k=Nr)
! |       |                     |                   *  |
! |"LB"++>==============================================
! |                                               "PWX"
! |
! | Up   increasing upwards.
! |/|\                                                       .
! | |
! | |
! | =====> E  i increasing eastwards
! | |         x increasing eastwards
! | |
! |\|/
! | Down,k increasing downwards.
! |
! | Note: r => height (m) => r increases upwards
! |       r => pressure (Pa) => r increases downwards
! |
! |
! |    i: East-west index
! |    j: North-south index
!     |    k: up-down index
! |    U: x-velocity (m/s)
! | rVel: z-velocity ( units of r )
! |       The vertical velocity variable rVel is in units of
! |       "r" the vertical coordinate. r in m will give
! |       rVel m/s. r in Pa will give rVel Pa/s.
! |    T: potential temperature (oC)
! | "UB": Upper boundary.
! | "LB": Lower boundary (always solid - therefore om|w == 0)
! | "WB": Western boundary
! |"PWX": Periodic wrap around in X.
! |----------------------------------------------------------
! | (3) Views showing nomenclature and indexing
! |     for grid descriptor variables.
! |
! |      Fig 3a. shows the orientation, indexing and
! |      notation for the grid spacing terms used internally
! |      for the evaluation of gradient and averaging terms.
! |      These varaibles are set based on the model input
! |      parameters which define the model grid in terms of
! |      spacing in X, Y and Z.
! |
! |      Fig 3b. shows the orientation, indexing and
! |      notation for the variables that are used to define
! |      the model grid. These varaibles are set directly
! |      from the model input.
! |
! | Figure 3a
! | =========
! |       |------------------------------------
! |       |                       |
! |"PWY"********************************* etc...
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |
! |       .                       .
! |       .                       .
! |       .                       .
! |       e                       e
! |       t                       t
! |       c                       c
! |       |-----------v-----------|-----------v----------|-
! |       |                       |                      |
! |       |                       |                      |
! |       |                       |                      |
! |       |                       |                      |
! |       |                       |                      |
! |       u<--dxF(i=1,j=2,k=1)--->u           t          |
! |       |/|\       /|\          |                      |
! |       | |         |           |                      |
! |       | |         |           |                      |
! |       | |         |           |                      |
! |       |dyU(i=1,  dyC(i=1,     |                      |
! | ---  ---|--j=2,---|--j=2,-----------------v----------|-
! | /|\   | |  k=1)   |  k=1)     |          /|\         |
! |  |    | |         |           |          dyF(i=2,    |
! |  |    | |         |           |           |  j=1,    |
! |dyG(   |\|/       \|/          |           |  k=1)    |
! |   i=1,u---        t<---dxC(i=2,j=1,k=1)-->t          |
! |   j=1,|                       |           |          |
! |   k=1)|                       |           |          |
! |  |    |                       |           |          |
! |  |    |                       |           |          |
! | \|/   |           |<---dxV(i=2,j=1,k=1)--\|/         |
! |"SB"++>|___________v___________|___________v__________|_
! |       <--dxG(i=1,j=1,k=1)----->
! |      /+\                                              .
! |       +
! |       +
! |     "WB"
! |
! |   N, y increasing northwards
! |  /|\ j increasing northwards
! |   |
! |   |
! |   ======>E, x increasing eastwards
! |             i increasing eastwards
! |
! |    i: East-west index
! |    j: North-south index
!     |    k: up-down index
! |    u: x-velocity point
! |    V: y-velocity point
!     |    t: tracer point
! | "SB": Southern boundary
! | "WB": Western boundary
! |"PWX": Periodic wrap around in X.
! |"PWY": Periodic wrap around in Y.
! |
! | Figure 3b
! | =========
! |
! |       .                       .
! |       .                       .
! |       .                       .
! |       e                       e
! |       t                       t
! |       c                       c
! |       |-----------v-----------|-----------v--etc...
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |       u<--delX(i=1)---------->u           t
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |       |                       |
! |       |-----------v-----------------------v--etc...
! |       |          /|\          |
! |       |           |           |
! |       |           |           |
! |       |           |           |
! |       u        delY(j=1)      |           t
! |       |           |           |
! |       |           |           |
! |       |           |           |
! |       |           |           |
! |       |          \|/          |
! |"SB"++>|___________v___________|___________v__etc...
! |      /+\                                                 .
! |       +
! |       +
! |     "WB"
! |
! *==========================================================*
! \ev
!EOP

! Macros that override/modify standard definitions
#include "GRID_MACROS.h"

!--   COMMON /GRID_RL/ RL valued grid defining variables.
! deepFacC  :: deep-model grid factor (fct of vertical only) for dx,dy
! deepFacF     at level-center (deepFacC)  and level interface (deepFacF)
! deepFac2C :: deep-model grid factor (fct of vertical only) for area dx*dy
! deepFac2F    at level-center (deepFac2C) and level interface (deepFac2F)
! gravitySign :: indicates the direction of gravity relative to R direction
!               (= -1 for R=Z (Z increases upward, -gravity direction  )
!               (= +1 for R=P (P increases downward, +gravity direction)
! rkSign     :: Vertical coordinate to vertical index orientation.
!               ( +1 same orientation, -1 opposite orientation )
! globalArea :: Domain Integrated horizontal Area ( m^2 )
! rAc_3dMean :: domain 3-d average of grid-cell horizontal area ( m^2 )
! n2dWetPts  :: number of non-empty columns (wet free-surface points)
!     n3dWetPts  :: number of non-empty grid points (wet grid points)
      COMMON /GRID_RL/                                                            &
     &      cosFacU, cosFacV, sqCosFacU, sqCosFacV,                               &
     &      deepFacC, deepFac2C, recip_deepFacC, recip_deepFac2C,                 &
     &      deepFacF, deepFac2F, recip_deepFacF, recip_deepFac2F,                 &
     &      gravitySign, rkSign,                                                  &
     &      globalArea, rAc_3dMean, n2dWetPts, n3dWetPts
      _RL cosFacU        (1-OLy:sNy+OLy,nSx,nSy)
      _RL cosFacV        (1-OLy:sNy+OLy,nSx,nSy)
      _RL sqCosFacU      (1-OLy:sNy+OLy,nSx,nSy)
      _RL sqCosFacV      (1-OLy:sNy+OLy,nSx,nSy)
      _RL deepFacC       (Nr)
      _RL deepFac2C      (Nr)
      _RL deepFacF       (Nr+1)
      _RL deepFac2F      (Nr+1)
      _RL recip_deepFacC (Nr)
      _RL recip_deepFac2C(Nr)
      _RL recip_deepFacF (Nr+1)
      _RL recip_deepFac2F(Nr+1)
      _RL gravitySign
      _RL rkSign
      _RL globalArea, rAc_3dMean
      _RL n2dWetPts, n3dWetPts

!--   COMMON /GRID_RS/ RS valued grid defining variables.
! dxC     :: Cell center separation in X across western cell wall (m)
! dxG     :: Cell face separation in X along southern cell wall (m)
! dxF     :: Cell face separation in X thru cell center (m)
! dxV     :: V-point separation in X across south-west corner of cell (m)
! dyC     :: Cell center separation in Y across southern cell wall (m)
!     dyG     :: Cell face separation in Y along western cell wall (m)
! dyF     :: Cell face separation in Y thru cell center (m)
! dyU     :: U-point separation in Y across south-west corner of cell (m)
! drC     :: Cell center separation along Z axis ( units of r ).
! drF     :: Cell face separation along Z axis ( units of r ).
! R_low   :: base of fluid in r_unit (Depth(m) / Pressure(Pa) at top Atmos.)
! rLowW   :: base of fluid column in r_unit at Western  edge location.
! rLowS   :: base of fluid column in r_unit at Southern edge location.
! Ro_surf :: surface reference (at rest) position, r_unit.
! rSurfW  :: surface reference position at Western  edge location [r_unit].
! rSurfS  :: surface reference position at Southern edge location [r_unit].
! hFac    :: Fraction of cell in vertical which is open i.e how
!          "lopped" a cell is (dimensionless scale factor).
!          Note: The code needs terms like MIN(hFac,hFac(I-1))
!                On some platforms it may be better to precompute
!                hFacW, hFacS, ... here than do MIN on the fly.
! maskInC :: Cell Center 2-D Interior mask (i.e., zero beyond OB)
! maskInW :: West  face 2-D Interior mask (i.e., zero on and beyond OB)
! maskInS :: South face 2-D Interior mask (i.e., zero on and beyond OB)
! maskC   :: cell Center land mask
! maskW   :: West face land mask
! maskS   :: South face land mask
! recip_dxC   :: Reciprocal of dxC
! recip_dxG   :: Reciprocal of dxG
! recip_dxF   :: Reciprocal of dxF
! recip_dxV   :: Reciprocal of dxV
! recip_dyC   :: Reciprocal of dxC
! recip_dyG   :: Reciprocal of dyG
! recip_dyF   :: Reciprocal of dyF
! recip_dyU   :: Reciprocal of dyU
! recip_drC   :: Reciprocal of drC
! recip_drF   :: Reciprocal of drF
! recip_Rcol  :: Inverse of cell center column thickness (1/r_unit)
! recip_hFacC :: Inverse of cell open-depth f[X,Y,Z] ( dimensionless ).
! recip_hFacW    rhFacC center, rhFacW west, rhFacS south.
! recip_hFacS   Note: This is precomputed here because it involves division.
! xC     :: X-coordinate of cell center f[X,Y]. The units of xc, yc
!           depend on the grid. They are not used in differencing or
!           averaging but are just a convient quantity for I/O,
!           diagnostics etc.. As such xc is in m for cartesian
!           coordinates but degrees for spherical polar.
! yC     :: Y-coordinate of center of cell f[X,Y].
! yG     :: Y-coordinate of corner of cell (c-grid vorticity point) f[X,Y].
! rA     :: R-face are f[X,Y] ( m^2 ).
!           Note: In a cartesian framework rA is simply dx*dy,
!               however we use rA to allow for non-globally
!               orthogonal coordinate frames (with appropriate
!               metric terms).
! rC     :: R-coordinate of center of cell f[Z] (units of r).
! rF     :: R-coordinate of face of cell f[Z] (units of r).
! - *HybSigm* - :: Hybrid-Sigma vert. Coord coefficients
! aHybSigmF    at level-interface (*HybSigmF) and level-center (*HybSigmC)
! aHybSigmC    aHybSigm* = constant r part, bHybSigm* = sigma part, such as
! bHybSigmF    r(ij,k,t) = rLow(ij) + aHybSigm(k)*[rF(1)-rF(Nr+1)]
! bHybSigmC              + bHybSigm(k)*[eta(ij,t)+Ro_surf(ij) - rLow(ij)]
! dAHybSigF :: vertical increment of Hybrid-Sigma coeff.: constant r part,
! dAHybSigC    between interface (dAHybSigF) and between center (dAHybSigC)
! dBHybSigF :: vertical increment of Hybrid-Sigma coefficient: sigma part,
! dBHybSigC    between interface (dBHybSigF) and between center (dBHybSigC)
! tanPhiAtU :: tan of the latitude at U point. Used for spherical polar
!              metric term in U equation.
! tanPhiAtV :: tan of the latitude at V point. Used for spherical polar
!              metric term in V equation.
! angleCosC :: cosine of grid orientation angle relative to Geographic
! direction at cell center: alpha=(Eastward_dir,grid_uVel_dir)=(North_d,vVel_d)
! angleSinC :: sine   of grid orientation angle relative to Geographic
! direction at cell center: alpha=(Eastward_dir,grid_uVel_dir)=(North_d,vVel_d)
! u2zonDir  :: cosine of grid orientation angle at U point location
! v2zonDir  :: minus sine of  orientation angle at V point location
! fCori     :: Coriolis parameter at grid Center point
! fCoriG    :: Coriolis parameter at grid Corner point
! fCoriCos  :: Coriolis Cos(phi) parameter at grid Center point (for NH)

      COMMON /GRID_RS/                                                            &
     &      dxC,dxF,dxG,dxV,dyC,dyF,dyG,dyU,                                      &
     &      rLowW, rLowS,                                                         &
     &      Ro_surf, rSurfW, rSurfS,                                              &
     &      recip_dxC,recip_dxF,recip_dxG,recip_dxV,                              &
     &      recip_dyC,recip_dyF,recip_dyG,recip_dyU,                              &
     &      xC,yC,rA,rAw,rAs,rAz,xG,yG,                                           &
     &      maskInC, maskInW, maskInS,                                            &
     &      maskC, maskW, maskS,                                                  &
     &      recip_rA,recip_rAw,recip_rAs,recip_rAz,                               &
     &      drC, drF, recip_drC, recip_drF, rC, rF,                               &
     &      aHybSigmF, bHybSigmF, aHybSigmC, bHybSigmC,                           &
     &      dAHybSigF, dBHybSigF, dBHybSigC, dAHybSigC,                           &
     &      tanPhiAtU, tanPhiAtV,                                                 &
     &      angleCosC, angleSinC, u2zonDir, v2zonDir,                             &
     &      fCori, fCoriG, fCoriCos
      _RS dxC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxV            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyU            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rLowW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rLowS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS Ro_surf        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rSurfW         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rSurfS         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dxC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dxF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dxG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dxV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS xC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS xG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rA             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rAw            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rAs            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rAz            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rAw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rAs      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rAz      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS maskInC        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS maskInW        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS maskInS        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS maskC          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS maskW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS maskS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS drC            (Nr+1)
      _RS drF            (Nr)
      _RS recip_drC      (Nr+1)
      _RS recip_drF      (Nr)
      _RS rC             (Nr)
      _RS rF             (Nr+1)
      _RS aHybSigmF      (Nr+1)
      _RS bHybSigmF      (Nr+1)
      _RS aHybSigmC      (Nr)
      _RS bHybSigmC      (Nr)
      _RS dAHybSigF      (Nr)
      _RS dBHybSigF      (Nr)
      _RS dBHybSigC      (Nr+1)
      _RS dAHybSigC      (Nr+1)
      _RS tanPhiAtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS tanPhiAtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS angleCosC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS angleSinC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS u2zonDir       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS v2zonDir       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fCori          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fCoriG         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS fCoriCos       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!--   COMMON /GRID_VAR_RS/ potentially time-dependent or active RS
! valued grid defining variables. These grid defining variables are
! time-dependent when using a non-linear free surface, or they are
! active in an AD sense when using depth as a control parameter, or
! both.
      COMMON /GRID_VAR_RS/                                                        &
     &      hFacC, hFacW, hFacS,                                                  &
     &      recip_hFacC,recip_hFacW,recip_hFacS,                                  &
     &      R_low, recip_Rcol
      _RS hFacC          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS hFacW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS hFacS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS recip_hFacC    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS recip_hFacW    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS recip_hFacS    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS R_low          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_Rcol     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_DEPTH_CONTROL
!--   COMMON /GRID_DEPTH_CTRL/ grid defining variables for Depth Control code.
! xx_r_low  :: in TAF-sense active replacement of R_low
      COMMON /GRID_DEPTH_CTRL/                                                    &
     &      xx_r_low
      _RL xx_r_low       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DEPTH_CONTROL */

!--   COMMON /GRID_I/ INTEGER valued grid defining variables.
! kSurfC  :: vertical index of the surface tracer cell
! kSurfW  :: vertical index of the surface U point
! kSurfS  :: vertical index of the surface V point
! kLowC   :: index of the r-lowest "wet cell" (2D)
! IMPORTANT: kLowC = 0 and kSurfC,W,S = Nr+1 (or =Nr+2 on a thin-wall)
!        where the fluid column is empty (continent)
      COMMON /GRID_I/                                                             &
     &      kSurfC, kSurfW, kSurfS,                                               &
     &      kLowC
      INTEGER :: kSurfC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER :: kSurfW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER :: kSurfS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER :: kLowC (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
