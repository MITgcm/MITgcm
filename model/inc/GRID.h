C $Header: /u/gcmpack/MITgcm/model/inc/GRID.h,v 1.14 2001/02/04 14:38:44 cnh Exp $
C $Name:  $
C
C     /==========================================================\
C     | GRID.h                                                   |
C     | o Header file defining model grid.                       |
C     |==========================================================|
C     | Model grid is defined for each process by reference to   |
C     | the arrays set here.                                     |
C     | Notes                                                    |
C     | =====                                                    |
C     | The standard MITgcm convention of westmost, southern most|
C     | and upper most having the (1,1,1) index is used here.    |
C     | i.e.                                                     |
C     |----------------------------------------------------------|
C     | (1)  Plan view schematic of model grid (top layer i.e. ) |
C     |      ================================= ( ocean surface ) |
C     |                                        ( or top of     ) |
C     |                                        ( atmosphere    ) |
C     |      This diagram shows the location of the model        |
C     |      prognostic variables on the model grid. The "T"     |
C     |      location is used for all tracers. The figure also   |
C     |      shows the southern most, western most indexing      |
C     |      convention that is used for all model variables.    |
C     |                                                          |
C     |                                                          |
C     |             V(i=1,                     V(i=Nx,           |
C     |               j=Ny+1,                    j=Ny+1,         |
C     |               k=1)                       k=1)            |
C     |                /|\                       /|\  "PWX"      |
C     |       |---------|------------------etc..  |---- *---     |
C     |       |                     |                   *  |     |
C     |"PWY"*******************************etc..  **********"PWY"|
C     |       |                     |                   *  |     |
C     |       |                     |                   *  |     |
C     |       |                     |                   *  |     |
C     |U(i=1, ==>       x           |             x     *==>U    |
C     |  j=Ny,|      T(i=1,         |          T(i=Nx,  *(i=Nx+1,|
C     |  k=1) |        j=Ny,        |            j=Ny,  *  |j=Ny,|
C     |       |        k=1)         |            k=1)   *  |k=1) |
C     |                                                          |
C     |       .                     .                      .     |
C     |       .                     .                      .     |
C     |       .                     .                      .     |
C     |       e                     e                   *  e     |
C     |       t                     t                   *  t     |
C     |       c                     c                   *  c     |
C     |       |                     |                   *  |     |
C     |       |                     |                   *  |     |
C     |U(i=1, ==>       x           |             x     *  |     |
C     |  j=2, |      T(i=1,         |          T(i=Nx,  *  |     |
C     |  k=1) |        j=2,         |            j=2,   *  |     |
C     |       |        k=1)         |            k=1)   *  |     |
C     |       |                     |                   *  |     |
C     |       |        /|\          |            /|\    *  |     |
C     |      -----------|------------------etc..  |-----*---     |
C     |       |       V(i=1,        |           V(i=Nx, *  |     |
C     |       |         j=2,        |             j=2,  *  |     |
C     |       |         k=1)        |             k=1)  *  |     |
C     |       |                     |                   *  |     |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U    |
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,|
C     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1, |
C     |       |        k=1)         |            k=1)   *  |k=1) |
C     |       |                     |                   *  |     |
C     |       |        /|\          |            /|\    *  |     |
C     |"SB"++>|---------|------------------etc..  |-----*---     |
C     |      /+\      V(i=1,                    V(i=Nx, *        |
C     |       +         j=1,                      j=1,  *        |
C     |       +         k=1)                      k=1)  *        |
C     |     "WB"                                      "PWX"      |
C     |                                                          |
C     |   N, y increasing northwards                             |
C     |  /|\ j increasing northwards                             |
C     |   |                                                      |
C     |   |                                                      |
C     |   ======>E, x increasing eastwards                       |
C     |             i increasing eastwards                       |
C     |                                                          |
C     |    i: East-west index                                    |
C     |    j: North-south index                                  |
C     |    k: up-down index                                      |
C     |    U: x-velocity (m/s)                                   |
C     |    V: y-velocity (m/s)                                   |
C     |    T: potential temperature (oC)                         |
C     | "SB": Southern boundary                                  |
C     | "WB": Western boundary                                   |
C     |"PWX": Periodic wrap around in X.                         |
C     |"PWY": Periodic wrap around in Y.                         |
C     |----------------------------------------------------------|
C     | (2) South elevation schematic of model grid              |
C     |     =======================================              |
C     |     This diagram shows the location of the model         |
C     |     prognostic variables on the model grid. The "T"      |
C     |     location is used for all tracers. The figure also    |
C     |     shows the upper most, western most indexing          |
C     |     convention that is used for all model variables.     |
C     |                                                          |
C     |      "WB"                                                |
C     |       +                                                  |
C     |       +                                                  |
C     |      \+/       /|\                       /|\             |
C     |"UB"++>|-------- | -----------------etc..  | ----*---     |
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |     |
C     |       |         j=1,        |             j=1,  *  |     |
C     |       |         k=1)        |             k=1)  *  |     |
C     |       |                     |                   *  |     |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U    |
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,|
C     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1, |
C     |       |        k=1)         |            k=1)   *  |k=1) |
C     |       |                     |                   *  |     |
C     |       |        /|\          |            /|\    *  |     |
C     |       |-------- | -----------------etc..  | ----*---     |
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |     |
C     |       |         j=1,        |             j=1,  *  |     |
C     |       |         k=2)        |             k=2)  *  |     |
C     |                                                          |
C     |       .                     .                      .     |
C     |       .                     .                      .     |
C     |       .                     .                      .     |
C     |       e                     e                   *  e     |
C     |       t                     t                   *  t     |
C     |       c                     c                   *  c     |
C     |       |                     |                   *  |     |
C     |       |                     |                   *  |     |
C     |       |                     |                   *  |     |
C     |       |                     |                   *  |     |
C     |       |        /|\          |            /|\    *  |     |
C     |       |-------- | -----------------etc..  | ----*---     |
C     |       |    rVel(i=1,        |        rVel(i=Nx, *  |     |
C     |       |         j=1,        |             j=1,  *  |     |
C     |       |         k=Nr)       |             k=Nr) *  |     |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U    |
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,|
C     |  k=Nr)|        j=1,         |  k=Nr)     j=1,   *  |j=1, |
C     |       |        k=Nr)        |            k=Nr)  *  |k=Nr)|
C     |       |                     |                   *  |     |
C     |"LB"++>==============================================     |
C     |                                               "PWX"      |
C     |                                                          |
C     | Up   increasing upwards.                                 | 
C     |/|\                                                       |
C     | |                                                        |
C     | |                                                        |
C     | =====> E  i increasing eastwards                         |
C     | |         x increasing eastwards                         |
C     | |                                                        |
C     |\|/                                                       |
C     | Down,k increasing downwards.                             |
C     |                                                          |
C     | Note: r => height (m) => r increases upwards             |
C     |       r => pressure (Pa) => r increases downwards        |
C     |                                                          |
C     |                                                          |
C     |    i: East-west index                                    |
C     |    j: North-south index                                  |
C     |    k: up-down index                                      |
C     |    U: x-velocity (m/s)                                   |
C     | rVel: z-velocity ( units of r )                          |
C     |       The vertical velocity variable rVel is in units of |
C     |       "r" the vertical coordinate. r in m will give      |
C     |       rVel m/s. r in Pa will give rVel Pa/s.             |
C     |    T: potential temperature (oC)                         |
C     | "UB": Upper boundary.                                    |
C     | "LB": Lower boundary (always solid - therefore om|w == 0)|
C     | "WB": Western boundary                                   |
C     |"PWX": Periodic wrap around in X.                         |
C     |----------------------------------------------------------|
C     | (3) Views showing nomenclature and indexing              |
C     |     for grid descriptor variables.                       |
C     |                                                          |
C     |      Fig 3a. shows the orientation, indexing and         |
C     |      notation for the grid spacing terms used internally |
C     |      for the evaluation of gradient and averaging terms. |
C     |      These varaibles are set based on the model input    |
C     |      parameters which define the model grid in terms of  |
C     |      spacing in X, Y and Z.                              |
C     |                                                          |
C     |      Fig 3b. shows the orientation, indexing and         |
C     |      notation for the variables that are used to define  |
C     |      the model grid. These varaibles are set directly    |
C     |      from the model input.                               |
C     |                                                          |
C     | Figure 3a                                                |
C     | =========                                                |
C     |       |------------------------------------              |
C     |       |                       |                          |
C     |"PWY"********************************* etc...             |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |                                                          |
C     |       .                       .                          |
C     |       .                       .                          |
C     |       .                       .                          |
C     |       e                       e                          |
C     |       t                       t                          |
C     |       c                       c                          |
C     |       |-----------v-----------|-----------v----------|-  |
C     |       |                       |                      |   |
C     |       |                       |                      |   |
C     |       |                       |                      |   |
C     |       |                       |                      |   |
C     |       |                       |                      |   |
C     |       u<--dxF(i=1,j=2,k=1)--->u           t          |   |
C     |       |/|\       /|\          |                      |   |
C     |       | |         |           |                      |   |
C     |       | |         |           |                      |   |
C     |       | |         |           |                      |   |
C     |       |dyU(i=1,  dyC(i=1,     |                      |   |
C     | ---  ---|--j=2,---|--j=2,-----------------v----------|-  |
C     | /|\   | |  k=1)   |  k=1)     |          /|\         |   |
C     |  |    | |         |           |          dyF(i=2,    |   |
C     |  |    | |         |           |           |  j=1,    |   |
C     |dyG(   |\|/       \|/          |           |  k=1)    |   |
C     |   i=1,u---        t<---dxC(i=2,j=1,k=1)-->t          |   |
C     |   j=1,|                       |           |          |   |
C     |   k=1)|                       |           |          |   |
C     |  |    |                       |           |          |   |
C     |  |    |                       |           |          |   |
C     | \|/   |           |<---dxV(i=2,j=1,k=1)--\|/         |   |
C     |"SB"++>|___________v___________|___________v__________|_  |
C     |       <--dxG(i=1,j=1,k=1)----->                          |
C     |      /+\                                                 |
C     |       +                                                  |
C     |       +                                                  |
C     |     "WB"                                                 |
C     |                                                          |
C     |   N, y increasing northwards                             |
C     |  /|\ j increasing northwards                             |
C     |   |                                                      |
C     |   |                                                      |
C     |   ======>E, x increasing eastwards                       |
C     |             i increasing eastwards                       |
C     |                                                          |
C     |    i: East-west index                                    |
C     |    j: North-south index                                  |
C     |    k: up-down index                                      |
C     |    u: x-velocity point                                   |
C     |    V: y-velocity point                                   |
C     |    t: tracer point                                       |
C     | "SB": Southern boundary                                  |
C     | "WB": Western boundary                                   |
C     |"PWX": Periodic wrap around in X.                         |
C     |"PWY": Periodic wrap around in Y.                         |
C     |                                                          |
C     | Figure 3b                                                |
C     | =========                                                |
C     |                                                          |
C     |       .                       .                          |
C     |       .                       .                          |
C     |       .                       .                          |
C     |       e                       e                          |
C     |       t                       t                          |
C     |       c                       c                          |
C     |       |-----------v-----------|-----------v--etc...      |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       u<--delX(i=1)---------->u           t              |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |                       |                          |
C     |       |-----------v-----------------------v--etc...      |
C     |       |          /|\          |                          |
C     |       |           |           |                          |
C     |       |           |           |                          |
C     |       |           |           |                          |
C     |       u        delY(j=1)      |           t              |
C     |       |           |           |                          |
C     |       |           |           |                          |
C     |       |           |           |                          |
C     |       |           |           |                          |
C     |       |          \|/          |                          |
C     |"SB"++>|___________v___________|___________v__etc...      |
C     |      /+\                                                 |
C     |       +                                                  |
C     |       +                                                  |
C     |     "WB"                                                 |
C     |                                                          |
C     \==========================================================/

C     Macros that override/modify standard definitions
#include "GRID_MACROS.h"

C
C--   COMMON /GRID_R/ REAL valued grid defining variables.
C     dxC    - Cell center separation in X across western cell wall (m)
C     dxG    - Cell face separation in X along southern cell wall (m)
C     dxF    - Cell face separation in X thru cell center (m)
C     dxV    - V-point separation in X across south-west corner of cell (m)
C     dyC    - Cell center separation in Y across southern cell wall (m)
C     dyG    - Cell face separation in Y along western cell wall (m)
C     dyF    - Cell face separation in Y thru cell center (m)
C     dyU    - U-point separation in Y across south-west corner of cell (m)
C     drC    - Cell center separation along Z axis ( units of r ).
C     drF    - Cell face separation along Z axis ( units of r ).
C     H      - Depth of base of fluid from upper surface f[X,Y] (m).
C     hFac   - Fraction of cell in vertical which is open i.e how 
C              "lopped" a cell is (dimensionless scale factor).
C              Note: The code needs terms like MIN(hFac,hFac(I+1))
C                    On some platforms it may be better to precompute
C                    hFacW, hFacE, ... here than do MIN on the fly.
C     rkFac     - Vertical coordinate to vertical index orientation.
C                 ( -1 same orientation, 1 opposite orientation )
C                 ( vertical coord == m  -> rkFac =  1 )
C                 ( vertical coord == Pa -> rkFac = -1 )
C     maskW  - West face land mask
C     maskS  - South face land mask
C     recip_dxC   - Recipricol of dxC
C     recip_dxG   - Recipricol of dxG
C     recip_dxF   - Recipricol of dxF
C     recip_dxV   - Recipricol of dxV
C     recip_dyC   - Recipricol of dxC
C     recip_dyG   - Recipricol of dyG
C     recip_dyF   - Recipricol of dyF
C     recip_dyU   - Recipricol of dyU
C     recip_drC   - Recipricol of drC
C     recip_drF   - Recipricol of drF
C     recip_H     - Inverse of cell center open-depth
C     recip_hFacC - Inverse of cell open-depth f[X,Y,Z] ( dimensionless ).
C     recip_hFacW   rhFacC center, rhFacW west, rhFacS south.
C     recip_hFacS   Note: This is precomputed here because it involves division.
C     saFac  - Shallow atmosphere factor (dimensionless scale factor).
C     xC     - X-coordinate of center of cell f[X,Y]. The units of xc, yc
C              depend on the grid. They are not used in differencing or
C              averaging but are just a convient quantity for I/O,
C              diagnostics etc.. As such xc is in m for cartesian 
C              coordinates but degrees for spherical polar.
C     yC     - Y-coordinate of center of cell f[X,Y].
C     yG     - Y-coordinate of corner of cell ( c-grid vorticity point) f[X,Y]. 
C     xC0, yC0 - West edge x coord  ( metres or degrees )
C                South edge y coord ( metres or degrees )
C     rA     - R-face are f[X,Y] ( m^2 ).
C              Note: In a cartesian framework zA is simply dx*dy,
C                    however we use zA to allow for non-globally
C                    orthogonal coordinate frames (with appropriate
C                    metric terms).
C     rC     - R-coordinate of center of cell f[Z] (units of r).
C     rF     - R-coordinate of face of cell f[Z] (units of r).
C     tanPhiAtU - tan of the latitude at U point. Used for spherical polar 
C                 metric term in U equation.
C     tanPhiAtV - tan of the latitude at V point. Used for spherical polar 
C                 metric term in V equation.
      COMMON /GRID_R/
     &  dxC,dxF,dxG,dxV,dyC,dyF,dyG,dyU,drC,drF,
     &  H,HFacC,HFacW,HFacS,DepthInK,
     &  recip_dxC,recip_dxF,recip_dxG,recip_dxV,
     &  recip_dyC,recip_dyF,recip_dyG,recip_dyU,
     &  recip_drC,recip_drF,
     &  recip_H, 
     &  recip_hFacC,recip_hFacW,recip_hFacS, 
     &  rkFac, recip_rkFac,
     &  saFac,
     &  xC,yC,rA,rAw,rAs,rAz,rC,rF,yC0,xC0,xG,yG,
     &  maskW,maskS,recip_rA,recip_rAw,recip_rAs,recip_rAz,
     &  tanPhiAtU, tanPhiAtV
      _RS dxC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxV            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyC            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyF            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyG            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyU            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS drC            (1:Nr)
      _RS drF            (1:Nr)
      _RS rkFac
      _RS recip_rkFac
      _RS DepthInK       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS H              (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS HFacC          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS HFacW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS HFacS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS recip_dxC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dxF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dxG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dxV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyF      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_dyU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_drC      (1:Nr)
      _RS recip_drF      (1:Nr)
      _RS recip_h        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_hFacC    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS recip_hFacW    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS recip_hFacS    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS saFac          (1:Nr)
      _RS xC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS xG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS xC0
      _RS yC             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yG             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yC0
      _RS rA             (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rAw            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rAs            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rAz            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rA       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rAw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rAs      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS recip_rAz      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rC             (1:Nr)
      _RS rF             (1:Nr+1)
      _RS maskW          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS maskS          (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RS tanPhiAtU      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS tanPhiAtV      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_NONHYDROSTATIC
      COMMON /GRID_NH/
     &  recip_hFacU
      _RS recip_hFacU    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
#endif
