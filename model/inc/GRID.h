C $Header: /u/gcmpack/MITgcm/model/inc/GRID.h,v 1.3 1998/05/26 21:29:44 cnh Exp $
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
C     |      \+/        |                         |              |
C     |"UB"++>|--------\|/-----------------etc.. \|/----*---     |
C     |       |    w,om(i=1,        |        w,om(i=Nx, *  |     |
C     |       |         j=1,        |             j=1,  *  |     |
C     |       |         k=1)        |             k=1)  *  |     |
C     |       |                     |                   *  |     |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U    |
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,|
C     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1, |
C     |       |        k=1)         |            k=1)   *  |k=1) |
C     |       |                     |                   *  |     |
C     |       |         |           |             |     *  |     |
C     |       |--------\|/-----------------etc.. \|/----*---     |
C     |       |    w,om(i=1,        |        w,om(i=Nx, *  |     |
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
C     |       |         |           |             |     *  |     |
C     |       |--------\|/-----------------etc.. \|/----*---     |
C     |       |    w,om(i=1,        |        w,om(i=Nx, *  |     |
C     |       |         j=1,        |             j=1,  *  |     |
C     |       |         k=Nz)       |             k=Nz) *  |     |
C     |U(i=1, ==>       x         ==>U(i=2,       x     *==>U    |
C     |  j=1, |      T(i=1,         |  j=1,    T(i=Nx,  *(i=Nx+1,|
C     |  k=Nz)|        j=1,         |  k=Nz)     j=1,   *  |j=1, |
C     |       |        k=Nz)        |            k=Nz)  *  |k=Nz)|
C     |       |                     |                   *  |     |
C     |"LB"++>==============================================     |
C     |                                               "PWX"      |
C     |                                                          |
C     | Up,z increasing upwards.                                 |
C     |/|\                                                       |
C     | |                                                        |
C     | |                                                        |
C     | =====> E  i increasing eastwards                         |
C     | |         x increasing eastwards                         |
C     | |                                                        |
C     |\|/                                                       |
C     | Down,k increasing downwards.                             |
C     |                                                          |
C     |    i: East-west index                                    |
C     |    j: North-south index                                  |
C     |    k: up-down index                                      |
C     |    U: x-velocity (m/s)                                   |
C     | w,om: z-velocity (Pa/s - therefore +ve is down)          |
C     |       The vertical velocity variable is sometimes named  |
C     |       "w" in the model code. The vertical coordinate and |
C     |       also gridding are both under review - watch this   |
C     |       space!                                             |
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
C     dzC    - Cell center separation in Z (Pa).
C     dzF    - Cell face separation in Z (Pa).
C     H      - Depth of base of fluid from upper surface f[X,Y] (m).
C     hFac   - Fraction of cell in vertical which is open i.e how 
C              "lopped" a cell is (dimensionless scale factor).
C              Note: The code needs terms like MIN(hFac,hFac(I+1))
C                    On some platforms it may be better to precompute
C                    hFacW, hFacE, ... here than do MIN on the fly.
C     maskW  - West face land mask
C     maskS  - South face land mask
C     rdxC   - Recipricol of dxC
C     rdxG   - Recipricol of dxG
C     rdxF   - Recipricol of dxF
C     rdxV   - Recipricol of dxV
C     rdyC   - Recipricol of dxC
C     rdyG   - Recipricol of dyG
C     rdyF   - Recipricol of dyF
C     rdyU   - Recipricol of dyU
C     rdzC   - Recipricol of dzC
C     rdzF   - Recipricol of dzF
C     rh     - Inverse of cell center open-depth
C     rhFacC - Inverse of cell open-depth f[X,Y,Z] ( dimensionless ).
C     rhFacW   rhFacC center, rhFacW west, rhFacS south.
C     rhFacS   Note: This is precomputed here because it involves division.
C     saFac  - Shallow atmosphere factor (dimensionless scale factor).
C     xC     - X-coordinate of center of cell f[X,Y]. The units of xc, yc
C              depend on the grid. They are not used in differencing or
C              averaging but are just a convient quantity for I/O,
C              diagnostics etc.. As such xc is in m for cartesian 
C              coordinates but degrees for spherical polar.
C     yC     - Y-coordinate of center of cell f[X,Y].
C     zA     - Z-face are f[X,Y] (m^2).
C              Note: In a cartesian framework zA is simply dx*dy,
C                    however we use zA to allow for non-globally
C                    orthogonal coordinate frames (with appropriate
C                    metric terms).
C     zC     - Z-coordinate of center of cell f[Z]
C     zFace  - Z-coordinate of face of cell f[Z] (Pa).
      COMMON /GRID_R/
     &  dxC,dxF,dxG,dxV,dyC,dyF,dyG,dyU,dzC,dzF,
     &  H,HFacC,HFacW,HFacS,
     &  rdxC,rdxF,rdxG,rdxV,rdyC,rdyF,rdyG,rdyU,rdzC,rdzF,
     &  rH, rhFacC,rhFacW,rhFacS, 
     &  saFac,xC,yC,zA,zC,zFace,
     &  maskW,maskS
      _RS dxC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxF     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxG     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dxV     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyF     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyG     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dyU     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS dzC     (1:Nz)
      _RS dzF     (1:Nz)
      _RS H       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS HFacC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      _RS HFacW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      _RS HFacS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      _RS rdxC    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdxF    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdxG    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdxV    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdyC    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdyF    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdyG    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdyU    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rdzC    (1:Nz)
      _RS rdzF    (1:Nz)
      _RS rh      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS rhFacC  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      _RS rhFacW  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      _RS rhFacS  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      _RS saFac   (1:Nz)
      _RS xC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS zA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS zC      (1:Nz)
      _RS zFace   (1:Nz+1)
      _RS maskW   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)
      _RS maskS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nz,nSx,nSy)


C--   COMMON /GRID_I/ Integer valued grid defining variables
C     iDep  - Index of last "non-land" cell for each column f[X,Y].
      COMMON /GRID_I/
     &      iDep
      INTEGER iDep(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
