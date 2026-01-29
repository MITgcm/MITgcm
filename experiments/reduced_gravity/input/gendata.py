#!/usr/bin/env python3
"""
gendata.py

Generate initial conditions for a reduced-gravity (1.5-layer) Gaussian eddy
interacting with a circular island on a beta-plane, for MITgcm.

    - bathy_flat.bin  : 2D bathymetry (Ny, Nx), flat ocean (no island)
    - bathy.bin       : 2D bathymetry with circular island 
    - etaInit_a.bin   : 2D interface displacement (Ny, Nx), anticyclone
    - etaInit_c.bin   : 2D interface displacement (Ny, Nx), cyclone
    - uVelInit_a.bin  : 3D zonal velocity (Nr=1, Ny, Nx), anticyclone
    - vVelInit_a.bin  : 3D meridional velocity (Nr=1, Ny, Nx), anticyclone
    - uVelInit_c.bin  : 3D zonal velocity (Nr=1, Ny, Nx), cyclone
    - vVelInit_c.bin  : 3D meridional velocity (Nr=1, Ny, Nx), cyclone


References
----------
These are *conceptual* references for why a Gaussian eddy is a common idealized choice and how it connects to SSH shape and vortex structure:
- Chelton et al. (2011): composite mesoscale eddy SSH shapes are well represented by a Gaussian in the core (observational basis).
"""

from __future__ import annotations
import numpy as np
from pathlib import Path

# ===================== Grid & physical parameters =====================

Nx, Ny = 200, 200          # must match SIZE.h
dx, dy = 4000.0, 4000.0    # [m]
H_depth = 500.0            # active-layer mean depth [m]

# Coriolis and reduced gravity
f0      = 5.0e-5           # [s^-1]
beta    = 2.1e-11          # [m^-1 s^-1]
g_prime = 0.02             # [m s^-2] reduced gravity (must match gBaro or equivalent)

# Derived domain size
Lx = Nx * dx
Ly = Ny * dy

# Island geometry
island_radius = 40e3       # [m]
island_x      = 0.25 * Lx  # west-side island
island_y      = 0.5 * Ly   # centered in y

# Gaussian eddy
eddy_radius = 60e3         # [m] e-folding scale

# For a 1.5-layer model, surface SSH and interface displacement η are related by
#   SSH ≈ -(g'/g) * η
# so for SSH_max ≈ +0.25 m and g' = 0.02, |η| ≈ 0.25 * g / g' ≈ 122 m.
eddy_amp_abs = 122.0          # [m] magnitude of η
eddy_amp_a   = -eddy_amp_abs  # anticyclone: depressed interface (η < 0)
eddy_amp_c   = +eddy_amp_abs  # cyclone    : raised interface (η > 0)

# Place the eddy to the east of the island; distance between island and eddy edges
eddy_edge_gap = 100e3      # [m] edge-to-edge distance between island and eddy
eddy_x0 = island_x + island_radius + eddy_edge_gap + eddy_radius
eddy_y0 = 0.4 * Ly         # centered in y

# Output datatype
dtype_be = ">f8"           # big-endian float64

# =====================================================================

def main() -> None:
    here = Path(__file__).resolve().parent

    # -----------------------------------------------------------------
    # 1. C-grid coordinates (Cartesian, y ∈ [0, Ly])
    # -----------------------------------------------------------------
    # Tracer (C) centers
    xC_1d = (np.arange(Nx) + 0.5) * dx       # [0.5dx, Lx - 0.5dx]
    yC_1d = (np.arange(Ny) + 0.5) * dy       # [0.5dy, Ly - 0.5dy]

    # U-faces (west faces of tracer cells)
    xU_1d = np.arange(Nx) * dx               # [0, Lx - dx]
    yU_1d = yC_1d.copy()

    # V-faces (south faces of tracer cells)
    xV_1d = xC_1d.copy()
    yV_1d = np.arange(Ny) * dy               # [0, Ly - dy]

    XX_C, YY_C = np.meshgrid(xC_1d, yC_1d, indexing="xy")
    XX_U, YY_U = np.meshgrid(xU_1d, yU_1d, indexing="xy")
    XX_V, YY_V = np.meshgrid(xV_1d, yV_1d, indexing="xy")

    # -----------------------------------------------------------------
    # 2. Bathymetry:  circular island
    # -----------------------------------------------------------------
    # Flat ocean
    bathy_flat = -H_depth * np.ones((Ny, Nx))   # no island

    # Copy and overwrite island as land (0 depth)
    bathy = bathy_flat.copy()
    dist_island = np.sqrt((XX_C - island_x)**2 + (YY_C - island_y)**2)
    island_mask = dist_island <= island_radius
    bathy[island_mask] = 0.0                    # island = land (0 depth)

    # -----------------------------------------------------------------
    # 3. Interface displacement η(x, y) (reduced-gravity)
    #
    #    η(x,y) = A exp(-r^2 / L^2)  on tracer (C) grid,
    #    r^2 = (x - x0)^2 + (y - y0)^2.
    # -----------------------------------------------------------------
    r2_C = (XX_C - eddy_x0)**2 + (YY_C - eddy_y0)**2
    eta_a = eddy_amp_a * np.exp(-r2_C / eddy_radius**2)
    eta_c = eddy_amp_c * np.exp(-r2_C / eddy_radius**2)

    # Avoid creating pressure gradients exactly on land points
    eta_a[bathy == 0.0] = 0.0
    eta_c[bathy == 0.0] = 0.0

    # -----------------------------------------------------------------
    # 4. Geostrophic velocities using reduced gravity
    #
    #    f k × u = - g' ∇η
    #  ⇒  u = - (g'/f) ∂η/∂y,   v = (g'/f) ∂η/∂x
    #
    # For Gaussian η = A exp(-r^2/L^2):
    #    ∂η/∂x = η * [-2(x - x0)/L^2]
    #    ∂η/∂y = η * [-2(y - y0)/L^2]
    #
    # so
    #    u =  (2 g' A (y - y0) / (f L^2)) exp(-r^2/L^2)
    #    v = -(2 g' A (x - x0) / (f L^2)) exp(-r^2/L^2)
    #
    # On a beta-plane:
    #    f(y) = f0 + β (y - Ly/2),
    # with f ≈ f0 at the domain center.
    # -----------------------------------------------------------------
    f_U = f0 + beta * (YY_U - 0.5 * Ly)
    f_V = f0 + beta * (YY_V - 0.5 * Ly)

    # relative coordinates to eddy center on U/V grids
    x_rel_U = XX_U - eddy_x0
    y_rel_U = YY_U - eddy_y0
    r2_U    = x_rel_U**2 + y_rel_U**2

    x_rel_V = XX_V - eddy_x0
    y_rel_V = YY_V - eddy_y0
    r2_V    = x_rel_V**2 + y_rel_V**2

    def build_uv(amp: float) -> tuple[np.ndarray, np.ndarray]:
        """Return (u2d, v2d) for a given eddy interface amplitude amp."""
        with np.errstate(divide="ignore", invalid="ignore"):
            prefU = (2.0 * g_prime * amp * y_rel_U) / (f_U * eddy_radius**2)
            prefV = -(2.0 * g_prime * amp * x_rel_V) / (f_V * eddy_radius**2)

            u = prefU * np.exp(-r2_U / eddy_radius**2)
            v = prefV * np.exp(-r2_V / eddy_radius**2)

            # Clean up any NaN/Inf where f ≈ 0 etc.
            u[~np.isfinite(u)] = 0.0
            v[~np.isfinite(v)] = 0.0
        return u, v

    # build velocities for anticyclone and cyclone
    u2d_a, v2d_a = build_uv(eddy_amp_a)
    u2d_c, v2d_c = build_uv(eddy_amp_c)

    # -----------------------------------------------------------------
    # 5. Mask velocities crossing land–ocean boundaries
    #
    # U(i,j) connects tracer cells (i-1, j) and (i, j)
    # V(i,j) connects tracer cells (i, j-1) and (i, j)
    # If either side is land (bathy == 0), zero that face velocity.
    # -----------------------------------------------------------------
    ocean = bathy != 0.0

    # U-mask
    ocean_left  = np.zeros_like(ocean)
    ocean_right = np.zeros_like(ocean)
    ocean_left[:, 1:] = ocean[:, :-1]
    ocean_right[:, :] = ocean[:, :]
    u_mask = ocean_left & ocean_right

    u2d_a[~u_mask] = 0.0
    u2d_c[~u_mask] = 0.0

    # V-mask
    ocean_down = np.zeros_like(ocean)
    ocean_up   = np.zeros_like(ocean)
    ocean_down[1:, :] = ocean[:-1, :]
    ocean_up[:, :]    = ocean[:, :]
    v_mask = ocean_down & ocean_up

    v2d_a[~v_mask] = 0.0
    v2d_c[~v_mask] = 0.0

    # Add vertical dimension (Nr = 1)
    u3d_a = u2d_a[None, ...]
    v3d_a = v2d_a[None, ...]
    u3d_c = u2d_c[None, ...]
    v3d_c = v2d_c[None, ...]

    # -----------------------------------------------------------------
    # 6. Write binary files
    # -----------------------------------------------------------------
    def write_bin(name: str, arr: np.ndarray) -> None:
        path = here / name
        arr.astype(dtype_be, order="C").tofile(path)
        print(f"Wrote {name} with shape {arr.shape}")

    # bathymetry
    write_bin("bathy_flat.bin", bathy_flat)
    write_bin("bathy.bin", bathy)

    # interface displacement
    write_bin("etaInit_a.bin", eta_a)
    write_bin("etaInit_c.bin", eta_c)

    # velocities
    write_bin("uVelInit_a.bin", u3d_a)
    write_bin("vVelInit_a.bin", v3d_a)
    write_bin("uVelInit_c.bin", u3d_c)
    write_bin("vVelInit_c.bin", v3d_c)

    print("=== gendata (reduced-gravity eddy + island): DONE ===")
    print(f"Lx = {Lx/1e3:.1f} km, Ly = {Ly/1e3:.1f} km")
    print(f"dx = {dx/1e3:.2f} km, dy = {dy/1e3:.2f} km, H_depth = {H_depth} m")
    print(f"g' = {g_prime:.3f} m/s^2, f0 = {f0:.2e} s^-1, beta = {beta:.2e} m^-1 s^-1")
    print(f"Eddy |η| = {eddy_amp_abs} m, R = {eddy_radius/1e3:.1f} km")
    print(f"Eddy center = ({eddy_x0/1e3:.1f} km, {eddy_y0/1e3:.1f} km)")
    print(f"Island R = {island_radius/1e3:.1f} km at "
          f"({island_x/1e3:.1f} km, {island_y/1e3:.1f} km)")
    print("No domain walls: only island is land.")

# =====================================================================

if __name__ == "__main__":
    main()