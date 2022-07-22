Package `BBL` is a simple bottom boundary layer scheme.  The initial
motivation is to allow dense water that forms on the continental shelf around
Antarctica (High Salinity Shelf Water) in the CS510 configuration to sink to
the bottom of the model domain and to become a source of Antarctic Bottom
Water.  The bbl package aims to address the following two limitations of
package down_slope:

(i) In `pkg/down_slope`, dense water cannot flow down-slope unless there is a
step, i.e., a change of vertical level in the bathymetry.  In pkg/bbl, dense
water can flow even on a slight incline or flat bottom.

(ii) In `pkg/down_slope`, dense water is diluted as it flows into grid cells
whose thickness depends on model configuration, typically much thicker than a
bottom boundary layer.  In pkg/bbl, dense water is contained in a thin
sub-layer and hence able to preserve its tracer properties.

Specifically, the bottommost wet grid cell of thickness

    thk = hFacC(kBot) * drF(kBot),

with properties `tracer`, and density `rho` is divided in two sub-levels:

1. A bottom boundary layer with T/S tracer properties `bbl_tracer`,
density `bbl_rho`, and thickness `bbl_eta`.

2. A residual thickness `resThk = thk - bbl_eta` with tracer properties

    resTracer = ( tracer * thk - bbl_tracer * bbl_eta ) / resThk

such that the volume integral of `bbl_tracer` and `resTracer` is consistent with
the Tracer properties of bottommost wet grid cell.

The bottom boundary layer properties `bbl_tracer` evolve as follows:

I. Vertical detrainment between BBL and residual volume of bottommost wet cell:

(i) If `bbl_rho > rho` , the T/S properties of the BBL are detrained into
the residual volume with a vertical velocity `bbl_wvel` such that:

    bbl_eta(T+deltaT) = max( 0, bbl_eta(T) - bbl_wvel * deltaT )

(ii) If `rho >= bbl_rho` or `bbl_eta(T) = 0` then set `bbl_tracer = Tracer`

Above operations do not change tracer properties of bottommost wet cell.

II. Horizontal exchange between adjacent bottom boundary layer cells when
heavy BBL water is above or at the same level as lighter BBL water.  The
strength of horizontal exchange is controlled by velocity parameter `bbl_hvel`.

(i) Replenish thickness of donor cell, `bbl_eta(d)`, if needed:

    IF bbl_tracer(d) .EQ. Tracer(d) .AND. bbl_eta(d) .LT. bbl_initEta THEN
       bbl_eta(d) = min ( bbl_initEta, Thk(d) )
    ENDIF

(ii) Heavy water flowing out of a cell lowers `bbl_eta(d)` in donor cell and
increases `bbl_eta(r)` in receiving cell.  The volume transport `dVol` out
of the donor cell is:

    dVol = min( bbl_eta(d) * rA(d) / 2,
                (thk(r) - bbl_eta(r)) * rA(r) / 2,
                d(x/y)G * bbl_eta(d) * bbl_hvel * deltaT )

(iii) Compute and accumulate tracer tendencies for donor and receiving cell.
These will be applied to the model's tracer quantities by `bbl_tendency_apply`.

    bbl_tend = dVol * (bbl_tracer(d) - resTracer(r)) / deltaT

    bbl_tendTracer(d) = bbl_tendTracer(d) - bbl_tend / rA(d) / thk(d)

    bbl_tendTracer(r) = bbl_tendTracer(r) + bbl_tend / rA(r) / thk(r)

(iv) Adjust bbl thickness and tracer properties.

    bbl_eta(d,T+deltaT) = bbl_eta(d,T) - dVol / rA(d)
    IF bbl_eta(d) .LT. 0.0001 THEN
       bbl_eta(d) = 0.0
       bbl_tracer(d) = tracer(d)
    ENDIF

    bbl_eta(r,T+deltaT) = bbl_eta(r,T) + dVol / rA(r)

    bbl_tracer(r,T+deltaT) = ( dVol * bbl_tracer(d,T) +
                               bbl_eta(r,T) * rA(r) * bbl_tracer(r,T) ) /
                             ( bbl_eta(r,T+deltaT) * rA(r) )

Note that horizontal exchange is carried out in following order:

1. Meridional bbl exchange at northern edge.
2. Meridional bbl exchange inside tile.
3. Zonal bbl exchange at eastern edge.
4. Zonal bbl exchange inside tile.

The northern and eastern edge computations need to be done before
computations inside the tile in order to avoid edge artifacts.
Note that different tiling will produce slightly different results
as order of computations does matters.

Some key limitations of package as it stands:

- Detrainment and horizontal transport velocities (`bbl_wvel` and `bbl_hvvel`) are
  prescribed rather than computed.
- There is no provision for entrainment (negative `bbl_wvel`).
- bbl depth (`bbl_eta`) cannot be thicker than thickness of bottommost wet grid cell.
