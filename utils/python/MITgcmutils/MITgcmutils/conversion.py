#
# created EGavilan Pascual-Ahuir on 2023-04-07

import numpy as np

def pfromz(rC, rF0=0.0, lat=None, rhoConst=1.0275e+3, eosRefP0=1.01325e+5,
           top_Pres=0.0, surf_pRef=1.01325e+5):
    """
    Computes pressure (dbar) of sea water from depth.

    Parameters
    ----------
    rC : float or array_like
        Depth at c point [m].
    rF0 : float or array_like
        Depth at rF[k=0], default 0.
    lat : array_like or None
        Latitude to compute unesco gravity.  If None, use gravity = 9.81.
        Default None.
    rhoConst : float
        Density of seawater, default 1027.5.
    eosRefP0 : float
        EOS reference pressure (Pa), default 1.01325e+5.
    top_Pres : float
        Reference pressure at the top, default 0.
    surf_pRef : float
        Surface pressure (Pa), default 1.01325e+5.

    Returns
    -------
    p : array_like
        Pressure [dbar].  If rC and lat are both 1-dimensional, a 2-dimensional
        array corresponding to the outer product will be returned.

    Example
    -------
    >>> pfromz(-1000)
    1007.9775
    >>> pfromz([-100,-1000])
    100.7978, 1007.9775
    >>> pfromz([-100,-1000],lat=90)
    101.0256, 1010.2562
    >>> pfromz(-1000,lat=[70,90])
    1009.6304, 1010.2562
    """
    z = np.asarray(rC, dtype=np.float64)

    assert np.all(z<=0), 'input_error: values cannot be positive'

    if lat is None:
        gravity = 9.81
    else:
        lat = np.asarray(lat, dtype=np.float64)
        if lat.ndim == 1 and z.ndim == 1:
            lat,z = np.meshgrid(lat,z)

        if lat.shape != z.shape:
            print('trying to broadcast shapes')
            np.broadcast(lat, z)

        # Redefine gravity (UNESCO Tech. Pap. in Mar. Sci., 1983, eq 27)
        sinsqlat = np.sin(np.deg2rad(lat))**2
        gravity = 9.780318*(1.0 + (5.2788e-3 + 2.36e-5*sinsqlat)*sinsqlat)

    pref = surf_pRef - eosRefP0
    dz = z - rF0
    p = (top_Pres - rhoConst*dz*gravity + pref)*1e-4

    return p
