#
# created EGavilan Pascual-Ahuir on 2023-04-07

import numpy as np

# Global constants
deg2rad = np.pi/180
def pfromz(rC, rF0=0.0, lat=None, rhoConst=1.0275e+03, eosRefP0=1.01325e+05,
           top_Pres=0.0, surf_pRef=1.01325e+05):
    """
    Computes pressure (dbar) of sea water from depth.

    Parameters
    ----------
    rC    : depth at
          : c point [m]
          : rC may have dims 1x1,
          : kx1, or kxnxm
    rF0   : depth at rF[k=0]
          : default=0
    lat   : latitude to compute
          : unesco gravity
          : lat may have dims 1x1,
          : nx1, nxm or kxnxm
          : default=None
    rhoConst  : Density of seawater
              : default=1027.5
    eosRefP0  : EOS reference pressure (Pa)
              : default=1.013250e+05
    top_Pres  : reference pressure at the top
               default=0
    surf_pRef : Surface pressure (Pa)
              : default=1.013250e+05

    Returns
    -------
    p : array_like
        pressure [dbar]

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
    z = np.asfarray(rC)

    assert np.all(z<=0), 'input_error: values cannot be positive'

    if lat is None:
        gravity = 9.81
    else:
        lat = np.asfarray(lat)
        if lat.ndim == 1 and z.ndim == 1:
            lat,z = np.meshgrid(lat,z)

        if lat.shape != z.shape:
            print('trying to broadcast shapes')
            np.broadcast(lat, z)

        # Redefine gravity (UNESCO Tech. Pap. in Mar. Sci., 1983, eq 27)
        sinsqlat = np.sin(lat*deg2rad)**2
        gravity = 9.780318*(1.0 + (5.2788e-3 + 2.36e-5*sinsqlat)*sinsqlat)

    pref = surf_pRef - eosRefP0
    dz = z - rF0
    p = (top_Pres - rhoConst*dz*gravity + pref)*1e-4

    return p

