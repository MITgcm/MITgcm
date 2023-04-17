#
# created EGavilan Pascual-Ahuir on 2023-04-07

import numpy as np

# Global constants
gravity  = -9.810000e+00
rhoConst =  1.027500e+03
eosRefP0 =  1.013250e+05

def pfromz(rC,rF0=0.0,top_Pres=0.0,surf_pRef=1.013250e+05):
    """
    Computes pressure (dbar) of sea water from depth.

    Parameters
    ----------
    rC    : depth at
          : c point [m]
    rF0   : depth at rF[k=0] 
          : default=0
         
    top_Pres : reference pressure at the top
               default=0    
    surf_pRef : Surface pressure (Pa)
              : default=1.013250e+05    
    
    Returns
    -------
    p : array_like
        pressure [dbar]; broadcastable to shape of z

    Example
    -------
    >>> pfromz(-1000)
    1007.9775
    >>> pfromz([-100,-1000])
    100.79775, 1007.9775   
    """
    z = np.asfarray(rC)
    
    assert np.any(z<=0),('input_error: values cannot be positive')
        
    pref = surf_pRef - eosRefP0
    dz=z-rF0
    p=(top_Pres+rhoConst*dz*gravity+pref)*1e-4

    return p

