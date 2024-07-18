#
# created by mlosch on 2002-08-09
# converted to python by jahn on 2010-04-29

import sys
import numpy as np

__doc__ = """
Density of Sea Water using the Jackett and McDougall 1995 (JAOT 12) polynomial
"""

# coefficients nonlinear equation of state in pressure coordinates for
# 1. density of fresh water at p = 0
eosJMDCFw = [ 999.842594,
              6.793952e-02,
           -  9.095290e-03,
              1.001685e-04,
           -  1.120083e-06,
              6.536332e-09,
            ]
# 2. density of sea water at p = 0
eosJMDCSw = [     8.244930e-01,
               -  4.089900e-03,
                  7.643800e-05,
               -  8.246700e-07,
                  5.387500e-09,
               -  5.724660e-03,
                  1.022700e-04,
               -  1.654600e-06,
                  4.831400e-04,
            ]
# coefficients in pressure coordinates for
# 3. secant bulk modulus K of fresh water at p = 0
eosJMDCKFw = [    1.965933e+04,
                  1.444304e+02,
                - 1.706103e+00,
                  9.648704e-03,
                - 4.190253e-05,
             ]
# 4. secant bulk modulus K of sea water at p = 0
eosJMDCKSw = [    5.284855e+01,
                - 3.101089e-01,
                  6.283263e-03,
                - 5.084188e-05,
                  3.886640e-01,
                  9.085835e-03,
                - 4.619924e-04,
             ]
# 5. secant bulk modulus K of sea water at p
eosJMDCKP = [     3.186519e+00,
                  2.212276e-02,
                - 2.984642e-04,
                  1.956415e-06,
                  6.704388e-03,
                - 1.847318e-04,
                  2.059331e-07,
                  1.480266e-04,
                  2.102898e-04,
                - 1.202016e-05,
                  1.394680e-07,
                - 2.040237e-06,
                  6.128773e-08,
                  6.207323e-10,
            ]

def densjmd95(s,theta,p):
    """
    Computes in-situ density of sea water

    Density of Sea Water using Jackett and McDougall 1995 (JAOT 12)
    polynomial (modified UNESCO polynomial).

    Parameters
    ----------
    s : array_like
        salinity [psu (PSS-78)]
    theta : array_like
        potential temperature [degree C (IPTS-68)];
        same shape as s
    p : array_like
        pressure [dbar]; broadcastable to shape of s

    Returns
    -------
    dens : array
        density [kg/m^3]

    Example
    -------
    >>> densjmd95(35.5, 3., 3000.)
    1041.83267

    Notes
    -----
    AUTHOR:  Martin Losch 2002-08-09  (mlosch@mit.edu)

    Jackett and McDougall, 1995, JAOT 12(4), pp. 381-388
    """

    # make sure arguments are floating point
    s = np.asarray(s, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p, dtype=np.float64)

    # convert pressure to bar
    p = .1*p

    t2 = t*t
    t3 = t2*t
    t4 = t3*t

    if np.any(s<0):
        sys.stderr.write('negative salinity values! setting to nan\n')
#       the sqrt will take care of this
#        if s.ndim > 0:
#            s[s<0] = np.nan
#        else:
#            s = np.nan
            
    s3o2 = s*np.sqrt(s)

    # density of freshwater at the surface
    rho = ( eosJMDCFw[0]
          + eosJMDCFw[1]*t
          + eosJMDCFw[2]*t2
          + eosJMDCFw[3]*t3
          + eosJMDCFw[4]*t4
          + eosJMDCFw[5]*t4*t
          )
    # density of sea water at the surface
    rho = ( rho
           + s*(
                 eosJMDCSw[0]
               + eosJMDCSw[1]*t
               + eosJMDCSw[2]*t2
               + eosJMDCSw[3]*t3
               + eosJMDCSw[4]*t4
               )
           + s3o2*(
                 eosJMDCSw[5]
               + eosJMDCSw[6]*t
               + eosJMDCSw[7]*t2
               )
           + eosJMDCSw[8]*s*s
          )

    rho = rho / (1. - p/bulkmodjmd95(s,t,p))

    return rho


def bulkmodjmd95(s,theta,p):
    """ Compute bulk modulus
    """
    # make sure arguments are floating point
    s = np.asarray(s, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p ,dtype=np.float64)

    t2 = t*t
    t3 = t2*t
    t4 = t3*t

#    if np.any(s<0):
#        sys.stderr.write('negative salinity values! setting to nan\n')
#       the sqrt will take care of this
#        if s.ndim > 0:
#            s[s<0] = np.nan
#        else:
#            s = np.nan

    s3o2 = s*np.sqrt(s)

    #p = pressure(i,j,k,bi,bj)*SItoBar
    p2 = p*p
    # secant bulk modulus of fresh water at the surface
    bulkmod = ( eosJMDCKFw[0]
              + eosJMDCKFw[1]*t
              + eosJMDCKFw[2]*t2
              + eosJMDCKFw[3]*t3
              + eosJMDCKFw[4]*t4
              )
    # secant bulk modulus of sea water at the surface
    bulkmod = ( bulkmod
              + s*(      eosJMDCKSw[0]
                       + eosJMDCKSw[1]*t
                       + eosJMDCKSw[2]*t2
                       + eosJMDCKSw[3]*t3
                       )
              + s3o2*(   eosJMDCKSw[4]
                       + eosJMDCKSw[5]*t
                       + eosJMDCKSw[6]*t2
                       )
               )
    # secant bulk modulus of sea water at pressure p
    bulkmod = ( bulkmod
              + p*(   eosJMDCKP[0]
                    + eosJMDCKP[1]*t
                    + eosJMDCKP[2]*t2
                    + eosJMDCKP[3]*t3
                  )
              + p*s*(   eosJMDCKP[4]
                      + eosJMDCKP[5]*t
                      + eosJMDCKP[6]*t2
                    )
              + p*s3o2*eosJMDCKP[7]
              + p2*(   eosJMDCKP[8]
                     + eosJMDCKP[9]*t
                     + eosJMDCKP[10]*t2
                   )
              + p2*s*(  eosJMDCKP[11]
                      + eosJMDCKP[12]*t
                      + eosJMDCKP[13]*t2
                     )
               )

    return bulkmod


# aliases
dens = densjmd95

