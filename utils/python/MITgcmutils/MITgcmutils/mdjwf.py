# $Header: /u/gcmpack/MITgcm/utils/python/MITgcmutils/MITgcmutils/mdjwf.py,v 1.2 2018/01/11 14:34:22 mlosch Exp $
# $Name:  $
#
# converted from matlab version to python in Jan 2018

import sys
import numpy as np

__doc__ = """
Density of Sea Water using McDougall et al. 2003 (JAOT 20) polynomial

Functions:

dens :: computes in-situ density from salinity, potential temperature
        and pressure
"""

# coefficients nonlinear equation of state in pressure coordinates for
eosMDJWFnum =  [ 7.35212840e+00,
                -5.45928211e-02,
                 3.98476704e-04,
                 2.96938239e+00,
                -7.23268813e-03,
                 2.12382341e-03,
                 1.04004591e-02,
                 1.03970529e-07,
                 5.18761880e-06,
                -3.24041825e-08,
                -1.23869360e-11,
                 9.99843699e+02 ]
  
eosMDJWFden =  [ 7.28606739e-03,
                -4.60835542e-05,
                 3.68390573e-07,
                 1.80809186e-10,
                 2.14691708e-03,
                -9.27062484e-06,
                -1.78343643e-10,
                 4.76534122e-06,
                 1.63410736e-09,
                 5.30848875e-06,
                -3.03175128e-16,
                -1.27934137e-17,
                 1.00000000e+00 ]


def densmdjwf(s,theta,p):
    """
    densmdjwf    Density of sea water
   =========================================================================

    USAGE:  dens = densmdjwf(s,theta,p)

    DESCRIPTION:
       Density of Sea Water using McDougall et al. 2003 (JAOT 20)
       polynomial (Gibbs Potential).

    INPUT:  (all must have same dimensions)
      S     = salinity    [psu      (PSS-78)]
      Theta = potential temperature [degree C (IPTS-68)]
      P     = pressure    [dbar]
          (P may have dims 1x1, mx1, 1xn or mxn for S(mxn) )

    OUTPUT:
      dens = density  [kg/m^3]

    AUTHOR:  Martin Losch 2002-08-09  (Martin.Losch@awi.de)

    check value
    S     = 35 PSU
    Theta = 25 degC
    P     = 2000 dbar
    rho   = 1031.654229 kg/m^3

    McDougall et al., 2003, JAOT 20(5), pp. 730-741
    """
    
    # make sure arguments are floating point
    s = np.asfarray(s)
    t = np.asfarray(theta)
    p = np.asfarray(p)

    p1 = np.copy(p);
    
    t1 = np.copy(t);
    t2 = t*t;
    
    s1 = np.copy(s);

    if np.any(s1<0):
        sys.stderr.write('negative salinity values! setting to nan\n')
        # # the sqrt will take care of this
        # if s.ndim > 0:
        #     s[s<0] = np.nan
        # else:
        #     s = np.nan
            
    sp5 = np.sqrt(s1)
    p1t1=p1*t1

    num = ( eosMDJWFnum[11] 
            + t1*(eosMDJWFnum[0]
                  + t1*(eosMDJWFnum[1] + eosMDJWFnum[2]*t1) )
	    + s1*(eosMDJWFnum[3]
                  + eosMDJWFnum[4]*t1  + eosMDJWFnum[5]*s1)
	    + p1*(eosMDJWFnum[6] + eosMDJWFnum[7]*t2
                  + eosMDJWFnum[8]*s1
	          + p1*(eosMDJWFnum[9] + eosMDJWFnum[10]*t2) )
    )
    den = ( eosMDJWFden[12]
            + t1*(eosMDJWFden[0]
	          + t1*(eosMDJWFden[1]
	                + t1*(eosMDJWFden[2] + t1*eosMDJWFden[3] ) ) )
            + s1*(eosMDJWFden[4]
	          + t1*(eosMDJWFden[5]
	                + eosMDJWFden[6]*t2)
	          + sp5*(eosMDJWFden[7] + eosMDJWFden[8]*t2) )
	    + p1*(eosMDJWFden[9]
	          + p1t1*(eosMDJWFden[10]*t2 + eosMDJWFden[11]*p1) )
    )
  
    epsln = 0
    denom = 1.0/(epsln+den)
    rho = num*denom;

    return rho

# aliases
dens = densmdjwf

