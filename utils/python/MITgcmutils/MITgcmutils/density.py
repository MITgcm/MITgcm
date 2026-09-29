#
# created by mlosch on 2002-08-09
# converted to python by EGavilan Pascual-Ahuir on 2023-04-07

import numpy as np
import warnings

__doc__ = """
Density of Sea Water using linear EOS and POLY3 method.
Density of Sea Water using the Jackett and McDougall 1995 (JAOT 12) polynomial
Density of Sea Water using the UNESCO equation of state formula (IES80)
of Fofonoff and Millard (1983) [FRM83].
Density of Sea Water using the EOS-10 48-term polynomial.
"""

# coefficients nonlinear equation of state in pressure coordinates for
# 1. density of fresh water at p = 0 jmd95 and unesco
eosJMDCFw = [   999.842594,
                6.793952e-02,
              - 9.095290e-03,
                1.001685e-04,
              - 1.120083e-06,
                6.536332e-09,
            ]
# 2. density of sea water at p = 0
eosJMDCSw = [   8.244930e-01,
              - 4.089900e-03,
                7.643800e-05,
              - 8.246700e-07,
                5.387500e-09,
              - 5.724660e-03,
                1.022700e-04,
              - 1.654600e-06,
                4.831400e-04,
            ]

def _check_salinity(s):

    sneg = s<0
    if np.any(sneg):
        warnings.warn('found negative salinity values')
        # warnings.warn('found negative salinity values, reset them to nan')
        # s[sneg] = np.nan

    return s

def _check_dimensions(s,t,p=np.zeros(())):
    """
    Check compatibility of dimensions of input variables and

    Parameters
    ----------
    salt : array_like
        salinity [psu (PSS-78)]
    theta : array_like
        potential temperature [degree C (IPTS-68)]
        same shape as salt
    p  : pressure [dBar]
    """

    if s.shape != t.shape or s.shape != p.shape:
        print('trying to broadcast shapes')
        np.broadcast(s,t,p)

    return

def linear(salt,theta,
           sref=30,tref=20,sbeta=7.4e-04,talpha=2.0e-04,rhonil=9.998e+02):
    """
    Computes in-situ density of water

    Density of water using the linear EOS of McDougall (1987).

    Parameters
    ----------
    salt : array_like
        salinity [psu (PSS-78)]
    theta : array_like
        potential temperature [degree C (IPTS-68)]
        same shape as salt
    sref  : reference  salinity
            default 30 [psu (PSS-78)]
    tref  : reference potential temperature
            default 20 [degree C (IPTS-68)]
    sbeta  : haline expansion coefficient
             default 7.4e-04 [1/C]
    talpha    : therma expansion coefficient
              default 2.0e-04 [(g/Kg)-1]
    rhonil    : density of water
              default 999.8 [(g/Kg)-1];

    Returns
    -------
    dens : array
        density [kg/m^3]

    Example
    -------
    >>> dens.linear(35.5, 3.)
    1007.268506

    Notes
    -----
    - Source code written by Martin Losch 2002-08-09
    - Converted to python by Gavilan on 2024-07-18
    """

    # make sure arguments are floating point
    s = np.asarray(salt, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)

    _check_dimensions(s,t)

    s = _check_salinity(s)

    rho = rhonil*(sbeta *(s-sref)-talpha*(t-tref))+rhonil

    return rho

def poly3(poly3,salt,theta):
    """
    Calculates in-situ density as approximated by the POLY3 method
    based on the Knudsen formula (see Bryan and Cox 1972).

    Parameters
    ----------
    poly3 : coefficients read from file
            'POLY3.COEFFS' using INI_POLY3
    salt : array_like
           salinity [psu (PSS-78)]
    theta : array_like
            potential temperature [degree C (IPTS-68)];
            same shape as salt

    Returns
    -------
    dens : array
        density [kg/m^3]

    Example
    -------
    >>> p=ini_poly3()
    >>> T=rdmds('T',100)
    >>> S=rdmds('S',100)
    >>> D=poly3(p,salt,theta)
    >>> or to work within a single model level
    >>> D=poly3(P[3,:],S[3,:,:],T[3,:,:])


    Notes
    -----
    - Source code written by Martin Losch 2002-08-09
    - Converted to python by Gavilan on 2024-07-18
    """
    # make sure arguments are floating point
    s = np.asarray(salt, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    coeffs=poly3[:,3:]


    _check_dimensions(s,t)

    s = _check_salinity(s)

    Nr=poly3.shape[0]

    t=np.reshape(t,[Nr,np.prod(np.shape(t))//Nr])
    s=np.reshape(s,[Nr,np.prod(np.shape(s))//Nr])
    rho=np.zeros([Nr,np.prod(np.shape(s))//Nr])
    for k in range(0,Nr):
        tRef=poly3[k,0]
        sRef=poly3[k,1]
        dRef=poly3[k,2]+1000
        tp=t[k,:]-tRef;
        sp=s[k,:]-sRef;

        tp2=tp*tp
        sp2=sp*sp

        deltaSig = (   coeffs[k,0]*tp
                     + coeffs[k,1]*sp
                     + coeffs[k,2]*tp2
                     + coeffs[k,3]*tp*sp
                     + coeffs[k,4]*sp2
                     + coeffs[k,5]*tp2*tp
                     + coeffs[k,6]*tp2*sp
                     + coeffs[k,7]*tp*sp2
                     + coeffs[k,8]*sp2*sp
                    )

        rho[k,:]=deltaSig+dRef

    return np.reshape(rho,s.shape)


def ini_poly3(fpath='POLY3.COEFFS'):
    """Reads the file fpath (default 'POLY3.COEFFS') and returns
    coefficients in poly
    """

    with open(fpath,'r') as fid:
        poly_data=fid.readlines()

    Nr=int(poly_data.pop(0))    # Number of vertical levels

    poly=np.zeros([Nr,12])

    poly_split = [i.split() for i in poly_data[:Nr]]
    poly[:,:3] = np.asarray(poly_split)

    poly_split = [i.split() for i in poly_data[Nr:]]
    poly[:,3:] = np.asarray(poly_split).reshape((Nr,9))

    return poly

def jmd95(salt,theta,p):
    """
    Computes in-situ density of sea water

    Density of Sea Water using Jackett and McDougall 1995 (JAOT 12)
    polynomial (modified UNESCO polynomial).

    Parameters
    ----------
    salt : array_like
           salinity [psu (PSS-78)]
    theta : array_like
        potential temperature [degree C (IPTS-68)];
        same shape as s
    p : array_like
        sea pressure [dbar]. p may have dims 1x1,
        mx1, 1xn or mxn for s(mxn)

    Returns
    -------
    dens : array
        density [kg/m^3]

    Example
    -------
    >>> dens.jmd95(35.5, 3., 3000.)
    1041.83267

    Notes
    -----

    - Jackett and McDougall, 1995, JAOT 12(4), pp. 381-388
    - Source code written by Martin Losch 2002-08-09
    - Converted to python by jahn on 2010-04-29
    """

    # make sure arguments are floating point
    s = np.asarray(salt, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p, dtype=np.float64)

    _check_dimensions(s,t,p)

    s = _check_salinity(s)

    # convert pressure to bar
    p = .1*p
    t2 = t*t
    t3 = t2*t
    t4 = t3*t

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


def bulkmodjmd95(salt,theta,p):
    """ Compute bulk modulus
    """
    # make sure arguments are floating point
    s = np.asarray(salt, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p, dtype=np.float64)

    # coefficients in pressure coordinates for
    # 3. secant bulk modulus K of fresh water at p = 0
    eosJMDCKFw = [   1.965933e+04,
                     1.444304e+02,
                   - 1.706103e+00,
                     9.648704e-03,
                   - 4.190253e-05
                 ]
    # 4. secant bulk modulus K of sea water at p = 0
    eosJMDCKSw = [  5.284855e+01,
                  - 3.101089e-01,
                    6.283263e-03,
                  - 5.084188e-05,
                    3.886640e-01,
                    9.085835e-03,
                  - 4.619924e-04
                 ]
    # 5. secant bulk modulus K of sea water at p
    eosJMDCKP = [    3.186519e+00,
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
                    6.207323e-10
                ]

    _check_dimensions(s,t,p)

    t2 = t*t
    t3 = t2*t
    t4 = t3*t
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

def unesco(salt,theta,p):
    """
    Computes in-situ density of sea water

    Density of Sea Water using Fofonoff and Millard (1983)
    polynomial.

    Parameters
    ----------
    salt : array_like
           salinity [psu (PSS-78)]
    theta : array_like
        potential temperature [degree C (IPTS-68)];
        same shape as s
    p : array_like
        sea pressure [dbar]. p may have dims 1x1,
        mx1, 1xn or mxn for s(mxn)

    Returns
    -------
    dens : array
        density [kg/m^3]

    Example
    -------
    >>> dens.unesco(35.5, 3., 3000.)
    1041.87663

    Notes
    -----
    - Source code written by Martin Losch 2002-08-09
    - Converted to python by Gavilan on 2024-07-18
    """

    # make sure arguments are floating point
    s = np.asarray(salt, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p, dtype=np.float64)

    _check_dimensions(s,t,p)

    s = _check_salinity(s)

    # convert pressure to bar
    p = .1*p
    t2 = t*t
    t3 = t2*t
    t4 = t3*t
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

    rho = rho / (1. - p/bulkmodunesco(s,t,p))

    return rho


def bulkmodunesco(salt,theta,p):
    """ Compute bulk modulus
    """
    # make sure arguments are floating point
    s = np.asarray(salt, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p, dtype=np.float64)

    # 3. secant bulk modulus K of fresh water at p = 0
    eosJMDCKFw = [   1.965221e+04,
                     1.484206e+02,
                   - 2.327105e+00,
                     1.360477e-02,
                   - 5.155288e-05
         ]
    # 4. secant bulk modulus K of sea water at p = 0
    eosJMDCKSw = [   5.467460e+01,
                   - 0.603459e+00,
                     1.099870e-02,
                   - 6.167000e-05,
                     7.944000e-02,
                     1.648300e-02,
                   - 5.300900e-04
             ]
    # 5. secant bulk modulus K of sea water at p
    eosJMDCKP = [   3.239908e+00,
                    1.437130e-03,
                    1.160920e-04,
                  - 5.779050e-07,
                    2.283800e-03,
                  - 1.098100e-05,
                  - 1.607800e-06,
                    1.910750e-04,
                    8.509350e-05,
                 -  6.122930e-06,
                    5.278700e-08,
                 -  9.934800e-07,
                    2.081600e-08,
                    9.169700e-10
                ]

    _check_dimensions(s,t,p)

    t2 = t*t
    t3 = t2*t
    t4 = t3*t

    sneg = np.where(s < 0 )

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
              + s3o2*(     eosJMDCKSw[4]
                       + eosJMDCKSw[5]*t
                       + eosJMDCKSw[6]*t2
                       )
               )
    # secant bulk modulus of sea water at pressure p
    bulkmod = ( bulkmod
              + p*(      eosJMDCKP[0]
                    + eosJMDCKP[1]*t
                    + eosJMDCKP[2]*t2
                    + eosJMDCKP[3]*t3
                  )
              + p*s*(    eosJMDCKP[4]
                      + eosJMDCKP[5]*t
                      + eosJMDCKP[6]*t2
                    )
              + p*s3o2*eosJMDCKP[7]
              + p2*(   eosJMDCKP[8]
                     + eosJMDCKP[9]*t
                     + eosJMDCKP[10]*t2
                   )
              + p2*s*(    eosJMDCKP[11]
                      + eosJMDCKP[12]*t
                      + eosJMDCKP[13]*t2
                     )
               )

    return bulkmod

def mdjwf(salt,theta,p,epsln=0):
    """
    Computes in-situ density of sea water

    Density of Sea Water using the McDougall et al. 2003 (JAOT 20)
    polynomial.

    Parameters
    ----------
    salt : array_like
        salinity [psu (PSS-78)]
    theta : array_like
        potential temperature [degree C (IPTS-68)];
        same shape as salt
    p : array_like
        sea pressure [dbar]. p may have dims 1x1,
        mx1, 1xn or mxn for salt(mxn)

    Returns
    -------
    dens : array
        density [kg/m^3]

    Example
    -------
    >>> dens.mdjwf(35.5, 3., 3000.)
    1041.83305

    Notes
    -----
    - McDougall et al., 2003, JAOT 20(5), pp. 730-741
    - Converted to python by Gavilan on 2024-07-18
    """

    # make sure arguments are floating point
    s = np.asarray(salt, dtype=np.float64)
    t = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p, dtype=np.float64)

    _check_dimensions(s,t,p)

    s = _check_salinity(s)

    eosMDJWFnum = [   7.35212840e+00,
                    - 5.45928211e-02,
                      3.98476704e-04,
                      2.96938239e+00,
                    - 7.23268813e-03,
                      2.12382341e-03,
                      1.04004591e-02,
                      1.03970529e-07,
                      5.18761880e-06,
                    - 3.24041825e-08,
                    - 1.23869360e-11,
                      9.99843699e+02
                  ]

    eosMDJWFden = [   7.28606739e-03,
                    - 4.60835542e-05,
                      3.68390573e-07,
                      1.80809186e-10,
                      2.14691708e-03,
                    - 9.27062484e-06,
                    - 1.78343643e-10,
                      4.76534122e-06,
                      1.63410736e-09,
                      5.30848875e-06,
                    - 3.03175128e-16,
                    - 1.27934137e-17,
                      1.00000000e+00
                  ]

    t1    = t
    t2    = t1*t1
    s1    = s
    p1    = p

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

    denom = 1.0/(epsln+den)
    rho = num*denom

    return rho

def teos10(salt,theta,p,epsln=0):
    """
    Computes in-situ density of sea water

    Density of Sea Water using TEOS-10.

    Parameters
    ----------
    salt  : array_like
            absolute salinity [g/kg]
    theta : array_like
            conservative temperature [degree C (IPTS-68)];
            same shape as s
    p : array_like
        sea pressure [dbar]. p may have dims 1x1,
        mx1, 1xn or mxn for s(mxn)

    Returns
    -------
    dens : array
        density [kg/m^3]

    Example
    -------
    >>> dens.teos10(35.5, 3., 3000.)
    1041.70578

    Notes
    -----
    - Converted to python by Gavilan on 2024-07-18
    """

    # make sure arguments are floating point
    sa = np.asarray(salt, dtype=np.float64)
    ct = np.asarray(theta, dtype=np.float64)
    p = np.asarray(p, dtype=np.float64)

    _check_dimensions(sa,ct,p)

    sa = _check_salinity(sa)

    teos = [   9.998420897506056e+02,
               2.839940833161907e00,
             - 3.147759265588511e-02,
               1.181805545074306e-03,
             - 6.698001071123802e00,
             - 2.986498947203215e-02,
               2.327859407479162e-04,
             - 3.988822378968490e-02,
               5.095422573880500e-04,
             - 1.426984671633621e-05,
               1.645039373682922e-07,
             - 2.233269627352527e-02,
             - 3.436090079851880e-04,
               3.726050720345733e-06,
             - 1.806789763745328e-04,
               6.876837219536232e-07,
             - 3.087032500374211e-07,
             - 1.988366587925593e-08,
             - 1.061519070296458e-11,
               1.550932729220080e-10,
               1.000000000000000e00,
               2.775927747785646e-03,
             - 2.349607444135925e-05,
               1.119513357486743e-06,
               6.743689325042773e-10,
             - 7.521448093615448e-03,
             - 2.764306979894411e-05,
               1.262937315098546e-07,
               9.527875081696435e-10,
             - 1.811147201949891e-11,
             - 3.303308871386421e-05,
               3.801564588876298e-07,
             - 7.672876869259043e-09,
             - 4.634182341116144e-11,
               2.681097235569143e-12,
               5.419326551148740e-06,
             - 2.742185394906099e-05,
             - 3.212746477974189e-07,
               3.191413910561627e-09,
             - 1.931012931541776e-12,
             - 1.105097577149576e-07,
               6.211426728363857e-10,
             - 1.119011592875110e-10,
             - 1.941660213148725e-11,
             - 1.864826425365600e-14,
               1.119522344879478e-14,
             - 1.200507748551599e-15,
               6.057902487546866e-17,
           ]

    sqrtsa = np.sqrt(sa)

    rhoNum = (teos[0]
              + ct*(teos[1] + ct*(teos[2] + teos[3]*ct))
              + sa*(teos[4] + ct*(teos[5] + teos[6]*ct)
              + sqrtsa*(teos[7] + ct*(teos[8]
              + ct*(teos[9] + teos[10]*ct))))
              + p*(teos[11] + ct*(teos[12] + teos[13]*ct)
              + sa*(teos[14] + teos[15]*ct)
              + p*(teos[16] + ct*(teos[17] + teos[18]*ct) + teos[19]*sa)))

    den = (teos[20]
           + ct*(teos[21] + ct*(teos[22] + ct*(teos[23] + teos[24]*ct)))
           + sa*(teos[25] + ct*(teos[26] + ct*(teos[27]
           + ct*(teos[28] + teos[29]*ct)))
           + teos[35]*sa
           + sqrtsa*(teos[30] + ct*(teos[31] + ct*(teos[32]
           + ct*(teos[33] + teos[34]*ct)))))
           + p*(teos[36] + ct*(teos[37] + ct*(teos[38] + teos[39]*ct))
           + sa*(teos[40] + teos[41]*ct)
           + p*(teos[42] + ct*(teos[43] + teos[44]*ct + teos[45]*sa)
           + p*(teos[46] + teos[47]*ct))))

    rhoden = 1.0/(epsln+den)

    rho = rhoNum*rhoden

    return rho
