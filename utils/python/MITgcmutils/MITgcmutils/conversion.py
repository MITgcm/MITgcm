#
# created EGavilan Pascual-Ahuir on 2023-04-07

import numpy as np

# Global constants
gravity  =  9.810000e+00
rad      =  np.pi/180
def pfromz(rC,rF0=0.0,lat=None,rhoConst=1.027500e+03,eosRefP0=1.013250e+05,
           top_Pres=0.0,surf_pRef=1.013250e+05):
    """
    Computes pressure (dbar) of sea water from depth.

    Parameters
    ----------
    rC    : depth at
          : c point [m]
    rF0   : depth at rF[k=0] 
          : default=0
    lat   : latitude to compute 
          : unesco gravity
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
    global gravity
    
    z = np.asfarray(rC)
    
    assert np.any(z<=0),('input_error: values cannot be positive')
       
    if lat != None: 
           
     lat = np.asfarray(lat)
          
     if lat.ndim > 2:
       diml = np.shape(lat)
       dimz = np.shape(z)
       assert len(diml)==len(dimz),('for more than two dimensions, lat and rC'
                                  + ' must have the same number of dimensions') 
       for k in range(0,len(diml)):
         assert diml[k]==dimz[k],('for more than two dimensions, lat and rC'
                                   + ' must have the same dimensions') 
     else:                
       # CHECK OPTIONAL SHAPES FOR z and lat  
            
       if lat.shape!=z.shape:           
        if z.size==1:                    # z is a scalar. Fill to size of lat
          z = z*np.ones(np.shape(lat))
        elif lat.size==1:                # lat is a scalar. Fill to size of z
          lat = lat*np.ones(np.shape(z))  
        elif z.ndim==1 and lat.ndim==1:
          [lat,z]=np.meshgrid(lat,z)
        else:
          lat=np.atleast_2d(lat)  
          z=np.atleast_2d(z)    
          [yz,xz] = np.shape(z) 
          [yl,xl] = np.shape(lat)   
          if yz==yl and xz==1:                    # P is row vector 
           z = np.repeat(z, repeats=yl, axis=0)   # Copy across each col
          elif xz==xl and yz==1:                    # P is column vector
           z = np.repeat(z, repeats=xl, axis=1);    # Copy across each row
          elif xz==xl and yz==yl:                   # P is a matrix size(S)
           pass                                    # shape ok 
          else:
           assert xz==xl,'check_stp: z has wrong row dimensions'
           assert yz==yl,'check_stp: z has wrong column dimensions'
        
        
     #Redefine gravity (UNESCO Tech. Pap. in Mar. Sci., 1983, eq 27)  
     
     gravity = 9.780318 * (1.0 + (5.2788e-3 + 2.36e-5 * pow(np.sin(lat*rad),2)) 
                             * pow(np.sin(lat*rad),2) )

    pref = surf_pRef - eosRefP0
    dz=z-rF0   
    p=(top_Pres-rhoConst*dz*gravity+pref)*1e-4

    return p

