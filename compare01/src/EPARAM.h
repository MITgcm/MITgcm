C $Id: EPARAM.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C
C     Include file defining EPARAM common block variables.
C     These variables contain global data structures used in the Eddy Parameterisation
C     code.
C     zFacUV      : Exponential factor Kh at U- and V-points
C     zFacW       : Exponential factor Kh at W-points
C     dScaleKh    : Scale depth for exponentially decaying Kh with depth     ! GMGS
C   dslpcalcfreq  :  Frequency in seconds with which to calculate isentropic slope
C   dkcalcfreq    :  Frequency in seconds with which to calculate diffusion terms
C   difmin        :  Minimum value of diffusion
C   coef          :  gamma * length scaling factor for diffusion scheme
C   difref        :  Reference diffusion for scaling shallow regions
C   difhmx        :  Maximum value of diffusion
C   dbotm         :  Depth in meters of mixing region
C   slpmax1       : max absolute value of isentropic slope allowed for the
C                   diffusion terms for levels with thickness dthin (50m) or more
C   slpmax2       : max absolute value of isentropic slope allowed for the
C                   eddy-induced velocity
C   slpmax3       : max absolute value of isentropic slope allowed for the
C                   diffusion terms for levels with thickness less than dthin
C   small         : small real value
C   dthin         : Minimum layer thickness in meters in which slope is calculated
C   MinSigStrat   : Mininum sigma stratification
C   asyncTSfac    : Mult factor for timestep of T and S
C     epsl        : Av/Ah in Redis scheme
C   MixLayrDensGrad :  Threashold in density change for setting mix layer base
      COMMON /EPARAM_F/
     & dScaleKh,dslpcalcfreq,dkcalcfreq,        
     & coef,difmin,difref,difhmx,dbotm,slpmax1,slpmax2,slpmax3,small,
     & dthin,MinSigStrat,asyncTSfac,epsl,MixLayrDensGrad,
     & zFacuv, zFacw
      real zfacuv(nz),zfacw(nz)                                  
      REAL dscalekh                                             
      REAL dslpcalcfreq
      REAL dkcalcfreq
      REAL difmin
      REAL coef
      REAL difref
      REAL difhmx
      REAL dbotm
      REAL slpmax1
      REAL slpmax2
      REAL slpmax3
      REAL small
      REAL dthin
      REAL MinSigStrat
      REAL asyncTSfac
      REAL epsl
      REAL MixLayrDensGrad

C     verticalProfKh: True if decay profile for Kh with Z is to be used.
      COMMON /EPARAM_L/
     & verticalProfKh
      LOGICAL verticalProfKh

C   trDifScheme   :  Name of mixing scheme
      COMMON /EPARAM_C/
     & trDifScheme
      CHARACTER*80  trDifScheme

C     Strings used in eddy parameterisation scheme
      CHARACTER*(*) horizDif
      CHARACTER*(*) varKhonly
      CHARACTER*(*) grst
      CHARACTER*(*) gmconKh
      CHARACTER*(*) gmvarKh
      PARAMETER    (horizDif  = 'horizontal diffusion'     )
      PARAMETER    (varKhonly = 'var kh, no slope'         )
      PARAMETER    (grst      = 'Green-Stone'              )
      PARAMETER    (gmconKh   = 'Gent-McWilliams, const Kh')
      PARAMETER    (gmvarKh   = 'Gent-McWilliams, var Kh'  )


