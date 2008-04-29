C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini_ecco.F,v 1.17 2008/04/29 21:43:22 gforget Exp $
C $Name:  $

#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_map_ini_ecco( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_map_ini_ecco
c     ==================================================================
c
c     o Add the temperature and salinity etc. parts of the control vector to
c       the model state and params and update the tile edges. 
c     o The control vector is defined in the header file "ctrl.h".
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 23-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     o rewritten: Gael Forget gforget@mit.edu 09-Feb-2008
c              - Introduce a generic routine (ctrl_map_ini_gen.F) 
c                handling individual ctrl vector terms
c
c     ==================================================================
c     SUBROUTINE ctrl_map_ini_ecco
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_ECCO
#include "ecco_cost.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_ECCO
c     == local variables ==

      _RL     fac
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer il
      _RL boundsVec(5)
      integer paramSmooth

      logical doglobalread
      logical ladinit

      character*( 80) fnamegeneric
      character*(MAX_LEN_FNAM) suff

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifdef ALLOW_AUTODIFF_TAMC
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = 0
          ikey = (act3 + 1) + act4*max3
#endif /* ALLOW_AUTODIFF_TAMC */

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
c--   only do interior, and exchange at the end
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      doglobalread = .false.
      ladinit      = .false.

      fac = 1. _d 0


      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
              tmpfld2d(i,j,bi,bj) = 1.
            enddo
          enddo
        enddo
      enddo
      _EXCH_XY_R8( tmpfld2d, mythid)

      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i = imin,imax
                tmpfld3d(i,j,k,bi,bj)=1.
              enddo
            enddo
          enddo
        enddo
      enddo
      _EXCH_XYZ_R8( tmpfld3d, mythid)



#ifdef ALLOW_THETA0_CONTROL
c--   Temperature field.
      boundsVec(1)=-2.
      boundsVec(2)=-1.9
      boundsVec(3)=39.
      boundsVec(4)=40.
      boundsVec(5)=5.
      paramSmooth=1
      call ctrl_map_ini_gen3D(xx_theta_file, 'wthetaLev', 
     & xx_theta_dummy, boundsVec, theta, maskc, paramSmooth,
     &  mythid ) 
      _EXCH_XYZ_R8( theta, mythid)
#endif


#ifdef ALLOW_SALT0_CONTROL
c--   Salinity field.
      boundsVec(1)=29.
      boundsVec(2)=29.5
      boundsVec(3)=40.5
      boundsVec(4)=41.
      boundsVec(5)=5.
      paramSmooth=1      
      call ctrl_map_ini_gen3D(xx_salt_file, 'wsaltLev', 
     & xx_salt_dummy, boundsVec, salt, maskc, paramSmooth, 
     & mythid )
      _EXCH_XYZ_R8( salt, mythid)
#endif


#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.      
      paramSmooth=1      
c-- initial zonal velocity
      call ctrl_map_ini_gen3D(xx_uvel_file, 'wuvel', 
     & xx_uvel_dummy, boundsVec, uvel, maskw, paramSmooth, 
     & mythid )
c-- initial merid. velocity
      call ctrl_map_ini_gen3D(xx_vvel_file, 'wvvel', 
     & xx_vvel_dummy, boundsVec, vvel, masks, paramSmooth, 
     & mythid )
c-- eschange UV:
       CALL EXCH_UV_XYZ_RS(uvel,vvel,.TRUE.,myThid)
#endif


#ifdef ALLOW_ETAN0_CONTROL
c--   initial Eta.
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.
      paramSmooth=1      
      call ctrl_map_ini_gen2D(xx_etan_file, 'wetan', 
     & xx_etan_dummy, boundsVec, etan, maskc, paramSmooth, 
     & mythid )
      _EXCH_XY_R8( etan, mythid)
#endif




#ifdef ALLOW_RELAXSST_CONTROL
c--   SST relaxation coefficient.
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.
      paramSmooth=1      
      call ctrl_map_ini_gen2D(xx_relaxsst_file, 'wrelaxsst', 
     & xx_relaxsst_dummy, boundsVec, 
     & lambdaThetaClimRelax, maskc, paramSmooth, mythid )
      _EXCH_XY_R8( lambdaSaltClimRelax, mythid)
#endif


#ifdef ALLOW_RELAXSSS_CONTROL
c--   SSS relaxation coefficient.
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.
      paramSmooth=1      
      call ctrl_map_ini_gen2D(xx_relaxsss_file, 'wrelaxsss', 
     & xx_relaxsss_dummy, boundsVec, 
     & lambdaSaltClimRelax, maskc, paramSmooth, mythid )
      _EXCH_XY_R8( lambdaSaltClimRelax, mythid)
#endif




#ifdef ALLOW_SEAICE
      call seaice_ctrl_map_ini( mythid )
#endif


#ifdef ALLOW_BOTTOMDRAG_CONTROL
      boundsVec(1)=0.
      boundsVec(2)=1. _d -4
      boundsVec(3)=0.9 _d -2
      boundsVec(4)=1. _d -2
      boundsVec(5)=0.
      paramSmooth=1      
      call ctrl_map_ini_gen2D(xx_bottomdrag_file, 'wbottomdrag', 
     & xx_bottomdrag_dummy, boundsVec, bottomdragfld, maskc, 
     & paramSmooth, mythid )
      _EXCH_XY_R8( bottomdragfld, mythid)
#endif


#if (defined (ALLOW_EDTAUX_CONTROL) && defined (ALLOW_EDTAUY_CONTROL))
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.
      paramSmooth=1      
c-- zonal eddy stress : edtaux
      call ctrl_map_ini_gen3D(xx_edtaux_file, 'wedtauxFld', 
     & xx_edtaux_dummy, boundsVec, eddytaux, maskw, paramSmooth,
     & mythid )
c-- meridional eddy stress : edtauy
      call ctrl_map_ini_gen3D(xx_edtauy_file, 'wedtauyFld', 
     & xx_edtauy_dummy, boundsVec, eddytauy, masks, paramSmooth, 
     & mythid )
c-- exchange UV:
       CALL EXCH_UV_XYZ_RS(eddyTauX,eddyTauY,.TRUE.,myThid)
#endif


#ifdef ALLOW_DIFFKR_CONTROL
      boundsVec(1)=1. _d -6
      boundsVec(2)=2. _d -6
      boundsVec(3)=0.9 _d -3
      boundsVec(4)=1. _d -3
      boundsVec(5)=0.
      paramSmooth=1      
      call ctrl_map_ini_gen3D(xx_diffkr_file, 'wdiffkrFld', 
     & xx_diffkr_dummy, boundsVec, diffkr, maskc, paramSmooth, 
     & mythid )
      _EXCH_XYZ_R8( diffkr, mythid)
#endif


#ifdef ALLOW_KAPGM_CONTROL
      boundsVec(1)=1. _d 1
      boundsVec(2)=2. _d 1
      boundsVec(3)=0.9 _d 4
      boundsVec(4)=1. _d 4
      boundsVec(5)=0.
      paramSmooth=1      
      call ctrl_map_ini_gen3D(xx_kapgm_file, 'wkapgmFld', 
     & xx_kapgm_dummy, boundsVec, kapgm, maskc, paramSmooth, 
     & mythid )
      _EXCH_XYZ_R8( kapgm, mythid)
#endif


#ifdef ALLOW_KAPREDI_CONTROL
      boundsVec(1)=1. _d 1
      boundsVec(2)=2. _d 1
      boundsVec(3)=0.9 _d 4
      boundsVec(4)=1. _d 4
      boundsVec(5)=0.
      paramSmooth=1      
      call ctrl_map_ini_gen3D(xx_kapredi_file, 'wkaprediFld', 
     & xx_kapredi_dummy, boundsVec, kapredi, maskc, paramSmooth, 
     & mythid )
      _EXCH_XYZ_R8( kapredi, mythid)
#endif



#endif /* ALLOW_ECCO */

      return
      end

