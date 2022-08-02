#include "CTRL_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

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

#ifdef ECCO_CTRL_DEPRECATED
c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_FIELDS.h"
# include "CTRL_GENARR.h"
# include "ctrl_dummy.h"
# include "optim.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif
#ifdef ALLOW_ECCO
# include "ecco_cost.h"
#endif
#endif /* ECCO_CTRL_DEPRECATED */

c     == routine arguments ==

      integer mythid

#ifdef ECCO_CTRL_DEPRECATED
#ifdef ALLOW_ECCO
c     == local variables ==

      _RS mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
      _RL     tmpfld2d(1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL     tmpfld3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      character*( 80) fnamegeneric
      character*(MAX_LEN_FNAM) suff

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

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
      _EXCH_XY_RL( tmpfld2d, mythid)

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
      _EXCH_XYZ_RL( tmpfld3d, mythid)

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
      _EXCH_XYZ_RL( theta, mythid)
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
      _EXCH_XYZ_RL( salt, mythid)
#endif

#if (defined (ALLOW_UVEL0_CONTROL) && defined (ALLOW_VVEL0_CONTROL))
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.
      paramSmooth=0
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
      CALL CTRL_GET_MASK2D( xx_etan_file, mask2D, myThid )
      call ctrl_map_ini_gen2D(xx_etan_file, 'wetan',
     & xx_etan_dummy, boundsVec, etan, mask2D, paramSmooth,
     & mythid )
      _EXCH_XY_RL( etan, mythid)
#endif

#ifdef ALLOW_RELAXSST_CONTROL
c--   SST relaxation coefficient.
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.
      paramSmooth=1
      CALL CTRL_GET_MASK2D( xx_relaxsst_file, mask2D, myThid )
      call ctrl_map_ini_gen2D(xx_relaxsst_file, 'wrelaxsst',
     & xx_relaxsst_dummy, boundsVec,
     & lambdaThetaClimRelax, mask2D, paramSmooth, mythid )
      _EXCH_XY_RS( lambdaThetaClimRelax, mythid)
#endif

#ifdef ALLOW_RELAXSSS_CONTROL
c--   SSS relaxation coefficient.
      boundsVec(1)=0.
      boundsVec(2)=0.
      boundsVec(3)=0.
      boundsVec(4)=0.
      boundsVec(5)=0.
      paramSmooth=1
      CALL CTRL_GET_MASK2D( xx_relaxsss_file, mask2D, myThid )
      call ctrl_map_ini_gen2D(xx_relaxsss_file, 'wrelaxsss',
     & xx_relaxsss_dummy, boundsVec,
     & lambdaSaltClimRelax, mask2D, paramSmooth, mythid )
      _EXCH_XY_RS( lambdaSaltClimRelax, mythid)
#endif

#ifdef ALLOW_BOTTOMDRAG_CONTROL
      boundsVec(1)=0.
      boundsVec(2)=1. _d -4
      boundsVec(3)=0.9 _d -2
      boundsVec(4)=1. _d -2
      boundsVec(5)=0.
      paramSmooth=1
      CALL CTRL_GET_MASK2D( xx_bottomdrag_file, mask2D, myThid )
      call ctrl_map_ini_gen2D(xx_bottomdrag_file, 'wbottomdrag',
     & xx_bottomdrag_dummy, boundsVec, bottomDragFld, mask2D,
     & paramSmooth, mythid )
      _EXCH_XY_RL( bottomDragFld, mythid)
#endif

#ifdef ALLOW_EDDYPSI_CONTROL
      boundsVec(1)=-0.4
      boundsVec(2)=-0.3
      boundsVec(3)=0.3
      boundsVec(4)=0.4
      boundsVec(5)=0.
      paramSmooth=0
c-- zonal eddy streamfunction : eddyPsiX
      call ctrl_map_ini_gen3D(xx_edtaux_file, 'wedtauxFld',
     & xx_edtaux_dummy, boundsVec, eddyPsiX, maskw, paramSmooth,
     & mythid )
c-- meridional eddy streamfunction : eddyPsiY
      call ctrl_map_ini_gen3D(xx_edtauy_file, 'wedtauyFld',
     & xx_edtauy_dummy, boundsVec, eddyPsiY, masks, paramSmooth,
     & mythid )
c-- exchange UV:
       CALL EXCH_UV_XYZ_RS(eddyPsiX,eddyPsiY,.TRUE.,myThid)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      boundsVec(1)=1. _d -6
      boundsVec(2)=2. _d -6
      boundsVec(3)=4. _d -4
      boundsVec(4)=5. _d -4
      boundsVec(5)=0.
      paramSmooth=1
      call ctrl_map_ini_gen3D(xx_diffkr_file, 'wdiffkrFld',
     & xx_diffkr_dummy, boundsVec, diffKr, maskc, paramSmooth,
     & mythid )
      _EXCH_XYZ_RL( diffKr, mythid )
#endif

#ifdef ALLOW_KAPGM_CONTROL
#ifdef GM_READ_K3D_GM
      boundsVec(1)=1. _d 2
      boundsVec(2)=2. _d 2
      boundsVec(3)=0.9 _d 4
      boundsVec(4)=1. _d 4
      boundsVec(5)=0.
      paramSmooth=1
      call ctrl_map_ini_gen3D(xx_kapgm_file, 'wkapgmFld',
     & xx_kapgm_dummy, boundsVec, GM_inpK3dGM, maskC, paramSmooth,
     & mythid )
      _EXCH_XYZ_RL( GM_inpK3dGM, mythid )
#endif
#endif

#ifdef ALLOW_KAPREDI_CONTROL
#ifdef GM_READ_K3D_REDI
      boundsVec(1)=1. _d 2
      boundsVec(2)=2. _d 2
      boundsVec(3)=0.9 _d 4
      boundsVec(4)=1.0 _d 4
      boundsVec(5)=0.
      paramSmooth=1
      call ctrl_map_ini_gen3D(xx_kapredi_file, 'wkaprediFld',
     & xx_kapredi_dummy, boundsVec, GM_inpK3dRedi, maskC, paramSmooth,
     & mythid )
      _EXCH_XYZ_RL( GM_inpK3dRedi, mythid )
#endif
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do k = 1,maxCtrlTim2D
            do j = 1,sNy
              do i =  1,sNx
                xx_gentim2d(i,j,bi,bj,k)=0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo
#endif

#endif /* ALLOW_ECCO */
#endif /* ECCO_CTRL_DEPRECATED */

      return
      end
