#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: STREAMICE_CHECK

C !INTERFACE:
      SUBROUTINE STREAMICE_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"
#include "STREAMICE_BDRY.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_STREAMICE
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
c     CHARACTER*(10) fname
c     INTEGER m,n,k

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'STREAMICE_CHECK: #define STREAMICE'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

#ifndef STREAMICE_COULOMB_SLIDING

      IF (STREAMICE_allow_reg_coulomb) THEN

       WRITE(msgBuf,'(A)')
     &   'STREAMICE_CHECK: STREAMICE_COULOMB_SLIDING must be defined'
        CALL PRINT_ERROR( msgBuf, myThid )

      ENDIF

#endif

      IF(n_basal_friction .lt. 0.0) THEN
       WRITE(msgBuf,'(A)')
     &   'STREAMICE_CHECK: n_basal_friction must be nonnegative number'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

C-   write out integer parameters

       CALL WRITE_0D_I( streamice_max_cg_iter, INDEX_NONE,
     &  ' max cg interations =',
     &  ' /* user defined parameter */')
       CALL WRITE_0D_I( streamice_max_nl_iter, INDEX_NONE,
     &  ' max nonlin interations =',
     &  ' /* user defined parameter */')
       CALL WRITE_0D_I( streamice_nstep_velocity, INDEX_NONE,
     &  ' # of timesteps per velocity solve  =',
     &  ' /* user defined parameter */')

C-   write out real parameters

       CALL WRITE_0D_RL( streamice_density, INDEX_NONE,
     & ' streamice_density =', ' /* user defined parameter */')
       CALL WRITE_0D_RL( streamice_density_ocean_avg, INDEX_NONE,
     & ' streamice_density_ocean_avg =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( B_glen_isothermal, INDEX_NONE,
     & ' glens law stifness =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( n_glen, INDEX_NONE,
     & ' glens law exponent =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( C_basal_fric_const, INDEX_NONE,
     & ' basal trac coeff =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( n_basal_friction, INDEX_NONE,
     & ' basal exponent =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( streamice_input_flux_unif, INDEX_NONE,
     & ' uniform input flux (m^2/a) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( streamice_vel_update, INDEX_NONE,
     & ' time for ice vel update (sec) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( streamice_cg_tol, INDEX_NONE,
     & ' conj grad tolerance =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( streamice_nonlin_tol, INDEX_NONE,
     & ' ice velocity nonlinear iter tol =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( shelf_max_draft, INDEX_NONE,
     & ' max shelf draft for analytic init (m) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( shelf_min_draft, INDEX_NONE,
     & ' min shelf draft for analytic init (m) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( shelf_edge_pos, INDEX_NONE,
     & ' shelf extent for analytic init (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( shelf_slope_scale, INDEX_NONE,
     & ' slope scale for analytic init (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( shelf_flat_width, INDEX_NONE,
     & ' flat shelf width for analytic init (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( flow_dir, INDEX_NONE,
     & ' flow direction for analytic init =',
     & ' /* user defined parameter */')

       CALL WRITE_0D_RL( min_x_noflow_NORTH, INDEX_NONE,
     & ' min range on no bd for no-slip (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_noflow_NORTH, INDEX_NONE,
     & ' max range on no bd for no-slip (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_x_noflow_SOUTH, INDEX_NONE,
     & ' min range on so bd for no-slip (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_noflow_SOUTH, INDEX_NONE,
     & ' max range on so bd for no-slip (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_noflow_EAST, INDEX_NONE,
     & ' min range on east bd for no-slip (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_noflow_EAST, INDEX_NONE,
     & ' max range on east bd for no-slip (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_noflow_WEST, INDEX_NONE,
     & ' min range on west bd for no-slip (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_noflow_WEST, INDEX_NONE,
     & ' max range on west bd for no-slip (km) =',
     & ' /* user defined parameter */')

       CALL WRITE_0D_RL( min_x_noStress_NORTH, INDEX_NONE,
     & ' min range on no bd for no-stress (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_noStress_NORTH, INDEX_NONE,
     & ' max range on no bd for no-stress (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_x_noStress_SOUTH, INDEX_NONE,
     & ' min range on so bd for no-stress (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_noStress_SOUTH, INDEX_NONE,
     & ' max range on so bd for no-stress (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_noStress_EAST, INDEX_NONE,
     & ' min range on east bd for no-stress (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_noStress_EAST, INDEX_NONE,
     & ' max range on east bd for no-stress (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_noStress_WEST, INDEX_NONE,
     & ' min range on west bd for no-stress (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_noStress_WEST, INDEX_NONE,
     & ' max range on west bd for no-stress (km) =',
     & ' /* user defined parameter */')

       CALL WRITE_0D_RL( min_x_FluxBdry_NORTH, INDEX_NONE,
     & ' min range on no bd for FluxBdry (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_FluxBdry_NORTH, INDEX_NONE,
     & ' max range on no bd for FluxBdry (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_x_FluxBdry_SOUTH, INDEX_NONE,
     & ' min range on so bd for FluxBdry (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_FluxBdry_SOUTH, INDEX_NONE,
     & ' max range on so bd for FluxBdry (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_FluxBdry_EAST, INDEX_NONE,
     & ' min range on east bd for FluxBdry (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_FluxBdry_EAST, INDEX_NONE,
     & ' max range on east bd for FluxBdry (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_FluxBdry_WEST, INDEX_NONE,
     & ' min range on west bd for FluxBdry (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_FluxBdry_WEST, INDEX_NONE,
     & ' max range on west bd for FluxBdry (km) =',
     & ' /* user defined parameter */')

       CALL WRITE_0D_RL( min_x_Dirich_NORTH, INDEX_NONE,
     & ' min range on no bd for Dirich (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_Dirich_NORTH, INDEX_NONE,
     & ' max range on no bd for Dirich (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_x_Dirich_SOUTH, INDEX_NONE,
     & ' min range on so bd for Dirich (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_Dirich_SOUTH, INDEX_NONE,
     & ' max range on so bd for Dirich (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_Dirich_EAST, INDEX_NONE,
     & ' min range on east bd for Dirich (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_Dirich_EAST, INDEX_NONE,
     & ' max range on east bd for Dirich (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_Dirich_WEST, INDEX_NONE,
     & ' min range on west bd for Dirich (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_Dirich_WEST, INDEX_NONE,
     & ' max range on west bd for Dirich (km) =',
     & ' /* user defined parameter */')

       CALL WRITE_0D_RL( min_x_CFBC_NORTH, INDEX_NONE,
     & ' min range on no bd for CFBC (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_CFBC_NORTH, INDEX_NONE,
     & ' max range on no bd for CFBC (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_x_CFBC_SOUTH, INDEX_NONE,
     & ' min range on so bd for CFBC (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_x_CFBC_SOUTH, INDEX_NONE,
     & ' max range on so bd for CFBC (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_CFBC_EAST, INDEX_NONE,
     & ' min range on east bd for CFBC (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_CFBC_EAST, INDEX_NONE,
     & ' max range on east bd for CFBC (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( min_y_CFBC_WEST, INDEX_NONE,
     & ' min range on west bd for CFBC (km) =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( max_y_CFBC_WEST, INDEX_NONE,
     & ' max range on west bd for CFBC (km) =',
     & ' /* user defined parameter */')

       CALL WRITE_0D_RL( flux_bdry_val_NORTH, INDEX_NONE,
     & ' val (m^2/a) for north flux bdry =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( flux_bdry_val_SOUTH, INDEX_NONE,
     & ' val (m^2/a) for south flux bdry =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( flux_bdry_val_EAST, INDEX_NONE,
     & ' val (m^2/a) for east flux bdry =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_RL( flux_bdry_val_WEST, INDEX_NONE,
     & ' val (m^2/a) for west flux bdry =',
     & ' /* user defined parameter */')

C-   write out logical parameters

       CALL WRITE_0D_L( STREAMICE_dump_mdsio, INDEX_NONE,
     & ' streamice_dump_mdsio =', ' /* user defined parameter */')
       CALL WRITE_0D_L( STREAMICE_dump_mdsio, INDEX_NONE,
     & ' streamice_dump_mdsio =', ' /* user defined parameter */')
       CALL WRITE_0D_L( STREAMICE_dump_mnc, INDEX_NONE,
     & ' streamice_dump_mnc =', ' /* user defined parameter */')
       CALL WRITE_0D_L( STREAMICE_tave_mnc, INDEX_NONE,
     & ' streamice_tave_mnc =', ' /* user defined parameter */')
c       CALL WRITE_0D_L( STREAMICE_GL_regularize, INDEX_NONE,
c     & ' streamice_GL_regularize =', ' /* user defined parameter */')
       CALL WRITE_0D_L( STREAMICE_move_front, INDEX_NONE,
     & ' streamice_move_front =', ' /* user defined parameter */')
       CALL WRITE_0D_L( STREAMICE_calve_to_mask, INDEX_NONE,
     & ' streamice_calve_to_mask =', ' /* user defined parameter */')

C-   write out string parameters

       CALL WRITE_0D_C( STREAMICEthickInit,-1,INDEX_NONE,
     & 'STREAMICEthickInit =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_C( STREAMICEthickFile,-1,INDEX_NONE,
     & 'STREAMICEthickFile =',
     & ' /* user defined parameter */')
       CALL WRITE_0D_C( STREAMICEcalveMaskFile,-1,INDEX_NONE,
     & 'STREAMICEcalveMaskFile =',
     & ' /* user defined parameter */')

C-     Put stops here if some flags are wrongly chosen.
C-     For example, require this package to use tempStepping

      _END_MASTER(myThid)

C    write init_thickness array to a binary file

c        CALL WRITE_FLD_XY_RL ( "H_streamIce", "init",
c      & H_streamIce, 0, myThid )
c        CALL WRITE_FLD_XY_RL ( "area_shelf_streamice", "init",
c      & area_shelf_streamice, 0, myThid )
c        CALL WRITE_FLD_XY_RL ( "STREAMICE_hmask", "init",
c      & STREAMICE_hmask, 0, myThid )
c        CALL WRITE_FLD_XY_RL ( "base_el_streamice", "init",
c      & base_el_streamice, 0, myThid )
c        CALL WRITE_FLD_XY_RL ( "ssurf_el_streamice", "init",
c      & surf_el_streamice, 0, myThid )

c       DO m=1,4
c        DO n=1,4
c         DO k=1,2
c          WRITE(fname,'(A,I1,A,I1,A,I1)')
c     &     "Dphi_", m, "_", n, "_", k
c          CALL WRITE_FLD_XY_RL (fname, "",
c     &     Dphi(:,:,:,:,m,n,k),0,myThid)
c         ENDDO
c        ENDDO
c       ENDDO

       CALL WRITE_FLD_XY_RL ( "k1AtC_str", "out",
     & k1AtC_str, 0, myThid )
       CALL WRITE_FLD_XY_RL ( "k2AtC_str", "out",
     & k2AtC_str, 0, myThid )
       CALL WRITE_FLD_XY_RS ( "STREAMICE_ufacemask_bdry", "out",
     & STREAMICE_ufacemask_bdry, 0, myThid )
       CALL WRITE_FLD_XY_RS ( "STREAMICE_vfacemask_bdry", "out",
     & STREAMICE_vfacemask_bdry, 0, myThid )
       CALL WRITE_FLD_XY_RL ( "u_bdry_values_SI", "out",
     & u_bdry_values_SI, 0, myThid )
       CALL WRITE_FLD_XY_RL ( "v_bdry_values_SI", "out",
     & v_bdry_values_SI, 0, myThid )
       CALL WRITE_FLD_XY_RL ( "u_flux_bdry_SI", "out",
     & u_flux_bdry_SI, 0, myThid )
       CALL WRITE_FLD_XY_RL ( "v_flux_bdry_SI", "out",
     & v_flux_bdry_SI, 0, myThid )

#endif /* ALLOW_STREAMICE */

      RETURN
      END
