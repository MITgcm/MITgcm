
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#undef  MULTIPLE_RECORD_STATE_FILES

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP

C     !INTERFACE:
      SUBROUTINE NF90IO_INIT_FILE ( ncfilename, myThid )
        use netcdf
        IMPLICIT NONE

#include "SIZE.h"
#include "BUILD_INFO.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C #include "mpif.h"  EESupport already includes this
C #include "NF90IO.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NF90IO.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

        INTEGER myThid
        character*(MAX_LEN_FNAM) ncfilename

        INTEGER mode_flag, ncid, err, i, j, k, t
        INTEGER bi, bj
        INTEGER i_dimid, j_dimid, k_dimid, dimids(4), varid
        INTEGER ig_dimid, jg_dimid, kl_dimid, ku_dimid, kp1_dimid
        INTEGER start(4), count(4), rec_dimid
        LOGICAL iAmDoingIO
        character*(MAX_LEN_MBUF) msgbuf
        _RL     a(sNx,sNy,Nr)
        _RL     is(sNx+10)
        _RL     js(sNy+10)
        _RL     ks(Nr+10)

C initialize this new file...
        mode_flag = IOR(nf90_netcdf4, nf90_classic_model)
        IF (usingMPI) THEN
#ifdef ALLOW_USE_MPI
              mode_flag = IOR(mode_flag, nf90_mpiio)
              err = nf90_create(ncfilename, mode_flag, ncid, comm =
     $             MPI_COMM_MODEL,info = MPI_INFO_NULL)
              CALL nf90ERR(err,"Opening netcdf file w/ MPI",myThid)
#endif
        ELSE
          err = nf90_create(ncfilename, mode_flag, ncid)
          CALL nf90ERR(err,"Opening netcdf file non-MPI",myThid)
        ENDIF
C Set the dimensions...
C record is my unlimited.  Other mitgcms seem to use time...
        err = nf90_def_dim(ncid, "record", nf90_unlimited, rec_dimid)
        CALL nf90ERR(err,"Adding record dim",myThid)
        err = nf90_def_dim(ncid, "i", nSx*sNx*nPx , i_dimid)
        CALL nf90ERR(err,"Adding i dim",myThid)
        err = nf90_def_dim(ncid, "i_g", nSx*sNx*nPx , ig_dimid)
        CALL nf90ERR(err,"Adding i_g dim",myThid)
        err = nf90_def_dim(ncid, "j", nSy*sNy*nPy , j_dimid)
        CALL nf90ERR(err,"Adding j dim",myThid)
        err = nf90_def_dim(ncid, "j_g", nSy*sNy*nPy , jg_dimid)
        CALL nf90ERR(err,"Adding j_g dim",myThid)
        err = nf90_def_dim(ncid, "k", Nr, k_dimid)
        CALL nf90ERR(err,"Adding k dim",myThid)
        err = nf90_def_dim(ncid, "k_l", Nr, kl_dimid)
        CALL nf90ERR(err,"Adding k_l dim",myThid)
        err = nf90_def_dim(ncid, "k_u", Nr, ku_dimid)
        CALL nf90ERR(err,"Adding k_u dim",myThid)
        err = nf90_def_dim(ncid, "k_p1", Nr+1, kp1_dimid)
        CALL nf90ERR(err,"Adding k_p1 dim",myThid)

C Define co-ordinate variables (i.e. data that goes w/ each dimension)
        err = nf90_def_var(ncid, "record", NF90_INT, (/ rec_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding record variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "Sequential record number")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        CALL nf90ERR(err,"Setting attributes",myThid)

        err = nf90_def_var(ncid, "i", NF90_INT, (/ i_dimid /), varid)
        CALL nf90ERR(err,"Adding i variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "x_grid_index")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "x-dimension of the t grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        CALL nf90ERR(err,"Setting attributes",myThid)

        err = nf90_def_var(ncid, "i_g", NF90_INT, (/ ig_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding i_g variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "x_grid_index_at_u_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "x-dimension of the u grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        err = nf90_put_att(ncid, varid,
     $       "c_grid_axis_shift",
     $       -0.5)
        CALL nf90ERR(err,"Setting attributes",myThid)

        err = nf90_def_var(ncid, "j", NF90_INT, (/ j_dimid /), varid)
        CALL nf90ERR(err,"Adding j variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "y_grid_index_at_t_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "y-dimension of the t grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        CALL nf90ERR(err,"Setting attributes",myThid)

        err = nf90_def_var(ncid, "j_g", NF90_INT, (/ jg_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding j_g variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "y_grid_index_at_v_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "y-dimension of the v grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        err = nf90_put_att(ncid, varid,
     $       "c_grid_axis_shift",
     $       -0.5)

        err = nf90_def_var(ncid, "k", NF90_INT, (/ k_dimid /), varid)
        CALL nf90ERR(err,"Adding k variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "z_grid_index")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "z-dimension of the t grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")



        err = nf90_def_var(ncid, "k_l", NF90_INT, (/ kl_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding k_l variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "z_grid_index_at_upper_w_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "z-dimension of the w grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        err = nf90_put_att(ncid, varid,
     $       "c_grid_axis_shift",
     $       0.5)


        err = nf90_def_var(ncid, "k_u", NF90_INT, (/ ku_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding k_u variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "z_grid_index_at_lower_w_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "z-dimension of the w grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        err = nf90_put_att(ncid, varid,
     $       "c_grid_axis_shift",
     $       -0.5)


        err = nf90_def_var(ncid, "k_p1", NF90_INT, (/ kp1_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding k_p1 variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "z_grid_index_at_w_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "z-dimension of the w grid")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "")
        err = nf90_put_att(ncid, varid,
     $       "c_grid_axis_shift",
     $       (/ -0.5, 0.5/))

C     record-like variables...
        err = nf90_def_var(ncid, "iter", NF90_INT, (/ rec_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding iter variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "timestep")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "model timestep number")

        err = nf90_def_var(ncid, "time", NF90_DOUBLE, (/ rec_dimid /),
     $       varid)
        CALL nf90ERR(err,"Adding iter variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "time")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "Time")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "seconds")
        err = nf90_put_att(ncid, varid,
     $       "axis",
     $       "T")
C     NOT SURE IF THIS IS A VARIABLE?
        err = nf90_put_att(ncid, varid,
     $       "calendar",
     $       "gregorian")

C     Water depth
        err = nf90_def_var(ncid, "Depth", NF90_DOUBLE, (/ i_dimid,
     $       j_dimid /),varid)
        CALL nf90ERR(err,"Adding Depth variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "ocean_depth")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "ocean depth")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinate",
     $       "XC YC")


C     Do the grid variables...
        err = nf90_def_var(ncid, "XC", NF90_DOUBLE, (/ i_dimid,
     $       j_dimid /),varid)
        CALL nf90ERR(err,"Adding XC variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "plane_x_coordinate")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "x coordinate")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinate",
     $       "YC XC")

        err = nf90_def_var(ncid, "XG", NF90_DOUBLE, (/ ig_dimid,
     $       jg_dimid /),varid)
        CALL nf90ERR(err,"Adding XG variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "plane_x_coordinate_at_f_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "x coordinate")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinate",
     $       "YG XG")

        err = nf90_def_var(ncid, "YC", NF90_DOUBLE, (/ i_dimid,
     $       j_dimid /),varid)
        CALL nf90ERR(err,"Adding YC variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "plane_y_coordinate")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "y coordinate")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinate",
     $       "YC XC")

        err = nf90_def_var(ncid, "YG", NF90_DOUBLE, (/ ig_dimid,
     $       jg_dimid /),varid)
        CALL nf90ERR(err,"Adding YG variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "plane_y_coordinate_at_f_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "y coordinate")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinate",
     $       "YG XG")


        err = nf90_def_var(ncid, "Z", NF90_DOUBLE, (/ k_dimid /)
     $       ,varid)
        CALL nf90ERR(err,"Adding Z variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "depth")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "vertical coordinate of cell center")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "positive",
     $       "down")
        err = nf90_put_att(ncid, varid,
     $       "coordinate",
     $       "Z")

        err = nf90_def_var(ncid, "Zu", NF90_DOUBLE, (/ ku_dimid /)
     $       ,varid)
        CALL nf90ERR(err,"Adding Zu variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "depth_at_lower_w_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "vertical coordinate of lower cell interface")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "positive",
     $       "down")

        err = nf90_def_var(ncid, "Zl", NF90_DOUBLE, (/ kl_dimid /)
     $       ,varid)
        CALL nf90ERR(err,"Adding Zl variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "depth_at_upper_w_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "vertical coordinate of upper cell interface")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "positive",
     $       "down")

        err = nf90_def_var(ncid, "Zp1", NF90_DOUBLE, (/ kp1_dimid /)
     $       ,varid)
        CALL nf90ERR(err,"Adding Zp1 variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "depth_at_w_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "vertical coordinate of cell interface")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "positive",
     $       "down")

C     rAs Areas

        err = nf90_def_var(ncid, "rA", NF90_DOUBLE, (/ i_dimid,
     $       j_dimid /),varid)
        CALL nf90ERR(err,"Adding rA variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_area")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell area")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m2")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YC XC")

        err = nf90_def_var(ncid, "rAw", NF90_DOUBLE, (/ ig_dimid,
     $       j_dimid /),varid)
        CALL nf90ERR(err,"Adding rAw variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_area_at_u_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell area")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m2")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YC XG")

        err = nf90_def_var(ncid, "rAs", NF90_DOUBLE, (/ i_dimid,
     $       jg_dimid /),varid)
        CALL nf90ERR(err,"Adding rAs variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_area_at_v_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell area")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m2")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YG XC")

        err = nf90_def_var(ncid, "rAz", NF90_DOUBLE, (/ ig_dimid,
     $       jg_dimid /),varid)
        CALL nf90ERR(err,"Adding rAz variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_area_at_f_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell area")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m2")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YG XG")

C     dx dy:
        err = nf90_def_var(ncid, "dxG", NF90_DOUBLE, (/ i_dimid,
     $       jg_dimid /),varid)
        CALL nf90ERR(err,"Adding dxG variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_x_size_at_v_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell x size")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YG XC")

        err = nf90_def_var(ncid, "dyG", NF90_DOUBLE, (/ ig_dimid,
     $       j_dimid /),varid)
        CALL nf90ERR(err,"Adding dyG variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_y_size_at_u_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell y size")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YC XG")

        err = nf90_def_var(ncid, "dxC", NF90_DOUBLE, (/ ig_dimid,
     $       j_dimid /),varid)
        CALL nf90ERR(err,"Adding dxC variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_x_size_at_u_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell y size")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YC XG")

        err = nf90_def_var(ncid, "dyC", NF90_DOUBLE, (/ i_dimid,
     $       jg_dimid /),varid)
        CALL nf90ERR(err,"Adding dyC variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_y_size_at_v_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell y size")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")
        err = nf90_put_att(ncid, varid,
     $       "coordinates",
     $       "YG XC")

C     drs
        err = nf90_def_var(ncid, "drC", NF90_DOUBLE, (/ kp1_dimid /)
     $       ,varid)
        CALL nf90ERR(err,"Adding drC variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_z_size_at_w_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell z size")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")

        err = nf90_def_var(ncid, "drF", NF90_DOUBLE, (/ k_dimid /)
     $       ,varid)
        CALL nf90ERR(err,"Adding drF variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_z_size")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "cell z size")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m")

C     hFacs:
        err = nf90_def_var(ncid, "hFacC", NF90_DOUBLE, (/ i_dimid,
     $       j_dimid, k_dimid/),varid)
        CALL nf90ERR(err,"Adding hFacC variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_vertical_fraction")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "vertical fraction of open cell")

        err = nf90_def_var(ncid, "hFacW", NF90_DOUBLE, (/ ig_dimid,
     $       j_dimid, k_dimid/),varid)
        CALL nf90ERR(err,"Adding hFacW variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_vertical_fraction_at_u_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "vertical fraction of open cell")

        err = nf90_def_var(ncid, "hFacS", NF90_DOUBLE, (/ i_dimid,
     $       jg_dimid, k_dimid/),varid)
        CALL nf90ERR(err,"Adding hFacS variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_vertical_fraction_at_v_location")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "vertical fraction of open cell")

C     Define pHrefs

        err = nf90_def_var(ncid, "PHrefC", NF90_DOUBLE, (/ k_dimid/)
     $       ,varid)
        CALL nf90ERR(err,"Adding PHrefC variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_reference_pressure")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "Reference Hydrostatif Pressure")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m2 s-2")

        err = nf90_def_var(ncid, "PHrefF", NF90_DOUBLE, (/ kp1_dimid/)
     $       ,varid)
        CALL nf90ERR(err,"Adding PHrefF variable",myThid)
        err = nf90_put_att(ncid, varid,
     $       "standard_name",
     $       "cell_reference_pressure")
        err = nf90_put_att(ncid, varid,
     $       "long_name",
     $       "Reference Hydrostatif Pressure")
        err = nf90_put_att(ncid, varid,
     $       "units",
     $       "m2 s-2")


C     Stop defining stuff and start filling
        err = nf90_enddef(ncid)

C     co-ordinate k:
        err = nf90_inq_varid(ncid, "k", varid)
        CALL nf90ERR(err, "Getting k varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting k variable to par access",myThid)
        ks(1:Nr) = (/  (I, I = 0, Nr-1)  /)
        err = nf90_put_var(ncid, varid, kS(1:Nr),
     $       start = (/ 1 /) , count = (/ Nr /) )
        CALL nf90ERR(err, "Setting k variable values",myThid)

C     co-ordinate i:
        err = nf90_inq_varid(ncid, "i", varid)
        CALL nf90ERR(err, "Getting i varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting i variable to par access",myThid)
        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              is(1:sNx) = (/  (I, I = myXGlobalLo+(bi-1)*sNx-1,
     $             myXGlobalLo+(bi)*sNx-1)  /)
              err = nf90_put_var(ncid, varid,
     &             is(1:sNx),
     &             start = (/ myXGlobalLo+(bi-1)*sNx /) ,
     &             count = (/ sNx/) )
              CALL nf90ERR(err, "Getting i varid",myThid)
           enddo
        enddo

C     co-ordinate i_g:
        err = nf90_inq_varid(ncid, "i_g", varid)
        CALL nf90ERR(err, "Getting i_g varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting i_g variable to par access",myThid)
        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              is(1:sNx) = (/  (I, I = myXGlobalLo+(bi-1)*sNx-1,
     $             myXGlobalLo+(bi)*sNx-1)  /)
              err = nf90_put_var(ncid, varid,
     &             is(1:sNx),
     &             start = (/ myXGlobalLo+(bi-1)*sNx /) ,
     &             count = (/ sNx/) )
              CALL nf90ERR(err, "Getting i_g varid",myThid)
           enddo
        enddo

C     co-ordinate j:
        err = nf90_inq_varid(ncid, "j", varid)
        CALL nf90ERR(err, "Getting j varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting j variable to par access",myThid)

        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              js(1:sNy) = (/  (I, I = myYGlobalLo+(bj-1)*sNy-1,
     &             myYGlobalLo+(bj)*sNy-1)  /)
              err = nf90_put_var(ncid, varid,
     &             js(1:sNy),
     &             start = (/ myYGlobalLo+(bj-1)*sNy /) ,
     &             count = (/ sNy/) )
              CALL nf90ERR(err, "Getting j varid",myThid)
           enddo
        enddo

C     co-ordinate j:
        err = nf90_inq_varid(ncid, "j_g", varid)
        CALL nf90ERR(err, "Getting j_g varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting j_g variable to par access",myThid)

        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              js(1:sNy) = (/  (I, I = myYGlobalLo+(bj-1)*sNy-1,
     &             myYGlobalLo+(bj)*sNy-1)  /)
              err = nf90_put_var(ncid, varid,
     &             js(1:sNy),
     &             start = (/ myYGlobalLo+(bj-1)*sNy /) ,
     &             count = (/ sNy/) )
              CALL nf90ERR(err, "Putting j_g varid",myThid)
           enddo
        enddo


C     co-ordinate ks:
        do t = 1,3
           if (t.EQ.1) then
              err = nf90_inq_varid(ncid, "k", varid)
           else if (t.EQ.2) then
              err = nf90_inq_varid(ncid, "k_u", varid)
           else if (t.EQ.3) then
              err = nf90_inq_varid(ncid, "k_l", varid)
           endif
           CALL nf90ERR(err, "Getting k varid",myThid)
           err = nf90io_var_par_access(ncid, varid)
           CALL nf90ERR(err, "Setting k variable to par access"
     $          ,myThid)

           ks(1:Nr) = (/  (I, I = 0,Nr-1)  /)
           err = nf90_put_var(ncid, varid,
     &          ks(1:Nr),
     &          start = (/ 1 /) ,
     &          count = (/ Nr /) )
           CALL nf90ERR(err, "Getting k varid",myThid)
        enddo

C     co-ordinate k+:
        err = nf90_inq_varid(ncid, "k_p1", varid)
        CALL nf90ERR(err, "Getting k_p1 varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting k_p1 variable to par access"
     $       ,myThid)
        ks(1:Nr+1) = (/  (I, I = 0,Nr)  /)
        err = nf90_put_var(ncid, varid,
     &       ks(1:Nr+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )
        CALL nf90ERR(err, "Writing k_p1",myThid)


CCCCCCCCCCCCCC
C     Now fill the grid data from variables...
C     z-s
        err = nf90_inq_varid(ncid, "Z", varid)
        CALL nf90ERR(err, "Getting Z varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rC,
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )

        err = nf90_inq_varid(ncid, "Zp1", varid)
        CALL nf90ERR(err, "Getting Zp1 varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rF,
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )

        err = nf90_inq_varid(ncid, "Zu", varid)
        CALL nf90ERR(err, "Getting Zu varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rF(2:Nr+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )

        err = nf90_inq_varid(ncid, "drC", varid)
        CALL nf90ERR(err, "Getting drC varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       drC(1:Nr+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )
        CALL nf90ERR(err, "writing drC ",myThid)

        err = nf90_inq_varid(ncid, "drF", varid)
        CALL nf90ERR(err, "Getting drF varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       drF(1:Nr),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )
        CALL nf90ERR(err, "writing drF",myThid)


        err = nf90_inq_varid(ncid, "Zl", varid)
        CALL nf90ERR(err, "Getting Zl varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rF(1:Nr),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )

C     Horizontal co-ordinates...

        CALL NF90IO_FILL_2Dnorec(ncid, "XC", xC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "XG", xG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "YC", yC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "YG", yG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dxC", dxC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dxG", dxG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dyC", dxC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dyG", dxG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rA", rA, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rAz", rAz, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rAw", rAw, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rAs", rAs, myThid)

C     Hfacs
        CALL NF90IO_FILL_3Dnorec(ncid, "hFacC", hFacC, myThid)
        CALL NF90IO_FILL_3Dnorec(ncid, "hFacW", hFacW, myThid)
        CALL NF90IO_FILL_3Dnorec(ncid, "hFacS", hFacS, myThid)

C     Depth:  Special because model doesn't store
        err = nf90_inq_varid(ncid, "Depth", varid)
        CALL nf90ERR(err, "Getting varid Depth",myThid)
        err = nf90io_var_par_access(ncid, varid)
        count(1:2) = (/  sNx, sNy /)
        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              start(1:2) = (/ myXGlobalLo+(bi-1)*sNx,
     &             myYGlobalLo+(bj-1)*sNy /)
              err = nf90_put_var(ncid, varid,
     &             Ro_surf(1:sNx, 1:sNy,bi,bj) -
     &             R_low(1:sNx, 1:sNy,bi,bj),
     &             start = start(1:2), count = count(1:2))
              CALL nf90ERR(err, "Putting Depth into file", myThid)
           enddo
        enddo

C     Reference densities...

        DO k=1,Nr+1
           ks(k) = phiRef(2*k-1)
        ENDDO
        err = nf90_inq_varid(ncid, "PHrefF", varid)
        CALL nf90ERR(err, "Getting PHRefF varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       ks(1:NR+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )

        DO k=1,Nr
           ks(k) = phiRef(2*k)
        ENDDO
        err = nf90_inq_varid(ncid, "PHrefC", varid)
        CALL nf90ERR(err, "Getting PHRefC varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       ks(1:NR),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )


CCCCCCCCCCCCCCCCCC
C     Set file attributes
        err = nf90_redef(ncid)
        CALL nf90ERR(err, "Entering Def mode for file attributes"
     $       ,myThid)

        err = nf90_put_att(ncid, NF90_GLOBAL, 'the_run_name',
     $       the_run_name)

#ifdef THISVER
        err = nf90_put_att( ncid, NF90_GLOBAL, 'MITgcm_version',
     $       THISVER)
        err = nf90_put_att( ncid, NF90_GLOBAL, 'source',
     $       THISVER)
#endif
#ifdef THISUSER
        err = nf90_put_att( ncid, NF90_GLOBAL, 'build_user',
     &       THISUSER)
#endif
#ifdef THISHOST
        err = nf90_put_att( ncid, NF90_GLOBAL, 'build_host',
     &       THISHOST)
#endif
#ifdef THISDATE
        err = nf90_put_att( ncid, NF90_GLOBAL, 'build_date',
     &       THISDATE)
#endif

        err = nf90_put_att( ncid, NF90_GLOBAL, 'MITgcm_URL',
     $       'http://mitgcm.org')

        err = nf90_put_att( ncid, NF90_GLOBAL, 'history',
     $       'Saved from nf90io')

        err = nf90_enddef(ncid)
        CALL nf90ERR(err, "Ending def mode file attributes",myThid)


CCCCCCCCCCCCCCCCCC
C     Close
        err = nf90_close(ncid)
        CALL nf90ERR(err, "Closing after setting up grids",myThid)

        RETURN
        END
