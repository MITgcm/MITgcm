#! /usr/bin/env bash
OADTOOLS=`dirname $0`
#OADTOOLS=`awk '{print $1 }' < oadtempflile`
echo "#generated file" > temp.sed
for i in `grep '^ *SUBROUTINE ' $1 | awk '{print $2}'`
do
  # extract the name
  srName=${i%%\(*}
  echo "/^ *SUBROUTINE $srName(/i\\" >> temp.sed
  case "$srName" in
    "OpenAD_main_do_loop" )
      if [ -z ${ALLOW_OPENAD_DIVA+x} ]; then
        echo "c\$openad XXX Template ad_template.revolve.f" >> temp.sed
      else
        echo "c\$openad XXX Template ad_template.regular.f" >> temp.sed
      fi
      ;;
    "OpenAD_streamice_cg_make_a" )
      echo "c\$openad XXX Template ad_template.split_non_anonymous.f" >> temp.sed
      ;;
    "OpenAD_inner_do_loop" )
      echo "c\$openad XXX Template ad_template.revolve.f" >> temp.sed
      ;;
    "OpenAD_cg2d" )
      echo "c\$openad XXX Template ad_template.sa_cg2d.f" >> temp.sed
      ;;
    "OpenAD_streamice_cg_solve" )
      echo "c\$openad XXX Template ad_template.streamice_cg_solve.f" >> temp.sed
      ;;
    "OpenAD_active_read_xyz" )
      echo "c\$openad XXX Template ad_template.active_read_xyz.f" >> temp.sed
      ;;
    "OpenAD_active_read_xy" )
      echo "c\$openad XXX Template ad_template.active_read_xy.f" >> temp.sed
      ;;
    "OpenAD_active_write_xy" )
      echo "c\$openad XXX Template ad_template.active_write_xy.f" >> temp.sed
      ;;
    "OpenAD_dummy_in_stepping" )
      echo "c\$openad XXX Template ad_template.dummy_in_stepping.f" >> temp.sed
      ;;
    "OpenAD_streamice_vel_phistage" )
      echo "c\$openad XXX Template ad_template.streamice_vel_phistage.f" >> temp.sed
      ;;
    "OpenAD_streamice_get_fp_err_oad" )
      echo "c\$openad XXX Template ad_template.streamice_get_fp_err_oad.f" >> temp.sed
      ;;
    "OpenAD_streamice_get_vel_resid_err_oad" )
      echo "c\$openad XXX Template ad_template.streamice_get_vel_resid_err_oad.f" >> temp.sed
      ;;
    "OpenAD_streamice_invert_surf_forthick" )
      echo "c\$openad XXX Template ad_template.streamice_invert_surf_forthick.f" >> temp.sed
      ;;
    "OpenAD_streamice_smooth_adjoint_field" )
      echo "c\$openad XXX Template ad_template.streamice_smooth_adjoint_field.f" >> temp.sed
      ;;
    "OpenAD_exch1_rl" |          \
    "OpenAD_exch1_rs" |          \
    "OpenAD_global_max_r8" |     \
    "OpenAD_global_sum_r8" |     \
    "OpenAD_global_sum_tile_rl" )
      tmpName=${srName#OpenAD_}
      echo "c\$openad XXX Template ad_template.${tmpName}.f" >> temp.sed
      ;;
    *)
      echo "c\$openad XXX Template ad_template.split.f" >> temp.sed
      ;;
  esac
done
cat $1 | sed -f temp.sed > $2
