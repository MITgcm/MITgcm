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
      echo "c\$openad XXX Template ad_template.revolve.f" >> temp.sed
      ;;
    "OpenAD_cg2d" )
      echo "c\$openad XXX Template ad_template.sa_cg2d.f" >> temp.sed
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
