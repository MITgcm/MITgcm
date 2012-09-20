#!/bin/bash
OADTOOLS=`awk '{print $1 }' < oadtempflile`
echo "#generated file" > temp.sed
for i in `grep '^ *SUBROUTINE ' $1 | awk '{print $2}'`
do 
  # extract the name
  srName=${i%%\(*}
  echo "/^ *SUBROUTINE $srName(/i\\" >> temp.sed
  case "$srName" in 
    "OpenAD_the_second_level_loop" | \
    "OpenAD_the_third_level_loop" |  \
    "OpenAD_the_fourth_level_loop" )
      echo "c\$openad XXX Template $OADTOOLS/ad_template.checkpoint.f" >> temp.sed
      ;;
    "OpenAD_the_first_level_loop" )
      echo "c\$openad XXX Template $OADTOOLS/ad_template.checkpoint_to_split.f" >> temp.sed
      ;;
    "OpenAD_the_first_level_plain" | \
    "OpenAD_the_second_level_plain" | \
    "OpenAD_the_third_level_plain" | \
    "OpenAD_the_fourth_level_plain" ) 
      echo "c\$openad XXX Template $OADTOOLS/ad_template.plain.f" >> temp.sed
      ;;
    "OpenAD_cg2d" ) 
      echo "c\$openad XXX Template $OADTOOLS/ad_template.sa_cg2d.f" >> temp.sed
      ;;
    "OpenAD_exch1_rl" |          \
    "OpenAD_exch1_rs" |          \
    "OpenAD_global_max_r8" |       \
    "OpenAD_global_sum_r8" )  
      tmpName=${srName#OpenAD_}
      echo "c\$openad XXX Template $OADTOOLS/ad_template.${tmpName}.f" >> temp.sed
      ;;
    *) 
      echo "test $OADTOOLS"
      echo "c\$openad XXX Template $OADTOOLS/ad_template.split.f" >> temp.sed
      ;;
  esac
done
cat $1 | sed -f temp.sed > $2
  
