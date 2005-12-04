#!/bin/bash
echo "#generated file" > temp.sed
for i in `grep '^      SUBROUTINE ' $1 | awk '{print $2}'`
do 
  # extract the name
  srName=${i%%\(*}
  echo "/^      SUBROUTINE $srName/i\\" >> temp.sed
  if [ "$srName" = "the_first_level_loop" -o "$srName" = "the_second_level_loop" -o "$srName" = "the_third_level_loop" -o "$srName" = "the_fourth_level_loop" ] 
  then 
    echo "c\$openad XXX Template ../code_ad/ad_template.checkpoint.f" >> temp.sed
  else
    echo "c\$openad XXX Template ../code_ad/ad_template.split.f" >> temp.sed
  fi
done
cat $1 | sed -f temp.sed > $2
  
