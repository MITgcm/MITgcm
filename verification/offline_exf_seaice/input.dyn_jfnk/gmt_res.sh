#!/bin/bash 
# GMT script to plot the development of the residual and the 
# number of Krylov steps per Newton step
#

\rm -rf .gmtdefault*
gmtset HEADER_FONT_SIZE 24p
gmtset GRID_PEN_PRIMARY thinnest,-

ps=res.ps
prj=-JX14/12l
xticks=100g500a500
#ticks="-B${xticks}:newton iteration:/1g5a5:residual:WSne"
ticks="-B${xticks}/1g5a5:residual:WSne"
xoff=-X4c
yoff=-Y14c

infile=STDOUT.0000
infile=$1
tmpfile1=res.tmp
tmpfile2=iter.tmp
grep JFNK: $infile | grep initial | \
    awk '{printf("%i %e\n", $(NF-2), $NF)}' > ${tmpfile1}
# get min/max as y-axis limits
R=`minmax -I1.e-16 ${tmpfile1}`
RR=`echo $R | awk -F/ '{printf("%e/%e",$3,$4)}'`
#R=-R0/2400/1.e-16/10
R=-R0/2400/$RR
psbasemap ${xoff} ${yoff} $R $prj -G255 "${ticks}" -P -K > $ps
psxy ${tmpfile1} -R -J -W1p,blue -O -K >> $ps
psxy ${tmpfile1} -R -J -Sx0.075i -O -K >> $ps
# add number of FGMRES iterations
grep JFNK: $infile | grep 'Nb. of FGMRES' | \
    awk '{printf("%i %i\n",$12,$NF)}' > ${tmpfile2}
yoff=-Y-10c
prj=-JX14/7
ticks="-B${xticks}:newton iteration:/1g5a5:FGMRES iterations:WSne"
ticks="-B${xticks}:newton iteration:/a10f5g10:FGMRES iterations:WSne"
# get x-range
R1=`echo $R | awk -F/ '{printf("%s/%s/",$1,$2)}'`
R=${R1}0/50
#R2=`minmax -C ${tmpfile2} | awk '{printf("%i/%i",$3,$4)}'`
#R=$R1$R2
psbasemap -Y-8c $R $prj -G255 "${ticks}" -O -K >> $ps
psxy ${tmpfile2} -R -J -W1p,blue -O -K >> $ps
psxy ${tmpfile2} -R -J -Sx0.075i -O -K >> $ps
#
echo "" | psxy -R -J -O  >> $ps

# if available show plot
have_gv=`which gv`
if [[ x$have_gv != x ]]; then
 gv $ps
fi
# generate png-file
ps2raster -A -Tg ${ps}

# clean up
\rm -f ${tmpfile1} ${tmpfile2} ${ps}



