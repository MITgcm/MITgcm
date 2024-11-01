Gray atmosphere physics example on Cubed-Sphere grid
============================================================

Use gray atmospheric physics (O'Gorman and Schneider, JCl, 2008)
from package "atm_phys" inside  MITgcm dynamical core.

1) standard cubed-sphere set-up (6 faces 32x32, 26 levels, non uniform deltaP)
   atm_gray/code/   : specific code
 primary set-up:
      uses 10m mixed layer depth with evolving SST (including a Q-flux)
      + damping of stratospheric winds ; starts @ t= 1.yr from pickup files.
   atm_gray/input/  : input files (+ uses script "prepare_run" in input)
   atm_gray/results/output.txt : standard output of a short reference run
 secondary set-up (using same executable):
      uses fixed SST (APE) and improved radiation code from Ruth Geen.
   atm_gray/inputu.ape/  : specific input files (remaining files from dir input)
   atm_gray/results/output.ape.txt : secondary output of a short reference run

--------------
Instructions:
--------------
Download MITgcm (including verification experiments) from MITgcm repository
 <my_dir_space> cvs co -P -d MITgcm_current MITgcm
and download "atm_gray" set-up from MITgcm_contrib repository,
   a) either directly in dir verification
     <my_dir_space> cd MITgcm_current/verification
     <verification> cvs co -P -d atm_gray MITgcm_contrib/verification_other/atm_gray
or b) in a similar level in the directory tree, e.g., in verification_other in MITgcm_current:
     <my_dir_space> cd MITgcm_current
     <MITgcm_current> mkdir verification_other ; cd verification_other
     <verification_other> cvs co -P -d atm_gray MITgcm_contrib/verification_other/atm_gray

to built and run the 1rst (standard) CS-32 set-up:
> cd atm_gray/build
> (if done after a previous built, do "make Clean" first)
> ../../../tools/genmake2 -mods ../code -of ../../../tools/build_options/[Selected-Option-File]
> make depend
> make
> cd ../run
> (if done after a previous run, clean-up all files: /bin/rm -f * )
> ln -s ../input/* .
> ../input/prepare_run
> ln -s ../build/mitgcmuv .
> mitgcmuv >& output.txt
and output.txt can be compared with atm_gray/results/output.txt

--------------
