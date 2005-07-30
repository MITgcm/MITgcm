
These are the daily MITgcm testing scripts used by Ed Hill on the
ITRDA head node of the ACRSgrid cluster.  The reslts are automatically
fed (by email using the mpack program) into the MITgcm web site at:

  http://mitgcm.org/testing.html

and the scripts have the following pecularities:

  - many paths are hard-coded

  - the "itrda_test_all" script needs an "mpack" binary which 
    can be obtained from the main MITgcm code using:

      $ cd MITgcm/tools/mpack-1.6
      $ make all

  - a current copy of MITgcm is checked out using cvs pserver 
    so you will need the corresponding pserver key in your 
    ~/.cvspass which can be obtained (and which only needs to 
    be performed *once*):

      $ cvs -d ':pserver:cvsanon@mitgcm.org:/u/gcmpack' login
      ===> ( enter the CVS password: "cvsanon" )


The files are:

  itrda_crontab		   result of "crontab -l > itrda_crontab"
  itrda_test_all	   main script called by cron
    itrda_gnu_test_mpi	    \
    itrda_intel_test_mpi     +=> MPI test scripts for each compiler
    itrda_pgi_test_mpi	    /

