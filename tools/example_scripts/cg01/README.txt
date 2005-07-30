$Header: /u/gcmpack/MITgcm/tools/example_scripts/cg01/Attic/README.txt,v 1.1 2005/07/30 15:13:43 jmc Exp $
$Name:  $

These are the daily MITgcm testing scripts used by Ed Hill on the
"cg01" head node.  The reslts are automatically fed (by email using
the mpack program) into the MITgcm web site at:

  http://mitgcm.org/testing.html

and the scripts have the following pecularities:

  - many paths are hard-coded

  - a current copy of MITgcm is checked out using cvs pserver 
    so you will need the corresponding pserver key in your 
    ~/.cvspass which can be obtained (and which only needs to 
    be performed *once*):

      $ cvs -d ':pserver:cvsanon@mitgcm.org:/u/gcmpack' login
      ===> ( enter the CVS password: "cvsanon" )


The files are:

  cg01_crontab		   result of "crontab -l > itrda_crontab"
    cg01_g77_test_mpi	    \
    cg01_intel_test_mpi      +=> MPI test scripts for each compiler
    cg01_pgi_test_mpi	    /

