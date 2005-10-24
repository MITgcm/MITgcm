C $Header: /u/gcmpack/MITgcm/pkg/mnc/MNC_PARAMS.h,v 1.14 2005/10/24 18:12:57 jmc Exp $
C $Name:  $
C

CBOP
C     !ROUTINE: MNC_PARAMS.h
C     !INTERFACE:
C     #include MNC_PARAMS.h

C     !DESCRIPTION:
C     Header file defining model "parameters".  The values from the
C     model standard input file are stored into the variables held
C     here. Notes describing the parameters can also be found here.
CEOP

C     ===  PARM_MNC_C Common Block  ===
C     mnc_outdir_str   :: name of the output directory
C     mnc_indir_str    :: name of the input directory

      COMMON /PARM_MNC_C/
     &     mnc_outdir_str,
     &     mnc_indir_str
      CHARACTER*(MAX_LEN_FNAM) mnc_outdir_str
      CHARACTER*(MAX_LEN_FNAM) mnc_indir_str

C     ===  PARM_MNC_L Common Block  ===
C     mnc_use_indir    :: use "mnc_indir_str" as input filename prefix
C     mnc_use_outdir   :: use "mnc_outdir_str" as output filename prefix
C     mnc_outdir_date  :: use a date string within the output dir name
C     mnc_outdir_num   :: use a seq. number within the output dir name 
C     mnc_use_name_ni0 :: use nIter0 in all the file names
C     mnc_echo_gvtypes :: echo type names (fails on many platforms)
C     pickup_write_mnc :: use mnc to write pickups
C     pickup_read_mnc  :: use mnc to read  pickups
C     mon_write_mnc    :: use mnc to write monitor output
C     writegrid_mnc    :: use mnc to write model-grid arrays to file
C     readgrid_mnc     :: read INI_CURVILINEAR_GRID() info using mnc

      COMMON /PARM_MNC_L/ 
     &     mnc_use_indir, mnc_use_outdir, mnc_outdir_date,
     &     mnc_outdir_num, mnc_use_name_ni0, mnc_echo_gvtypes,
     &     pickup_write_mnc, pickup_read_mnc,
     &     timeave_mnc, snapshot_mnc, monitor_mnc, autodiff_mnc, 
     &     writegrid_mnc, readgrid_mnc
      LOGICAL 
     &     mnc_use_indir, mnc_use_outdir, mnc_outdir_date,
     &     mnc_outdir_num, mnc_use_name_ni0, mnc_echo_gvtypes,
     &     pickup_write_mnc, pickup_read_mnc,
     &     timeave_mnc, snapshot_mnc, monitor_mnc, autodiff_mnc, 
     &     writegrid_mnc, readgrid_mnc

C     ===  PARM_MNC_I Common Block  ===
C     mnc_curr_iter    :: current iter for file names

      COMMON /PARM_MNC_I/
     &     mnc_curr_iter
      INTEGER mnc_curr_iter

C     ===  PARM_MNC_R Common Block  ===
C     mnc_max_fsize    :: maximum file size

      COMMON /PARM_MNC_R/
     &     mnc_max_fsize, mnc_filefreq
      REAL*8  mnc_max_fsize
      REAL*8  mnc_filefreq

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
