C $Header: /u/gcmpack/MITgcm/pkg/autodiff/ECCO_CPPOPTIONS.h,v 1.1 2012/08/10 17:41:45 jmc Exp $
C $Name:  $

--->>>     COMPILE FAILURE IS DELIBERATE     <<<---
--->>> default ECCO_CPPOPTIONS.h option-file <<<---
--->>> CANNOT be used WITHOUT CUSTOMIZATION  <<<---

#ifndef ECCO_CPPOPTIONS_H
#define ECCO_CPPOPTIONS_H

C-- Collect here, in a single option-file, options to control which optional
C   features to compile in packages AUTODIFF, COST, CTRL, ECCO, CAL and EXF.
C   If used, this option-file needs to be directly included in CPP_OPTIONS.h
C   Although this method, inherited from ECCO setup, has been traditionally
C   used for all adjoint built, work is in progess to allow to use the
C   standard method (each of the above pkg get its own options from its
C   specific option-file) also for adjoint built.

C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************

C-- see pkg/ecco/ECCO_OPTIONS.h file

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C-- see pkg/autodiff/AUTODIFF_OPTIONS.h file

C ********************************************************************
C ***                     Calendar Package                         ***
C ********************************************************************

CPH >>>>>> THERE ARE NO MORE CAL OPTIONS TO BE SET <<<<<<

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************

C-- see pkg/cost/COST_OPTIONS.h file

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

C-- see pkg/ctrl/CTRL_OPTIONS.h file

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************

C-- see pkg/exf/EXF_OPTIONS.h file

#endif /* ECCO_CPPOPTIONS_H */
