%-- In order to run a short test where "something" happens,
%   provide pseudo initial conditions which correspond to the output
%   of a standard run, at t=2.h, when convection starts to develop.

% $Header: /u/gcmpack/MITgcm/verification/tutorial_deep_convection/input/gen_ini.m,v 1.1 2008/04/24 01:48:52 jmc Exp $
% $Name:  $

it=360; %- t= 2.h in the simulation

namF='T';
var=rdmds(namF,it);
fid=fopen([namF,'.120mn.bin'],'w','b'); fwrite(fid,var,'real*4'); fclose(fid);

namF='Eta';
var=rdmds(namF,it);
fid=fopen([namF,'.120mn.bin'],'w','b'); fwrite(fid,var,'real*4'); fclose(fid);

namF='U';
var=rdmds(namF,it);
fid=fopen([namF,'.120mn.bin'],'w','b'); fwrite(fid,var,'real*4'); fclose(fid);

namF='V';
var=rdmds(namF,it);
fid=fopen([namF,'.120mn.bin'],'w','b'); fwrite(fid,var,'real*4'); fclose(fid);

