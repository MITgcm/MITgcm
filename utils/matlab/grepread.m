function [vals] = grepread(file,varargin)
% VALS = GREPREAD(FILE,EXPR1,...);
%
% Extracts the expressions "expr1","expr2",... from the file "file".
% This assumes output in the standard form defined by the MITgcm
% monitor package and is not a replacement for TEXTREAD.
%
% e.g.
% >> vals=grepread('output.txt','time_secondsf','ke_mean','ke_max');
% >> plot(vals(:,1)/86400,vals(:,2:3));

if nargin<2
 error('You must supply a filename and at least one search expression!')
end

tfile=sprintf('/tmp/grepexpr%15.15f',rand);
for k=1:nargin-1;
 try
  eval(['!grep ' varargin{k} ' ' file ' | sed s/.\*=// | sed s/NAN/1.23456789/ >! ' tfile])
  vals(:,k)=textread(tfile,'%f');
  delete(tfile)
 catch
  delete(tfile)
  error(sprintf('An error occured while scanning for: %s',varargin{k}));
 end
end
