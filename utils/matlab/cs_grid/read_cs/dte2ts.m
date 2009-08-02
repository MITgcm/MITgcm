function ts=dte2ts(dte,deltat,startyr,startmo,startdy)

% Function ts=dte2ts(dte,deltat,startyr,startmo,startdy)
% convert a date to a model time step
%
% INPUTS
% dte      date (e.g., 01-Apr-1997)
% deltat   time step length in s (default 1200)
% startyr  start year (default 1992)
% startmo  start month (default 1)
% startdy  start day (default 1)
% 
% OUTPUTS
% ts       time step number

if nargin < 5, startdy=1; end
if nargin < 4, startmo=1; end
if nargin < 3, startyr=1992; end
if nargin < 2, deltat=1200; end
if nargin < 1, error('please specify a date'); end

ts=(datenum(dte)-datenum(startyr,startmo,startdy))*60*60*24/deltat;

