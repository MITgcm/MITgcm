function dte=ts2dte(ts,deltat,startyr,startmo,startdy,form)

% Function dte=ts2dte(ts,deltat,startyr,startmo,startdy,form)
% convert model time step to a date
%
% INPUTS
% ts       time step number
% deltat   time step length in s (default 1200)
% startyr  start year (default 1992)
% startmo  start month (default 1)
% startdy  start day (default 1)
% form     format of date ("help datestr" for details)
% 
% OUTPUTS
% dte      date

if nargin < 6, form=-1; end
if nargin < 5, startdy=1; end
if nargin < 4, startmo=1; end
if nargin < 3, startyr=1992; end
if nargin < 2, deltat=1200; end
if nargin < 1, error('please specify time step number'); end

dte=datestr(datenum(startyr,startmo,startdy)+ts*deltat/60/60/24,form);
