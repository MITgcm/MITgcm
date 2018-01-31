% m-file: mit_timesteps.m
% sets up the the variables tname and timesteps for the 4x4 global runs

deltat = mit_getparm('data','deltaTtracer');
if isempty(deltat)
  error('deltaTtracer is empty')
end

if strcmp(dname,'baseline.000')
  timesteps = [0:11]'*36e3; % one hundred years
  timesteps = [timesteps; [12:30]'*72e3]; % one hundred years (baseline experiment)
elseif strcmp(dname,'etopo5.000')
  timesteps = [0:30]'*72e3; % one hundred years (etopo5 experiments)
else
  if meanfields
    [dummy, timesteps] = rdmds('uVeltave',NaN);
    if isempty(timesteps)
      meanfields=0;
      error('There are no averaged fields, use meanfields=0')
    end
    timesteps = [0 timesteps];
  else
    [dummy, timesteps] = rdmds('U',NaN);
  end
  timesteps = timesteps';
  clear dummy
end

nt = length(timesteps);
kt = 1:nt;
tname = cell(nt,1);
for k = kt
  tname{k} = sprintf('%010i',timesteps(k));
end

%% alternative (unflexible way)
% $$$   timesteps = [0:11]'*36e3; % one hundred years
% $$$   timesteps = [timesteps; [12:30]'*72e3]; % one hundred years (baseline experiment)
% $$$ clear timesteps tname kt nt
% $$$ timesteps = [0:30]'*72e3; % one hundred years (etopo5 experiments)
% $$$ %tincr = 20; timesteps = [0:36]'*20; % one hundred years
% $$$ nt = length(timesteps);
% $$$ kt = 1:nt;
% $$$ tname = cell(nt,1);
% $$$ for k = kt
% $$$   tname{k} = sprintf('%010i',timesteps(k));
% $$$ end

% create a time axis
oneday = 3600*24;
onemonth =oneday*30;
oneyr=onemonth*12;
msg_spinup = dir('spinup.tim');
if isempty(msg_spinup)
  tim = deltat*timesteps';
else
  tim = load('spinup.tim');
  itim = find(diff(tim) == 0);
  tim(itim) = [];
end
% create reasonable unit;
if max(tim(:))/oneday <= 360
  tim = tim/oneday;
  timeunit = 'days';
  tuname = 'day';
elseif max(tim(:))/onemonth <= 120
  tim = tim/onemonth;
  timeunit = 'months';
  tuname = 'month';
else 
  tim = tim/oneyr;
  timeunit = 'yrs';
  tuname = 'year';
end

if ~exist('kmax','var')
  kmax=max(kt);
end
disp(['kmax = ' num2str(kmax) ', diplayed time = ' ...
      num2str(tim(kmax)) ' ' timeunit])

