% m-file: mit_plotmeandrift.m
% mean temperature and salinity drift
if ~isempty(msg_spinup)
  % extracted from the monitor output with a script by jmc
  global_mt = load('spinup.t_A');
  global_ms = load('spinup.s_A');
  global_mt(itim) = [];
  global_ms(itim) = [];
else
  % or computed `on the fly'
  global_mt = zeros(size(kt));
  global_ms = zeros(size(kt));
  for k=kt;
    if meanfields
      % read the snapshots
      tk=rdmds('T',timesteps(k));
      sk=rdmds('S',timesteps(k));
      if strcmp(grd.buoyancy,'OCEANICP')
	% turn fields upside down
	tk=tk(:,:,end:-1:1).*grd.cmask;
	sk=sk(:,:,end:-1:1).*grd.cmask;
      end
    else
      % snapshots are already available
      if iscell(t)
	tk = t{k};
      else
	tk = t(:,:,:,k);
      end
      if iscell(s)
	sk = s{k};
      else
	sk = s(:,:,:,k);
      end
    end
    global_mt(k)= nansum(tk(:).*grd.volc(:))/nansum(grd.volc(:));
    global_ms(k)= nansum(sk(:).*grd.volc(:))/nansum(grd.volc(:));
  end
end
figure
subplot(2,1,1);
plot(tim,global_mt)
title('Drift of Mean Temperature'); xlabel(['Time [' timeunit ']']); ylabel('T [degC]')
subplot(2,1,2);
plot(tim,global_ms)
title('Drift of Mean Salinity'); xlabel(['Time [' timeunit ']']); ylabel('S [PSU]')
suptitle(['experiment ' dname])

