%function h=llc_pcol(t,fldname,iter)
%function h=llc_pcol(t,fldname,iter,'sphere')
%function h=llc_pcol(t,fldname,iter,'m_map arguments')
% with default iter = 1
%function h=llc_pcol(t,fldname)
%function h=llc_pcol(t,fldname,[],'m_map arguments')
% 
% plots 2D scalar fields v on the MITgcm llc (lat-lon-cap) grid with pcolor.
% The structure of tiles is assumed to be created by rdnctiles, e.g.,
% t = rdnctiles({'grid.*','state.*'},{'XG','YG','Eta'},[],'bytile',[]);
% see help rdnctiles for details 
% (addpath ${mitgcm_rootdir}/utils/matlab/gmt) 
% t must contain XG,YG and the field to be plotted as specified in fldname
%
% The optional flag sphere results in a 3D visualization on the sphere
% without any specific projection. Good for debugging.
%
% llc_pcol return a vector h of patch handles.
% 
% If present 'm_map argurments' are passed directly to m_proj to select
% any projection that m_map allows, then m_grid is called without any
% options (requires m_map, obviously). In order to control the input of
% m_grid, use m_ungrid and the m_grid with proper arguments, e.g.
% h=llc_pcol(x,y,v,'stereo','lon',0,'lat',80,'rad',60)
% plots a stereographic map with center at (0E,80N) and a radius of 60
% degrees as in: m_proj('stereo','lon',0,'lat',80,'rad',60); use
% m_ungrid; m_grid('box','off')
% to turn off the box around the plot.
% If you want coast lines, you can add them with m_coast after calling
% llc_pcol.
% Unfortunatly, cylindrical and conic maps are limited to the [-180 180]
% range. 
function h=llc_pcol(varargin)

  zlevel = 1;
  ph = [];
  holdstatus=ishold;
  if ~holdstatus; cla; end
  t      =varargin{1};
  if nargin < 2
    error('need at least tile-structure and fldname as input')
  end
  fldname=varargin{2}; 
  if nargin < 3
    iter = 1;
  else
    iter=varargin{3};
    if isempty(iter); iter = 1; end
  end
  % handle map projections
  mapit = 0;
  noproj= 0;
  if nargin > 3
    proj=varargin{4};
    if strcmp('sphere',proj)
      noproj = 1;
    else
      mapit = 1;
    end
  end
  if mapit
    m_proj(varargin{4:end});
  end
  % number of tiles
  ntiles = length(t);
  % loop over tiles
  if noproj
    % no projection at all
    for itile = 1:length(t)
      xc = getfield(t(itile).var,'XG')*pi/180;
      yc = getfield(t(itile).var,'YG')*pi/180;
      fld = getfield(t(itile).var,fldname);
      if ndims(fld) == 3;
	fld = fld(1:size(xc,1)-1,1:size(yc,2)-1,iter);
      else
	fld = fld(1:size(xc,1)-1,1:size(yc,2)-1,zlevel,iter);
      end
      [x,y,z] = sph2cart(xc,yc,1);
      ph = [ph;surf(x,y,z,sq(fld))];	    
      view(3)
      if (itile == 1); hold on; end
    end
    set(ph,'edgecolor','none')
    shading flat
    axis(gca,'image')
    view(gca,3)
  else
    overlap = 30;
    for itile = 1:length(t)
      fld = getfield(t(itile).var,fldname);
      xc = getfield(t(itile).var,'XG');
      yc = getfield(t(itile).var,'YG');
      if ndims(fld) == 3;
	fld = fld(1:size(xc,1)-1,1:size(yc,2)-1,iter);
      else
	fld = fld(1:size(xc,1)-1,1:size(yc,2)-1,zlevel,iter);
      end
      % divide tile into two to avoid 360 degree jump
      ic{1}=find(xc(:)>+overlap);
      ic{2}=find(xc(:)<-overlap);
      if mapit; [xc yc]=m_ll2xy(xc,yc,'clipping','on'); end
      %    if itile==9; keyboard; end
      for k=1:2
	tfld = fld([1:end 1],[1:end 1]); tfld(ic{k}) = NaN;
	x = xc; x(ic{k}) = NaN; y = yc; y(ic{k}) = NaN;
	if sum(~isnan(x(:))) > 0
	  ph = [ph;pcolor(x',y',sq(tfld)')];
	end
	if (k==1 & itile == 1); hold on; end
      end
      % now do this again with extensions above 180 and below -180 degrees
      xc = getfield(t(itile).var,'XG');
      yc = getfield(t(itile).var,'YG');
      im=find(xc<0); xc(im) = xc(im) + 360;
      ic{1}=find(xc(:)    >+overlap+180);
      ic{2}=find(xc(:)-360<-overlap-180);
      if mapit; [xc yc]=m_ll2xy(xc,yc,'clipping','on'); end
      for k=1:2
	x = xc-(k-1)*360;
	if mapit; x = xc-(k-1)*2*pi; end
	y = yc;
	tfld = fld([1:end 1],[1:end 1]); tfld(ic{k}) = NaN;
	x(ic{k}) = NaN;
	y(ic{k}) = NaN;
	if sum(~isnan(x(:))) > 0
	  ph = [ph;pcolor(x',y',sq(tfld)')];
	end
%    axis image; shading flat; pause
      end
    end 
    axis(gca,'image')
    set(gca,'box','on','layer','top')
    set(ph,'edgecolor','none')
    if mapit
      m_grid
    else
      axis([-2 2 -1 1]*90)
    end
  end %if
  hold off
  if holdstatus; hold on; end

  if nargout > 0
    h = ph;
  end
  
  return
