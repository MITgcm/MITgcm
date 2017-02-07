function [newmask,iMask,XV,YV]=set_mask(x,y,mask,val)

% [newmask,iMask,XV,YV]=set_mask(x,y,mask,val)
% GUI to create mask by hand. Idea is to set masking area 
% by selecting polygon(s) or selecting single points. 
% Area enclosed by the polygon or the selected point is 
% set to value 'val'. 
% Press 'v' or 'o' to begin selecting a polygon. 
% If you want to mark a single point, place cursor over that point 
% and press 'm'. 
% If you pressed 'v' or 'o', click as many times as needed to 
% pick the vertices of a polygon.
% When finished picking points, hit 'enter'. 
% (It is not necessary to close the polygon. This will be 
% done automatically.) Points with a value of '1' (ocean) are set 
% to a value of 'val'. Thus land points (NaN) and points 
% corresponding to other masks i.e., values ~= 1 (displayed in 
% different colors) will not be touched. HOWEVER, if you 
% pressed 'o', the latter points will be OVERWRITTEN. 
% If you pressed 'm', the selected point is set to value 'val', 
% but ONLY IF its original value was '1' (ocean). 
% So you can call this 
% function multiple times with a different value of 'val', and 
% set a different mask each time (without overwriting what was 
% set during a previous call).
% Once the polygon has been selected and the values set, the 
% plot will update. You can then pick another polygon or select 
% another point by hitting 'v'/'o' or 'm'.
% To end, press 'e'.
% To undo the last step, press 'u'.
% Outputs: 
%  newmask: the modified mask
%  iMask: vector of indices of points that were modified
%  XV,YV: cell arrays containing the vertices of polygons selected.
%         Thus, the action of this script can be reproduced in two ways:
%         (1) newmask=mask; 
%             newmask(iMask)=val;
%         (2) newmask=mask; [Xp,Yp]=ndgrid(x,y);
%             for iv=1:length(XV)
%               in=inpolygon(Xp,Yp,XV{iv},YV{iv});
%               ii=find(newmask==1 & in==1);
%               newmask(ii)=val;
%             end
%         (1) is most robust. (2) will not reproduce the action of 
%         selecting single points, but it will allow you to set masks 
%         defined on a different grid.

if val==0 | val==1
  error('Cannot set mask value to 0 or 1')
end

newmask=mask;
iNaNs=find(isnan(newmask));
newmask(iNaNs)=0;
[Xp,Yp]=ndgrid(x,y);

% pcolor(x,y,newmask');
% set(gcf,'renderer','zbuffer'); % hack to deal with disappearing crosshairs
refresh_plot(x,y,newmask)

iv=1;
iMask=[];
XV=[];
YV=[];
but='v';
while (~strcmp(but,'e'))
  [x1,y1,but]=ginput(1);
  but=char(but);
  if (strcmp(but,'v') | strcmp(but,'o'))  % select polygon
    xv=[];yv=[]; but1=but;
    while (~strcmp(but1,''))
      [x1,y1,but1]=ginput(1);
      but1=char(but1);
      xv=[xv;x1];yv=[yv;y1];
      add_point(xv,yv)
    end
    xv=[xv;xv(1)]; yv=[yv;yv(1)];
    XV{iv}=xv; YV{iv}=yv; iv=iv+1;
    in=inpolygon(Xp,Yp,xv,yv);
    if (strcmp(but,'v'))
%     Only select water points and points in polygon. Points belonging to 
%     other masks (different colors/values~=val) are not touched.
      lastBut='v';
      ii=find(newmask==1 & in==1);
    else
%     Only select water points and points in polygon. Points belonging to 
%     other masks (different colors/values~=val) may be overwritten.
%       ii=find(~isnan(newmask) & in==1);
      lastBut='o';
      ii=find(newmask~=0 & in==1);
    end
    oldmask=newmask; % in case we want to undo
    newmask(ii)=val;
    iMask=[iMask;ii];
%     pcolor(x,y,newmask');
%     set(gcf,'renderer','zbuffer'); % hack to deal with disappearing crosshairs
    refresh_plot(x,y,newmask,XV,YV)
  elseif (strcmp(but,'m'))
    lastBut='m';
    [m,im]=min(abs(x1-x));
    [m,jm]=min(abs(y1-y));
%   only mark water points    
    if newmask(im,jm)==1
      ii=sub2ind(size(newmask),im,jm);
      oldmask=newmask; % in case we want to undo
      newmask(ii)=val;
      iMask=[iMask;ii];
      refresh_plot(x,y,newmask)
    end    
  elseif (strcmp(but,'u'))  % undo
    nadd=length(ii); % number of points added
    disp('Undoing last step ...')
    newmask=oldmask; % reset previous values
    iMask=iMask(1:end-nadd); % delete mask indices
    if lastBut~='m'
      tmpXV=XV; tmpYV=YV; % delete polygon vertices. is there an easy way to do this?
      clear XV YV
      for k=1:length(tmpXV)-1
        XV{k}=tmpXV{k};
        YV{k}=tmpYV{k};
      end
      iv=iv-1;
%     pcolor(x,y,newmask');
%     set(gcf,'renderer','zbuffer'); % hack to deal with disappearing crosshairs    
    end
    refresh_plot(x,y,newmask,XV,YV)
  end
end  

newmask(iNaNs)=NaN;

function refresh_plot(x,y,mask,XV,YV)

% pcolor(x,y,mask');
% shading interp
imagesc(x,y,mask')
axis xy
% set(gcf,'renderer','zbuffer'); % hack to deal with disappearing crosshairs

if nargin>3
  hold on
  for k=1:length(XV)
    plot(XV{k},YV{k},'k-o')
  end
  hold off
end

function add_point(x,y)

hold on
plot(x,y,'k-o')
hold off
