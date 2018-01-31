function h=llc_vec(t,xfld,yfld,fspec);
%function h=llc_vecl(t,xfld,yfld,fspec)
% t - rdnctiles data structure
%    
% xfld  - field to use for x-component
% yfld  - field to use for y-component
% fspec - things to control plot region etc...
%         fspec.proj is either 'cart' or 'cyl'. 'sphere' with quiver3 is 
%         too slow to be useful on my laptop.
%         fspec.lathi high latitude value for each region in the plot
%         fspec.latlo low latitude value for each region in the plot
%         fspec.lonhi high longitude value for each region in the plot
%         fspec.lonlo low longitude value for each region in the plot
%         fspec.az azimuthal view angle
%         fspec.el elevation view angle
%         fspec.alx scale arrow x-location
%         fspec.aly scale arrow y-location
%         fspec.qscal quiver scale factor
%         fspec.skip arrow skip 1, every arrow, 2 every two arrows etc...
%         fspec.almag scale arrow length
%         fspec.xlab xlabel
%         fspec.ylab ylabel
%         Code that does the scale arrow is a bit of a hack - to say the
%         least :-).

% Make a vector plot using quiver
clf; h=[];

xvec=[]; yvec=[]; xloc=[]; yloc=[];
xp=[];yp=[];
for tn=1:length(t)
 uvec=getfield(t(tn).var,xfld );
 vvec=getfield(t(tn).var,yfld );
 xc  =getfield(t(tn).var,'XC');
 yc  =getfield(t(tn).var,'YC');
 xg  =getfield(t(tn).var,'XG');
 yg  =getfield(t(tn).var,'YG');
 % Make list of water and land points
 ikeep=[]; iland=[];
 i2land=[];j2land=[];
 for i=1:length(fspec.lathi)
  lathi=fspec.lathi(i);
  latlo=fspec.latlo(i);
  lonhi=fspec.lonhi(i);
  lonlo=fspec.lonlo(i);
  ikeep=[ikeep' find(  xc >= lonlo ...
                     & xc <= lonhi ...
                     & yc >= latlo ...
                     & yc <= lathi ...
                     & (uvec.^2+vvec.^2) ~= 0 )']';
  iland=[iland' find(  xc >= lonlo ...
                     & xc <= lonhi ...
                     & yc >= latlo ...
                     & yc <= lathi ...
                     & (uvec.^2+vvec.^2) == 0 )']';
 end
 ikeep=unique(ikeep); iland=unique(iland);
 % Extract vector values for water points
 xvec=[xvec' uvec(ikeep)']';
 yvec=[yvec' vvec(ikeep)']';
 xloc=[xloc'   xc(ikeep)']';
 yloc=[yloc'   yc(ikeep)']';
 % Create patch array for land points
 [tmpi,tmpj]=ind2sub(size(xc),iland);
 t(tn).xp=ones(length(tmpi),5);
 t(tn).yp=ones(length(tmpi),5);
 for l=1:length(tmpi)
  t(tn).xp(l,:)=[xg(tmpi(l)  , tmpj(l)  ) ...
                 xg(tmpi(l)+1, tmpj(l)  ) ...
                 xg(tmpi(l)+1, tmpj(l)+1) ...
                 xg(tmpi(l)  , tmpj(l)+1)  ...
                 xg(tmpi(l)  , tmpj(l)  )];
  t(tn).yp(l,:)=[yg(tmpi(l)  , tmpj(l)  ) ...
                 yg(tmpi(l)+1, tmpj(l)  ) ...
                 yg(tmpi(l)+1, tmpj(l)+1) ...
                 yg(tmpi(l)  , tmpj(l)+1)  ...
                 yg(tmpi(l)  , tmpj(l)  )];
 end
end

if strcmp(fspec.proj,'cyl') 
 % Do cylindrical plot (would be nice to use quiver3, but its very slow)
 [xloc_sp, yloc_sp, zloc_sp]=sph2cart(deg2rad(xloc+360),deg2rad(yloc),6.4e3);
 ca=cos(deg2rad(xloc+360)); sa=sin(deg2rad(xloc+360));
 if fspec.el == -90
  yvec=-yvec;
 end
 xvec_sp=-sa.*xvec-ca.*yvec; yvec_sp=+ca.*xvec-sa.*yvec;
 % Add reference arrow
 xloc_sp(end+1)=fspec.alx; 
 yloc_sp(end+1)=fspec.aly; 
 xvec_sp(end+1)=fspec.axmag;
 yvec_sp(end+1)=fspec.aymag;
 hq=quiver(xloc_sp,yloc_sp,xvec_sp,yvec_sp,fspec.scal);
 h=[h;hq];
 axis equal;axis tight
 hold on
 for tn=1:length(t)
  xp=t(tn).xp; yp=t(tn).yp;
  [xp_sp,yp_sp,zp_sp]=sph2cart(deg2rad(xp+360),deg2rad(yp+360),6.4e3);
  hp=patch(xp_sp',yp_sp',3);h=[h;hp];
  c=[0.6 0.6 0.6];
  e=[  0   0   0];
  set(hp,'FaceColor',c,'EdgeColor',e)
  set(hp,'EraseMode','background')
 end
 xlabel('km');
 ylabel('km');
 view(fspec.az,fspec.el);
 % Add label to reference arrow
 alab=sprintf('%.1f m s^{-1}',sqrt(fspec.axmag.^2+fspec.aymag.^2));
 if fspec.axmag == 0
  text(fspec.alx+150, fspec.aly, alab);
 else
  text(fspec.alx, fspec.aly+150, alab);
 end
end

if strcmp(fspec.proj,'cart') 
 if fspec.az == 180
  xloc(find(xloc<0))=xloc(find(xloc<0))+360;
 end
 % Add reference arrow
 xloc(end+1)=fspec.alx; 
 yloc(end+1)=fspec.aly; 
 xvec(end+1)=fspec.axmag;
 yvec(end+1)=fspec.aymag;
 hold on
 for tn=1:length(t)
  xp=t(tn).xp; yp=t(tn).yp;
  if fspec.az == 180
   xp(find(xp<0))=xp(find(xp<0))+360;
  end
  hp=patch(xp',yp',3);h=[h;hp];
  c=[0.6 0.6 0.6];
  e=[  0   0   0];
  set(hp,'FaceColor',c,'EdgeColor',c)
 end
 sk=fspec.skip;
 hq=quiver(xloc(1:sk:end),yloc(1:sk:end),xvec(1:sk:end),yvec(1:sk:end),fspec.scal);
 axis equal;axis tight
 h=[h;hq];
 xlabel('degrees');
 ylabel('degrees');
 % Add label to reference arrow
 alab=sprintf('%.1f m s^{-1}',sqrt(fspec.axmag.^2+fspec.aymag.^2));
 if fspec.axmag == 0
  text(fspec.alx+1, fspec.aly, alab);
 else
  text(fspec.alx, fspec.aly+1, alab);
 end
end

return
end
