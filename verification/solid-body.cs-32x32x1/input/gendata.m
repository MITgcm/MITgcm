for tl=1:6,
lon(:,tl,:)=rdda(sprintf('LONC.%3.3i.bin',tl),[32 32],1,'real*8','b')*pi/180;
lat(:,tl,:)=rdda(sprintf('LATC.%3.3i.bin',tl),[32 32],1,'real*8','b')*pi/180;
end
X=cos(lat).*sin(lon);Y=-cos(lat).*cos(lon);Z=sin(lat);clear lat lon

lon0=-90  *pi/180;
lat0=45   *pi/180;
xo=cos(lat0).*sin(lon0);yo=-cos(lat0).*cos(lon0);zo=sin(lat0);
ro=0.3;
R=sqrt( (X-xo).^2 + (Y-yo).^2 + (Z-zo).^2 );
s=(1+cos( pi*min(R/ro,1+0*R) ))/2;
wrda('S.init',s,1,'real*8','b');

lon0=0    *pi/180;
lat0=0    *pi/180;
xo=cos(lat0).*sin(lon0);yo=-cos(lat0).*cos(lon0);zo=sin(lat0);
ro=0.3;
R=sqrt( (X-xo).^2 + (Y-yo).^2 + (Z-zo).^2 );
h=1e4*(1+cos( pi*min(R/ro,1+0*R) ))/2; %cosine bell
%h=1e4*(1-( min(R/ro,1+0*R) ))/2;	%cone
%wrda('mountain_eq.init',h,1,'real*8','b');

lon0=0    *pi/180;
lat0=30   *pi/180;
xo=cos(lat0).*sin(lon0);yo=-cos(lat0).*cos(lon0);zo=sin(lat0);
ro=0.3;
R=sqrt( (X-xo).^2 + (Y-yo).^2 + (Z-zo).^2 );
h=1e4*(1+cos( pi*min(R/ro,1+0*R) ))/2; %cosine bell
%h=1e4*(1-( min(R/ro,1+0*R) ))/2;	%cone
%wrda('mountain_30.init',h,1,'real*8','b');
