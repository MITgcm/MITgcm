x=rdmds('XC');x=x(:,1)/1e3;	% X coordinate (km)
y=rdmds('YC');y=y(1,:)/1e3;	% Y coordinate (km)
t0=rdmds('T',0);		% Initial T
dz=144*rdmds('hFacC');		% Level thicknesses
D=sum(dz,3);			% Total water depth
D=max(D,.001);			% avoid division by zero later
freq=100;			% Output frequency

figure(1)			% Create a figure window
pos=get(gcf,'Position');	% Set aspect ratio and size of image
set(gcf,'Position',[pos(1:2) 320*[1 .75]]);
set(gcf,'Color',[1 1 1]);	% White background for movie
mov=avifile('dome.avi');	% Name of AVI movie file
mov.Quality=100; mov.Fps=10;	% Quality and frames per second

[t,nits]=rdmds('T',Inf);	% Read last snap-shot
nits=120*freq;

m=t; m(find(m~=0))=1;		% Create a mask
k=max(sum(m,3),1);		% Index of bottom level
[i,j]=ndgrid(1:length(x),1:length(y));	% I,J indices
ijk=sub2ind(size(t),i,j,k);	% I,J,K indices of bottom level


for iter=freq:freq:nits;
 s=rdmds('S',iter);		% Read snap-shot

 subplot(211)
 pcolor(x,y, sq(s(ijk))' );	% Plot tracer at ocean bottom
 shading flat
 caxis([1 2])
 colorbar
 title('Tracer concentration on bottom')

 subplot(212)
 pcolor(x,y, sq( sum(s.*dz,3)./D )' );	% Plot tracer inventory
 shading flat
 caxis([1 1.6])
 colorbar
 title('Tracer inventory (vertical average)')

 drawnow
 frame=getframe(gcf); mov=addframe(mov,frame);	% Add to movie
end

mov=close(mov);			% Finish writing the movie
