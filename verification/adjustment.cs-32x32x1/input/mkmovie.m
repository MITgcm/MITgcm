load GRID

nx=32;olx=5;
j=0;

cax=[1 1];
frame=0;
%for t=48:48:192;
 for t=0:8:288;
%for t=0:8:64;
 frame=frame+1;

 sf=sprintf('%10.10i',t);
 P=read_tiles(['Eta.' sf],nx*[1 1]);
%T=read_tiles(['T.' sf],nx*[1 1]);
%S=read_tiles(['S.' sf],nx*[1 1]);
%W=read_tiles(['W.' sf],nx*[1 1]);
%GT=read_tiles(['GT.' sf],nx*[1 1]);

 Q=P;

%displaytiles(Q);

%plotcube(XG,YG,ZG,Q)
%axis square
%axis off
%view(130+0*t/256*360,20+0*sin(t/256*pi));
%axis([-1 1 -1 1 -1 1]*sqrt(2/3))
%cax=caxis;
%caxis([-1 1]*max(abs(max(cax)),abs(min(cax))))

 merccube(lonG,latG,Q);colorbar
 cax=caxis;
 caxis([-1 1]*max(abs(max(cax)),abs(min(cax))));colorbar

 title(sprintf('Time = %4.1f hours',t*450/3600))
 drawnow

%print('-djpeg60','-r60',sprintf('splash-%3.3i.jpg',frame));
end
