nx=128;ny=64;

x=[0.5:nx]/nx*360;
y=[0.5:ny]/ny*180-90;

frame=0;
 for t=0:8:288;
 frame=frame+1;

 sf=sprintf('%10.10i',t);
 P=rdmds(['Eta.' sf]);

 Q=P;

 plot(y,Q');

%pcol(x,y,Q');colorbar
%cax=caxis;
%caxis([-1 1]*max(abs(max(cax)),abs(min(cax))));colorbar

 title(sprintf('Time = %4.1f hours',t*450/3600))
 drawnow

%print('-djpeg60','-r60',sprintf('splash-%3.3i.jpg',frame));
end
