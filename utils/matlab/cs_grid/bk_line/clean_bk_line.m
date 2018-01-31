function [ncut,icut,xcut,ycut,misfit,xyfit]=clean_bk_line( ...
         nf1,nf2,nc,ydim,yl,dylat,xMid,xx1,xx2,yy2, ...
         savI,savJ,savF,isav,jsav,xsav,nMx6t)

%- define "segments" = continuous part of the line : --> i,x,y_cut(:,1:2,:)

ncut=zeros(6,1); icut=zeros(nc,6,6); xcut=zeros(nc,4,6); ycut=zeros(nc,4,6);

%- cut unused part of the line :
for n=nf1:nf2, if nMx6t(n) > 1,
  iEnd=nMx6t(n); is=iEnd+1 ; ie=0;
  for i=2:iEnd,
   if savF(i-1,n) ~= 0, ie=i ; is=min(is,i-1); end
   if ie ~= 0 & ( savF(i-1,n) == 0 | i == iEnd ) , 
     ncut(n)=ncut(n)+1 ; in=ncut(n);
     icut(in,1,n)=is ; icut(in,2,n)=ie ;
     xcut(in,1,n)=xsav(is,n) ; 
     ycut(in,1,n)=yy2(isav(is,n),jsav(is,n),n) ; 
     xcut(in,2,n)=xsav(ie,n) ; 
     ycut(in,2,n)=yy2(isav(ie,n),jsav(ie,n),n) ; 
     is=iEnd+1 ; ie=0;
   end
  end
end; end ;

%========================================================================
%- detect fork and try to remove 1 branche : fill in icut(:,3:4,:)

misfit=0; xyfit = 0;
for n=nf1:nf2,
 for in=1:ncut(n),
%-------------------------------------------------------------------------
  if icut(in,4,n) == 0,
    xyfit = 0;
    xnloc=xcut(in,2,n) ;
    ynloc=ycut(in,2,n) ;

%- check if segment ncut(n) stop somewhere in an other segment
  for p=nf1:nf2, if p ~= n, jj=0;
   for ip=1:ncut(p), if jj == 0 & xnloc > xcut(ip,1,p) & xnloc < xcut(ip,2,p),
     JJ=find(xsav(:,p) == xnloc) ;
     for j=1:length(JJ),
      yploc=yy2(isav(JJ(j),p),jsav(JJ(j),p),p);
      if ynloc == yploc & savF(JJ(j),p) ~= 0, jj=JJ(j) ; end
     end
     if jj ~= 0,
       yploc=yy2(isav(jj,p),jsav(jj,p),p); 
       ypm1=yy2(isav(jj-1,p),jsav(jj-1,p),p);
       ynm1=yy2(isav(icut(in,2,n)-1,n),jsav(icut(in,2,n)-1,n),n);
      if abs(ypm1-yl) >= abs(ynm1-yl) & ...
       ( rem(isav(jj-1,p),nc) == 1 | rem(jsav(jj-1,p),nc) == 1 ),
       if icut(in,4,n) == 0, icut(in,4,n) = icut(in,2,n); xyfit=xyfit-1;
       else
        misfit=misfit+1; fprintf(['Multiple C n,in= %i %i ;', ...
                 '  Stop : icut_2,4,xcut,ycut= %i %i %8.3f %8.3f \n'], ...
                  n,in,icut(in,2,n),icut(in,4,n),xnloc,ynloc);
        fprintf('       in: p,ip,jj,icut= %i %i %i %i %i %8.3f %8.3f \n',...
                  p,ip,jj,icut(ip,1,p),icut(ip,2,p),xcut(ip,1,p),xcut(ip,2,p) );
       end 
%----------------------
       ie = jj-1 ; is=icut(ip,3,p); if is == 0, is=icut(ip,1,p); end
       flag=0; 
       if is < ie, yloc=zeros(1+ie-is,1) ;
         for i=is:ie, yloc(1+i-is)=yy2(isav(i,p),jsav(i,p),p)-yl;
           if rem(isav(i,p),nc) ~= 1 & rem(jsav(i,p),nc) ~= 1, flag=1 ; end
         end
         if min(yloc)*max(yloc) < 0, flag=1; end;
       end
       if flag == 1,
%-- cut the segment (p,ip) in 2 parts :
         ip2=ncut(p)+1; icut(ip2,:,p)=icut(ip,:,p);
         xcut(ip2,:,p)=xcut(ip,:,p); ycut(ip2,:,p)=ycut(ip,:,p);
         icut(ip2,2,p)=ie ; icut(ip2,4,p)=0 ;
         xcut(ip2,2,p)=xsav(ie,p) ; ycut(ip2,2,p)=ypm1 ;
         icut(ip,1,p)=jj ; icut(ip,3,p)=jj ;
         xcut(ip,1,p)=xnloc; ycut(ip,1,p)=ynloc ; 
         ncut(p)=ip2; xyfit=xyfit-1;
         if ydim == 1, fprintf([' p= %i ; cut_3 ip= %i in %3i %8.3f %8.3f', ...
            ' ; + add ip= %i \n'],p,ip,jj,xnloc,ynloc,ip2); end;
%---
       elseif icut(ip,3,p) == 0, icut(ip,3,p) = jj; xyfit=xyfit-1;
       else
        misfit=misfit+1; fprintf(['Multiple C n,in= %i %i ;', ...
                 '  stop : icut,xcut,ycut= %i %8.3f %8.3f \n'], ...
                  n,in,icut(in,2,n),xnloc,ynloc);
        fprintf(['       in: p,ip,jj,icut_312= %i %i %i %i %i %i', ...
                 ' %8.3f %8.3f \n'], p,ip,jj, ...
          icut(ip,3,p),icut(ip,1,p),icut(ip,2,p),xcut(ip,1,p),xcut(ip,2,p) );
       end 
%----------------------
      end
     end 

   end; end;	%-- for ip , if xnloc
  end; end;	%-- for p , if p ~= n ,
  if ydim == 1 & xyfit ~= -2,
    fprintf(['n= %i fit= %i ;  stop at: in,i,j= %i %2i %2i ;', ...
                       ' X,Y= %8.3f %8.3f \n'], ...
        n,xyfit, in,isav(icut(in,2,n),n),jsav(icut(in,2,n),n),xnloc,ynloc);
  end;

  end	%-- if icut(in,4,n) == 0
%-------------------------------------------------------------------------

%-------------------------------------------------------------------------
  if icut(in,3,n) == 0,
    xyfit = 0;
    xnloc=xcut(in,1,n) ;
    ynloc=ycut(in,1,n) ;

%- check that segment ncut(n) start somewhere in an other segment
  for p=nf1:nf2, if p ~= n, jj=0;
   for ip=1:ncut(p), if jj == 0 & xnloc > xcut(ip,1,p) & xnloc < xcut(ip,2,p),
     JJ=find(xsav(:,p) == xnloc) ;
     for j=1:length(JJ),
      yploc=yy2(isav(JJ(j),p),jsav(JJ(j),p),p);
      if ynloc == yploc & savF(JJ(j)-1,p) ~= 0, jj=JJ(j) ; end
     end
     if jj ~= 0,
       yploc=yy2(isav(jj,p),jsav(jj,p),p); 
       ypp1=yy2(isav(jj+1,p),jsav(jj+1,p),p);
       ynp1=yy2(isav(icut(in,1,n)+1,n),jsav(icut(in,1,n)+1,n),n);
      if abs(ypp1-yl) >= abs(ynp1-yl) & ...
       ( rem(isav(jj+1,p),nc) == 1 | rem(jsav(jj+1,p),nc) == 1 ),
       if icut(in,3,n) == 0, icut(in,3,n) = icut(in,1,n); xyfit=xyfit-1;
       else
        misfit=misfit+1; fprintf(['Multiple C n,in= %i %i ;', ...
                 ' Start : icut_1,3,xcut,ycut= %i %i %8.3f %8.3f \n'], ...
                  n,in,icut(in,1,n),icut(in,3,n),xnloc,ynloc);
        fprintf('       in: p,ip,jj,icut= %i %i %i %i %i %8.3f %8.3f \n',...
                  p,ip,jj,icut(ip,1,p),icut(ip,2,p),xcut(ip,1,p),xcut(ip,2,p) );
       end 
%----------------------
       is = jj+1 ; ie=icut(ip,4,p); if ie == 0, ie=icut(ip,2,p); end
       flag=0; 
       if is < ie, yloc=zeros(1+ie-is,1) ;
         for i=is:ie, yloc(1+i-is)=yy2(isav(i,p),jsav(i,p),p)-yl;
           if rem(isav(i,p),nc) ~= 1 & rem(jsav(i,p),nc) ~= 1, flag=1 ; end
         end
         if min(yloc)*max(yloc) < 0, flag=1; end;
       end
       if flag == 1,
%-- cut the segment (p,ip) in 2 parts :
         ip2=ncut(p)+1; icut(ip2,:,p)=icut(ip,:,p);
         xcut(ip2,:,p)=xcut(ip,:,p); ycut(ip2,:,p)=ycut(ip,:,p);
         icut(ip2,1,p)=is ; icut(ip2,3,p)=0 ;
         xcut(ip2,1,p)=xsav(is,p) ; ycut(ip2,1,p)=ypp1 ;
         icut(ip,2,p)=jj ; icut(ip,4,p)=jj ;
         xcut(ip,2,p)=xnloc; ycut(ip,2,p)=ynloc ; 
         ncut(p)=ip2; xyfit=xyfit-1;
         if ydim == 1, fprintf([' p= %i ; cut_4 ip= %i in %3i %8.3f %8.3f', ...
            ' ; + add ip= %i \n'],p,ip,jj,xnloc,ynloc,ip2); end;
%---
       elseif icut(ip,4,p) == 0, icut(ip,4,p) = jj; xyfit=xyfit-1;
       else
        misfit=misfit+1; fprintf(['Multiple C n,in= %i %i ;', ...
                 ' start : icut,xcut,ycut= %i %8.3f %8.3f \n'], ...
                  n,in,icut(in,1,n),xnloc,ynloc);
        fprintf(['       in: p,ip,jj,icut_412= %i %i %i %i %i %i', ...
                 ' %8.3f %8.3f \n'], p,ip,jj, ...
          icut(ip,4,p),icut(ip,1,p),icut(ip,2,p),xcut(ip,1,p),xcut(ip,2,p) );
       end 
%----------------------
      end
     end 

   end ; end;	%-- for ip , if xnloc
  end; end;	%-- for p , if p ~= n ,
  if ydim == 1 & xyfit ~= -2,
    fprintf(['n= %i fit= %i ; start at: in,i,j= %i %2i %2i ;', ...
                       ' X,Y= %8.3f %8.3f \n'], ...
        n,xyfit, in,isav(icut(in,1,n),n),jsav(icut(in,1,n),n),xnloc,ynloc);
  end

  end	%-- if icut(in,3,n) == 0
%-------------------------------------------------------------------------

end; end ; %- for n, for in=1:ncut(n)

if misfit > 0, error(['misfit (',int2str(misfit),') in starting point']) ; end

%=========================================================================

%- check if segments are conected :
%  --> icut(:,3:4,:) & icut(:,5,:)= index of the next segment

for n=nf1:nf2, for in=1:ncut(n),
%- fill in x,y_cut(3,4) :
 if icut(in,3,n) ~= 0, xcut(in,3,n)=xsav(icut(in,3,n),n);
  ycut(in,3,n)=yy2(isav(icut(in,3,n),n),jsav(icut(in,3,n),n),n); end
 if icut(in,4,n) ~= 0, xcut(in,4,n)=xsav(icut(in,4,n),n);
  ycut(in,4,n)=yy2(isav(icut(in,4,n),n),jsav(icut(in,4,n),n),n); end

 if ydim == 0,
  fprintf(' n,in ; icut,xcut,ycut(1 -> 4) :\n');
  fprintf('n,in= %i %i',n,in);
  fprintf(' ; %3i %7.2f %7.2f',icut(in,1,n),xcut(in,1,n),ycut(in,1,n));
  fprintf(' ; %3i %7.2f %7.2f',icut(in,2,n),xcut(in,2,n),ycut(in,2,n));
  fprintf('\n'); fprintf('   --->  ');
  fprintf(' ; %3i %7.2f %7.2f',icut(in,3,n),xcut(in,3,n),ycut(in,3,n));
  fprintf(' ; %3i %7.2f %7.2f',icut(in,4,n),xcut(in,4,n),ycut(in,4,n));
  fprintf(' |=> %i %3i ',rem(icut(in,5,n),7),fix(icut(in,5,n)/7));
  fprintf('\n');
 end;
end; end ; %- for n, for in=1:ncut(n)


misfit=0;
for n=nf1:nf2,
 for in=1:ncut(n),
%-------------------------------------------------------------------------

  if icut(in,5,n) == 0,
    xyfit = 0;
    ie=4 ; if icut(in,4,n) == 0, ie=2; end; 
    xnloc=xcut(in,ie,n) ; ynloc=ycut(in,ie,n) ;

%- check that segment ncut(n) stop where some segment ncut(p) start    
   for p=nf1:nf2,
    for ip=1:ncut(p),
     is=3 ; if icut(ip,3,p) == 0, is=1; end
     xploc=xcut(ip,is,p) ; yploc=ycut(ip,is,p) ;
     ddx = xploc - xnloc + 360 ; ddx=rem(ddx,360) ;
     if ddx == 0 & ynloc == yploc, 
      if icut(in,5,n) == 0, icut(in,5,n)=p+ip*7 ;
      else
       misfit=misfit+1; fprintf(['Double C n,in,ie= %i %i %i;', ...
              '  Stop : icut_5: %i %2i ; p,ip,is= %i %i %i \n'],...
             n,in,ie,rem(icut(in,5,n),7),fix(icut(in,5,n)/7),p,ip,is);
      end
      if ie == 4 & is == 3, xyfit=xyfit+2;
      elseif ie == 2 & is == 1, xyfit=xyfit+2;
       icut(in,4,n) = icut(in,2,n);
       icut(ip,3,p) = icut(ip,1,p);
      else 
       misfit=misfit+1; fprintf(['strange C n,in,ie= %i %i %i;', ...
                '  Stop : icut,xcut,ycut= %i %8.3f %8.3f ;', ...
                ' p,ip,is,icut= %i %i %i %i \n'],...
             n,in,ie,icut(in,ie,n),xnloc,ynloc,p,ip,is,icut(ip,is,p) );
      end 
     end
   end; end;	%-- for p , for ip
   if ydim == 1 & xyfit ~= 2,
    fprintf(['n= %i fit= %i ;  Stop at: in,ie,i,j= %i %i %i %i ;', ...
                        ' X,Y= %8.3f %8.3f \n'], ...
       n,xyfit, in,ie,isav(icut(in,ie,n),n),jsav(icut(in,ie,n),n),xnloc,ynloc);
   end

  end;		%-- if icut(in,4,n) == 0
%-------------------------------------------------------------------------

end; end ; %- for n, for in=1:ncut(n)

if misfit > 0, error(['misfit (',int2str(misfit),') in ending point']) ; end

%-- this part is not necessary :
misfit=0;
for n=nf1:nf2,
 for in=1:ncut(n),
%-------------------------------------------------------------------------

  if icut(in,3,n) == 0,
    xyfit = 0; is=1; 
    xnloc=xcut(in,is,n) ; ynloc=ycut(in,is,n) ;

%- check that segment ncut(n) start where some segment ncut(p) stop    
   for p=nf1:nf2,
    for ip=1:ncut(p),
     ie=4 ; if icut(ip,4,p) == 0, ie=2; end
     xploc=xcut(ip,ie,p) ; yploc=ycut(ip,ie,p) ;
     ddx = xploc - xnloc + 360 ; ddx=rem(ddx,360) ;
     if ddx == 0 & ynloc == yploc, 
      ipp=icut(ip,5,p);
      if ipp == 0, icut(ip,5,p)=n+in*7 ;
      elseif n+in*7 ~= ipp ,
       misfit=misfit+1; fprintf(['Double C n,in,is= %i %i %i;', ...
              ' Start : p,ip,ie= %i %i %i ; icut_5: %i %2i\n'],...
             n,in,is,p,ip,ie,rem(ipp,7),fix(ipp/7));
      end
      if is == 3 & ie == 4, xyfit=xyfit+2;
      elseif is == 1 & ie == 2, xyfit=xyfit+2;
       icut(in,3,n) = icut(in,1,n);
       icut(ip,4,p) = icut(ip,2,p);
      else 
       misfit=misfit+1; fprintf(['strange C n,in,is= %i %i %i;', ...
                ' Start : icut,xcut,ycut= %i %8.3f %8.3f ;', ...
                ' p,ip,ie,icut= %i %i %i %i \n'],...
             n,in,is,icut(in,is,n),xnloc,ynloc,p,ip,ie,icut(ip,ie,p) );
      end 
     end
   end; end;	%-- for p , for ip
   if ydim == 1 & xyfit ~= 2,
    fprintf(['n= %i fit= %i ; Start at: in,is,i,j= %i %i %i %i ;', ...
                        ' X,Y= %8.3f %8.3f \n'], ...
       n,xyfit, in,is,isav(icut(in,is,n),n),jsav(icut(in,is,n),n),xnloc,ynloc);
   end;

  end;		%-- if icut(in,.3,n) == 0
%-------------------------------------------------------------------------

end; end ; %- for n, for in=1:ncut(n)

if misfit > 0, error(['misfit (',int2str(misfit),') in connection']) ; end

%=========================================================================

%-------------------------------------------------------------------------
%-- Clean up the segment list :
for n=nf1:nf2, for in=1:ncut(n),

%- fill in x,y_cut(3,4) :
 if icut(in,3,n) ~= 0, xcut(in,3,n)=xsav(icut(in,3,n),n);
  ycut(in,3,n)=yy2(isav(icut(in,3,n),n),jsav(icut(in,3,n),n),n); end
 if icut(in,4,n) ~= 0, xcut(in,4,n)=xsav(icut(in,4,n),n);
  ycut(in,4,n)=yy2(isav(icut(in,4,n),n),jsav(icut(in,4,n),n),n); end

if ydim == 1,
  fprintf(' n,in ; icut,xcut,ycut(1 -> 4) :\n');
  fprintf('n,in= %i %i',n,in);
  fprintf(' ; %3i %7.2f %7.2f',icut(in,1,n),xcut(in,1,n),ycut(in,1,n));
  fprintf(' ; %3i %7.2f %7.2f',icut(in,2,n),xcut(in,2,n),ycut(in,2,n));
  fprintf('\n'); fprintf('   --->  ');
  fprintf(' ; %3i %7.2f %7.2f',icut(in,3,n),xcut(in,3,n),ycut(in,3,n));
  fprintf(' ; %3i %7.2f %7.2f',icut(in,4,n),xcut(in,4,n),ycut(in,4,n));
  fprintf(' |=> %i %3i ',rem(icut(in,5,n),7),fix(icut(in,5,n)/7));
  fprintf('\n');
end

%-------------------------
if icut(in,6,n) == 0, 

  flag=0; xcirc = 0; ipp=n+7*in;
  while ipp > 0,
   p=rem(ipp,7); ip=fix(ipp/7);
   is=icut(ip,3,p); if is == 0, is=icut(ip,1,p); end
   ie=icut(ip,4,p); if ie == 0, ie=icut(ip,2,p); end
   for i=is:ie,
    if rem(isav(i,p),nc) ~= 1 & rem(jsav(i,p),nc) ~= 1, flag=1 ; end
   end
   xcirc = xcirc + xsav(ie,p) - xsav(is,p) ; 
   ipp = icut(ip,5,p) ; if xcirc >= 360, ipp=0; end
  end ; ippEnd = icut(ip,5,p) ;

%-- if some valid Pts but a) not full circle or b) not connected: ==> misfit
  if flag == 1 & ( xcirc < 360 | icut(in,5,n) == 0 ), 
   misfit=misfit+1;
   fprintf('yl= %8.3f ;  WARNING: not full circle: %12.6f \n',yl,xcirc);
   ipp=n+7*in;
   while ipp > 0,
    p=rem(ipp,7); ip=fix(ipp/7);
    is=3; if icut(ip,3,p) == 0, is=1; end
    ie=4; if icut(ip,4,p) == 0, ie=2; end
    fprintf('p,ip= %i %i (is,e=%i,%i)',p,ip,is,ie);
    fprintf(' ; %3i %7.2f %7.2f',icut(ip,is,p),xcut(ip,is,p),ycut(ip,is,p));
    fprintf(' ; %3i %7.2f %7.2f',icut(ip,ie,p),xcut(ip,ie,p),ycut(ip,ie,p));
    fprintf(' (%i) |=> %i %3i\n',icut(ip,6,p), ...
                       rem(icut(in,5,n),7),fix(icut(in,5,n)/7) );
    ipp = icut(ip,5,p) ;
   end
%-- if make a full circle => tag all the segments
  elseif xcirc >= 360,
   ipp=n+7*in;
   while ( ipp > 0 & ipp ~= ippEnd), 
    p=rem(ipp,7); ip=fix(ipp/7); icut(ip,6,p)=flag;
    ipp = icut(ip,5,p) ;
   end
%-- if only on the edge (flag=0) & not making a full circle ==> remove segment 
  elseif flag == 0,
   ipp=n+7*in;
   while ( ipp > 0 ), 
    p=rem(ipp,7); ip=fix(ipp/7); icut(ip,6,p)=-1;
%   icut(ip,3,p)=-2; icut(ip,4,p)=-1;
    ipp = icut(ip,5,p) ;
   end
%--
  end

end	%-- if icut(in,6,n) = 0
%-------------------------
%if icut(in,3,n) == 0 & icut(in,4,n) == 0, 
% flag=0;
% for i=icut(in,1,n):icut(in,2,n),
%  if rem(isav(i,n),nc) ~= 1 & rem(jsav(i,n),nc) ~= 1, flag=1 ; end
% end
%-- remove disconected segments ; or WARNING if not empty
% if flag == 0, icut(in,3,n)=-2; icut(in,4,n)=-1;
% else misfit=misfit+1;
%  fprintf('yl= %8.3f ;  WARNING: found Isolated segment:\n',yl);
%  fprintf('n,in= %i %i',n,in);
%  fprintf(' ; %3i %7.2f %7.2f',icut(in,1,n),xcut(in,1,n),ycut(in,1,n));
%  fprintf(' ; %3i %7.2f %7.2f',icut(in,2,n),xcut(in,2,n),ycut(in,2,n));
%  fprintf('\n');
% end
%elseif icut(in,3,n) == 0, misfit=misfit+1;
%  fprintf('yl= %8.3f ;  WARNING: isolated start :\n',yl);
%  fprintf('n,in= %i %i',n,in);
%  fprintf(' ; %3i %7.2f %7.2f',icut(in,1,n),xcut(in,1,n),ycut(in,1,n));
%  fprintf(' ; %3i %7.2f %7.2f',icut(in,4,n),xcut(in,4,n),ycut(in,4,n));
%  fprintf('\n');
%elseif icut(in,4,n) == 0, misfit=misfit+1;
%  fprintf('yl= %8.3f ;  WARNING: isolated  end  :\n',yl);
%  fprintf('n,in= %i %i',n,in);
%  fprintf(' ; %3i %7.2f %7.2f',icut(in,3,n),xcut(in,3,n),ycut(in,3,n));
%  fprintf(' ; %3i %7.2f %7.2f',icut(in,2,n),xcut(in,2,n),ycut(in,2,n));
%  fprintf('\n');
%end
%if icut(in,3,n) == 0, icut(in,3,n) = icut(in,1,n) ; 
%   xcut(in,3,n)=xcut(in,1,n); ycut(in,3,n)=ycut(in,1,n) ;
%end
%if icut(in,4,n) == 0, icut(in,4,n) = icut(in,2,n) ; 
%   xcut(in,4,n)=xcut(in,2,n); ycut(in,4,n)=ycut(in,2,n) ; 
%end
   
end; end;
%-------------------------------------------------------------------------

return
