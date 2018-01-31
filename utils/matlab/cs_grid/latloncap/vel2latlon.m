function [u_ll, v_ll] = vel2latlon( ca, sa, uvec, vvec )
% Project two orthogonal components onto another at angle phi
% where sa=sin(phi) and ca=cos(phi).

for k = 1:size(uvec,3)
  uvec_bari   = ( uvec(1:end-1,:,k) + uvec(2:end,:,k) )*0.5;
  vvec_barj   = ( vvec(:,1:end-1,k) + vvec(:,2:end,k) )*0.5;
  u_ll(:,:,k) = ca.*uvec_bari - sa.*vvec_barj;
  v_ll(:,:,k) = ca.*vvec_barj + sa.*uvec_bari;
end
return
end
