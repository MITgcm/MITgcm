axis tight;
ax=axis;
xxx_range = ax(4) - ax(3);
yyy_range = ax(2) - ax(1);
tp_xxx_range = .05*xxx_range;
tp_yyy_range = .025*yyy_range;
xxx = [ax(1)-tp_yyy_range ax(2)+tp_yyy_range (ax(3)-tp_xxx_range) (ax(4)+tp_xxx_range)]
axis(xxx)
