%Source: Ports of programs found in TPDB/C/AProVE_numeric
%query: test_fun(g,g,g).
% source: port of http://cl2-informatik.uibk.ac.at/mercurial.cgi/TPDB/file/72cccd64ec42/C/AProVE_numeric/svcomp_a.06.c
test_fun(X, Y, Z) :- loop(X, Y, Z, 0).
loop(X, Y, Z, C) :- X > Y + Z, Y1 is Y + 1, Z1 is Z + 1, C1 is C + 1, loop(X, Y1, Z1, C1).
loop(X, Y, Z, C) :- X =< Y + Z.