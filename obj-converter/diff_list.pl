:- module(diff_list, [
	      diff_list_init/1,
	      diff_list_append/3,
	      diff_list_close/2
	  ]).

diff_list_init(X-X).

diff_list_append(A-[E|AE], E, A-AE).

diff_list_close(X-[], X).
