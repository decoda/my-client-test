
-ifndef(RECORD).
-define(RECORD, true).

-record(user, {id = 0,
			   x = 0,
			   y = 0,
			   pid = undefined,
			   pid_send = undefined}).
					 
-endif. % RECORD