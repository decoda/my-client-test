start werl +P 1024000 -smp auto -pa ../ebin -name tangj@localhost -setcookie tangjs -boot start_sasl -config myserver -s main server_start myserver
#start werl +P 1024000 -smp auto -pa ../ebin -name tangj@localhost -setcookie tangjs 
#+debug_info -boot start_sasl -config myserver -s
#::ping 127.0.0.1 -n 3 >nul
exit