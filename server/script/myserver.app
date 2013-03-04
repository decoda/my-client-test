{ 
  application, myserver,
  [
   {description, "This is my server!"},
   {vsn, "0.0.1"},
   {modules, [main, server_app, server_sup]},
   {registered, []},
   {applications, [kernel, stdlib]},
   {mod, {server_app, []}}
  ]
 }.