/*         Compile and Link a Basic program         */
parse arg name '/' switches
name = strip(name)

if switches = ''
   then switches = '/LC'           /* Make a DOS program rather than OS/2 */
   else do
      if translate(left(switches,2)) = left('OS/2',2)
         then switches = ''
         else switches = '/'||switches
   end

/* compile from directory 'source' into 'objs' directory */
'bascom source\'||name ', objs\'||name switches '/O /E;'
saverc = rc

if saverc ª= 0 then exit saverc

/* link edit the object into the current directory */
'link objs\'||name ',,objs\'||name||'.map ;'

if rc ª= 0 then exit rc

/* Make it runable in a Windowed session */
if wordpos("/LC",switches)=0 & wordpos("/lc",switches)=0 then 'winable' STRIP(name)||'.exe'

exit rc
