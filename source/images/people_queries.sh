#!/bin/sh
black='\033[30m'
red='\033[31m'
green='\033[32m'
yellow='\033[33m'
blue='\033[34m'
magenta='\033[35m'
cyan='\033[36m'
white='\033[37m'


alias Reset="tput sgr0"      #  Reset text attributes to normal
                             #+ without clearing screen.


cecho ()                     # Color-echo.
                             # Argument $1 = message
                             # Argument $2 = color
{
local default_msg="No message passed."
                             # Doesn't really need to be a local variable.

message=${1:-$default_msg}   # Defaults to default message.
color=${2:-$black}           # Defaults to black, if not specified.

  echo "$color$message"
  Reset                      # Reset to normal.

  return
}
cecho "Press [Enter] after each query description to execute." $green

cecho "Q: Get Sean's friends (A:Mark, Kevin)" $yellow
read
curl -s http://127.0.0.1:8091/riak/people/sean/_,friend,1 | grep -v ":\|--" | grep "[:alnum:]"
echo
cecho "Q: Get John's direct reports (A:Mark, Sean)" $yellow
read
curl -s http://127.0.0.1:8091/riak/people/john/_,supervises,1 | grep -v ":\|--" | grep "[:alnum:]"
echo
cecho "Q: Get friends of John's direct reports (A:Sean, Mark, Kevin, Justin)" $yellow
read
curl -s http://127.0.0.1:8091/riak/people/john/_,supervises,_/_,friend,1 | grep -v ":\|--" | grep "[:alnum:]"
echo
cecho "Q: Get Tony's direct and indirect reports (A:[John,Marisa],[Sean,Mark,Maureen])" $yellow
read
curl -s http://127.0.0.1:8091/riak/people/tony/_,supervises,1/_,supervises,1 | grep -v ":" | grep "[:alnum:]\|--"
echo
cecho "Q: Get friends of Tony's indirect reports (A:Sean,Mark,Kevin,Justin)" $yellow
read
curl -s http://127.0.0.1:8091/riak/people/tony/_,supervises,_/_,supervises,_/_,friend,1 | grep -v ":\|--" | grep "[:alnum:]"
echo
cecho "Q: Get friends of friends of Marisa's direct reports (A:Justin, Sean)" $yellow
read
curl -s http://127.0.0.1:8091/riak/people/marisa/_,supervises,_/_,friend,_/_,friend,1 | grep -v ":\|--" | grep "[:alnum:]"
echo
cecho "Q: Get people who report to Mark's friends (A:Kevin)" $yellow
read
curl -s http://127.0.0.1:8091/riak/people/mark/_,friend,_/_,supervises,1 | grep -v ":\|--" | grep "[:alnum:]"
echo
