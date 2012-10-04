#!/bin/sh

echo "Creating Sean."
curl -H "content-type: text/plain" -H 'Link: </riak/people/mark>; riaktag="friend", </riak/people/kevin>; riaktag="friend"' -d "Sean" -X PUT http://127.0.0.1:8091/riak/people/sean
echo "Creating Mark."
curl -H "content-type: text/plain" -H 'Link: </riak/people/sean>; riaktag="friend", </riak/people/justin>; riaktag="friend"' -d "Mark" -X PUT http://127.0.0.1:8091/riak/people/mark
echo "Creating John."
curl -H "content-type: text/plain" -H 'Link: </riak/people/sean>; riaktag="supervises", </riak/people/mark>; riaktag="supervises"' -d "John" -X PUT http://127.0.0.1:8091/riak/people/john
echo "Creating Kevin."
curl -H "content-type: text/plain" -d "Kevin" -X PUT http://127.0.0.1:8091/riak/people/kevin
echo "Creating Justin."
curl -H "content-type: text/plain" -H 'Link: </riak/people/kevin>; riaktag="supervises"' -d "Justin" -X PUT http://127.0.0.1:8091/riak/people/justin
echo "Creating Tony."
curl -H "content-type: text/plain" -H 'Link: </riak/people/john>; riaktag="supervises", </riak/people/marisa>; riaktag="supervises"' -d "Tony" -X PUT http://127.0.0.1:8091/riak/people/tony
echo "Creating Marisa."
curl -H "content-type: text/plain" -H 'Link: </riak/people/maureen>; riaktag="supervises"' -d "Marisa" -X PUT http://127.0.0.1:8091/riak/people/marisa
echo "Creating Maureen."
curl -H "content-type: text/plain" -H 'Link: </riak/people/mark>; riaktag="friend"' -d "Maureen" -X PUT http://127.0.0.1:8091/riak/people/maureen
