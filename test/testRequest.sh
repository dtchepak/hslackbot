#!/bin/sh

curl \
    -d "token=2OMF6jGOFHgQm6ANRZFswxHi" \
    -d "team_id=T0001" \
    -d "channel_id=C2147483705" \
    -d "channel_name=test" \
    -d "timestamp=1355517523.000005" \
    -d "user_id=U2147483697" \
    -d "user_name=Steve" \
    -d "text=googlebot: What is the air-speed velocity of an unladen swallow?" \
    -d "trigger_word=googlebot:" \
    "http://localhost:8080/jira?jiraPath=myserver/display"

