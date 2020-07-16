#!/bin/bash

SERVER_URL=$1
PLAYER_KEY=$2

echo "ServerUrl: $SERVER_URL; PlayerKey: $PLAYER_KEY"

RESPONSE_FILE=$( mktemp -t tmp.XXXXXXXXXX )
HTTP_CODE=$( curl -s -o "$RESPONSE_FILE" -w '%{http_code}' -XPOST -d "$PLAYER_KEY" "$SERVER_URL" )
EXIT_CODE=$?

if [[ ${EXIT_CODE} -ne 0 ]] ; then
    echo "run error code: $EXIT_CODE"
    exit 0
fi

if [[ ${HTTP_CODE} -ne 200 ]] ; then
    echo "Unexpected server response:"
    echo "HTTP code: $HTTP_CODE"
    echo "Response body: $( cat "$RESPONSE_FILE" )"
    exit 0
fi

echo "Server response: $( cat "$RESPONSE_FILE" )"
