#!/bin/bash

set -e

SERVER_URL=$1
PLAYER_KEY=$2

echo "ServerUrl: $SERVER_URL; PlayerKey: $PLAYER_KEY"
./solution/solution "$SERVER_URL" "$PLAYER_KEY"
