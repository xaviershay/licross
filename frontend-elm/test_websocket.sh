#!/bin/bash

exec curl --include \
  --no-buffer \
  --header "Connection: Upgrade" \
  --header "Upgrade: websocket" \
  --header "Host: http://localhost:8080" \
  --header "Origin: http://localhost:8080" \
  --header "Sec-WebSocket-Key: SGVsbG8sIHdvcmxkIQ==" \
  --header "Sec-WebSocket-Version: 13" \
  http://localhost:8080/game/1/player/45/subscribe
