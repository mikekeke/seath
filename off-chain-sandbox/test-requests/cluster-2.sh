#!/usr/bin/env bash
set -x


curl -X POST "http://localhost:3003/connect" -H 'Content-Type: application/json' -d "3004"
curl -X POST "http://localhost:3004/connect" -H 'Content-Type: application/json' -d "3005"
curl -X POST "http://localhost:3005/connect" -H 'Content-Type: application/json' -d "3004"
curl -X POST "http://localhost:3006/connect" -H 'Content-Type: application/json' -d "3004"
curl -X POST "http://localhost:3007/connect" -H 'Content-Type: application/json' -d "3006"

