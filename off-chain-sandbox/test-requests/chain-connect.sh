#!/usr/bin/env bash
set -x

curl -X POST "http://localhost:3000/connect" -H 'Content-Type: application/json' -d "3001"
curl -X POST "http://localhost:3001/connect" -H 'Content-Type: application/json' -d "3002"
curl -X POST "http://localhost:3002/connect" -H 'Content-Type: application/json' -d "3003"
curl -X POST "http://localhost:3003/connect" -H 'Content-Type: application/json' -d "3004"
curl -X POST "http://localhost:3004/connect" -H 'Content-Type: application/json' -d "3005"
curl -X POST "http://localhost:3005/connect" -H 'Content-Type: application/json' -d "3006"
curl -X POST "http://localhost:3006/connect" -H 'Content-Type: application/json' -d "3007"

