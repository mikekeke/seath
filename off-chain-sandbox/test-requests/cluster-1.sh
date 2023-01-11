#!/usr/bin/env bash
set -x

curl -X POST "http://localhost:3000/connect" -H 'Content-Type: application/json' -d "3001"
curl -X POST "http://localhost:3002/connect" -H 'Content-Type: application/json' -d "3001"

