#!/usr/bin/env bash
set -e
[ -d coverage ] || mkdir coverage
go test ./... -coverprofile=coverage/coverage.out
go tool cover -html=coverage/coverage.out -o coverage/index.html
echo "Starting web server to present the coverage report. Visit at http://localhost:8000. Press 'Ctrl' + 'C' to exit."
python3 -m http.server 8000 --bind 127.0.0.1 --directory coverage 2>&1 | grep -v "GET /.* 200 -"
