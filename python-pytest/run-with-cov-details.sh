#!/usr/bin/env bash
pytest --cov --cov-report html
echo "Starting web server to present the coverage report. Visit at http://localhost:8000. Press 'Ctrl' + 'C' to exit."
python -m http.server 8000 --bind 127.0.0.1 --directory htmlcov 2>&1 | grep -v "GET /.* 200 -"
