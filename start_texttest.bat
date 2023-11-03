set TEXTTEST_HOME=%~dp0
cd %TEXTTEST_HOME%

if not exist "venv" (
    py -m venv venv
)

venv\Scripts\pip install texttest

if %ERRORLEVEL% GEQ 1 (
    pause
) else (
    venv\Scripts\texttestc.py -con %*
)
