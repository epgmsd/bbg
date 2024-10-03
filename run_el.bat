@echo off

REM #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
REM # Batch script to update bbg folder on github.com/mf3a #
REM #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ECHO.
ECHO Running batch process to update bbg folder on github.com/mf3a
ECHO Code will fetch from main branch but push to dataupdate branch after updating
ECHO.

ECHO Step 1: Force sync from main branch at remote
git fetch origin main
git reset --hard origin/main
git merge origin/main
ECHO.
ECHO.

ECHO Step 2: Run main.r 
REM "F:\R-4.4.1\bin\Rscript.exe" "%~dp0main.r"
ECHO.
ECHO.

ECHO Step 3: Push to dataupdate branch after updating
git add .
git commit -m "new data update"
git push origin main:dataupdate
ECHO.
ECHO.

PAUSE
