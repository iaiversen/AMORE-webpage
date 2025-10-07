@echo off
echo ================================
echo Synchronizing styles.scss files
echo ================================
echo.
echo Master file: styles.scss
echo.

REM Copy to pages folder
copy /Y ..\assets\styles\styles.scss ..\pages\styles.scss
echo [OK] Copied to pages\
echo.

REM Copy to styles folder
copy /Y ..\assets\styles\styles.scss ..\assets\styles\styles.scss
echo [OK] Copied to assets\styles\
echo.

REM Copy to LMAs subfolder 
copy /Y ..\assets\styles\styles.scss ..\LMAs\styles.scss 
echo [OK] Copied to LMAs\
echo.

REM Add more folders here as needed when you create them:
REM copy /Y ..\assets\styles\styles.scss ..\pages\styles.scss 
REM echo [OK] Copied to pages\

echo =================================
echo         Sync complete!
echo =================================
echo. 
echo Note: _site folder will be regenerated 
echo automatically when you render. 
echo. 
pause