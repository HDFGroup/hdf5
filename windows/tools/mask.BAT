type nul > %2
for /f "tokens=1,2*" %%a in (
 %1
) do ( if %%a==Modified: (
          echo Modified: XXXX-XX-XX XX:XX:XX XXX>> %2
)      else (
          echo %%a %%b %%c>>%2
)
)
