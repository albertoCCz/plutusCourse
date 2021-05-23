### WSL Input/output error:
When a typed `code .` on the ubuntu terminal in WSL 2 the command was not recognized. Also, if VS Code was started from Windows the section title error was shown in the VS Code terminal. To resolve it:

Go to PowerShell in type:
```powershell
wsl.exe --shutdown
```
