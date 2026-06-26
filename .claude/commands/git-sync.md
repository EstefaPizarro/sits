# git-sync

Sincroniza con origin y muestra el estado.

1. Ejecuta `git fetch origin`
2. Ejecuta `git log HEAD..origin/$(git branch --show-current) --oneline` para ver commits nuevos
3. Ejecuta `git status`

Reporta los commits nuevos con autor y mensaje. Luego pregunta si quiero hacer `git pull`.
