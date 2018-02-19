Write-Host "===> dderl post_release" -foregroundcolor "magenta"
$DDerlScript = "/_build/prod/lib/dderl/post_release.ps1"
& ((Split-Path $MyInvocation.InvocationName) + $DDerlScript) -app egambo

Write-Host "===> egambo post_release" -foregroundcolor "magenta"
$egamboPriv = "/_build/prod/rel/egambo/lib/egambo-*/priv"
cd ((Split-Path $MyInvocation.InvocationName) + $egamboPriv)
If (Test-Path node_modules) {
    Remove-Item node_modules -Force -Recurse
    Write-Host "===> directory 'node_modules' deleted" -foregroundcolor "magenta"
}

Write-Host "===> npm install" -foregroundcolor "magenta"
npm install

Write-Host "===> npm run build" -foregroundcolor "magenta"
npm run build

Write-Host "===> clean up"
ForEach ($Dir in @("node_modules")) {
    If (Test-Path $Dir) {
        Remove-Item $Dir -Force -Recurse
        Write-Host "===> directory '$Dir' deleted" -foregroundcolor "magenta"
    }
}
Write-Host "===> egambo priv clean up finished"
