param(
  [string]$RRoot = "D:\tools\R\4.6.0\portable-r-4.6.0-win-x64",
  [string]$QuartoRoot = "D:\tools\Quarto\1.9.37"
)

$ErrorActionPreference = "Stop"

function Add-ToUserPath {
  param([Parameter(Mandatory=$true)][string]$Dir)
  if (!(Test-Path $Dir)) { throw "Path not found: $Dir" }

  $cur = [Environment]::GetEnvironmentVariable("Path", "User")
  if ([string]::IsNullOrWhiteSpace($cur)) { $cur = "" }
  $parts = $cur.Split(";", [System.StringSplitOptions]::RemoveEmptyEntries)
  if ($parts -contains $Dir) { return }
  $new = ($parts + $Dir) -join ";"
  [Environment]::SetEnvironmentVariable("Path", $new, "User")
}

$rBin = Join-Path $RRoot "bin\x64"
$qBin = Join-Path $QuartoRoot "bin"

Add-ToUserPath -Dir $rBin
Add-ToUserPath -Dir $qBin

Write-Host "Added to USER PATH:"
Write-Host "  $rBin"
Write-Host "  $qBin"

Write-Host ""
Write-Host "Verifying executables (may require opening a new terminal to pick up PATH changes)..."

& (Join-Path $rBin "Rscript.exe") --version
& (Join-Path $qBin "quarto.exe") --version

