class StreamHandler{
	$stream = ""
	StreamHandler(){
		$this.stream = Get-Content -Path "day6_input"
	}

	[boolean]AllDistinct([string]$packet){
		for($c = 0; $c -lt $packet.Length;$c++){
			for($t = 0; $t -lt $packet.Length;$t++){
				if($c -ne $t -and $packet[$c] -eq $packet[$t]){
					return $false
				}
			}
		}

		return $true
	}

	[int32] GetPosition([int32]$packetSize){
		for($c = 0; $c -lt ($this.stream.Length - $packetSize);$c++){
			$packet = $this.stream.SubString($c, $packetSize)
			if($this.AllDistinct($packet)){
				return $c + $packetSize;
			}
		}
		return 0
	}
}

$sh = [StreamHandler]::new()
Write-Output "Marker at $($sh.GetPosition(4))"
Write-Output "Message at $($sh.GetPosition(14))"
