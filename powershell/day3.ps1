

class Rugsack{
	[byte[]]$compartment1
	[byte[]]$compartment2

	Rugsack([string]$contents){
		$this.Compartmentalize($contents)
	}

	[boolean]HasItem([byte]$item){
		foreach($item1 in $this.compartment1){
			if($item -eq $item1){
				return $true
			}
		}
		foreach($item1 in $this.compartment2){
			if($item -eq $item1){
				return $true
			}
		}
		return	$false 
	}

	[byte]GetSharedItem(){
		foreach($item1 in $this.compartment1){
			foreach($item2 in $this.compartment2){
				if($item1 -eq $item2){
					return $item1
				}
			}
		}
		return 0
	}

	[byte]TransFormPriority([char]$letter){
		$current_value = [byte]$letter
		if($current_value -ge 97){
			return ($current_value - 96)
		} elseif($current_value -ge 65){
			return ($current_value - 38)
		} else {
			return 0
		}
	}

	[void]Compartmentalize([string]$contents){
		for($i = 0;$i -lt ($contents.length / 2); $i++){
			$this.compartment1 += ($this.TransformPriority($contents[$i]))
			$this.compartment2 += ($this.TransformPriority($contents[($contents.length / 2) + $i]))
		}
		Write-Output $this.compartment1
		Write-Output $this.compartment2
	}
}

class RugsackGroup{
	[Rugsack] $one;
	[Rugsack] $two;
	[Rugsack] $three;

	RugsackGroup([Rugsack]$one, [Rugsack]$two, [Rugsack]$three){
	$this.one = $one
	$this.two = $two
	$this.three = $three
	}

	[byte]GetSharedItem(){
		foreach($item1 in $this.one.compartment1){
			if($this.two.HasItem($item1) -and $this.three.HasItem($item1)){
			return $item1
			}
		}
		foreach($item1 in $this.one.compartment2){
			if($this.two.HasItem($item1) -and $this.three.HasItem($item1)){
			return $item1
			}
		}
		return 0
	}
}

[Rugsack[]] $rugsacks = @();
foreach($line in Get-Content -Path "day3_input"){
	$rugsacks += ([Rugsack]::new($line))
}

[int32]$total = 0
foreach($rugsack in $rugsacks){
	$total += $rugsack.GetSharedItem()	
}
Write-Output $total # outcome for part 1

[RugsackGroup[]] $groups = @()
for($c = 0; $c -lt $rugsacks.length ;$c += 3){
	$groups += ([RugsackGroup]::new($rugsacks[$c],$rugsacks[$c+1],$rugsacks[$c+2]))	
}

$total = 0
foreach($group in $groups){
	$total += $group.GetSharedItem()
}
Write-Output $total # outcome for part 2

