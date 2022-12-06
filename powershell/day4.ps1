# custom class so we don't have to revert to .NET Set class to do intersections and such
class RangeSet{

	[int32]$start
	[int32]$end
	RangeSet([string]$descriptor){
		$markers = $descriptor.Split("-")
		$this.start = $markers[0]
		$this.end = $markers[1]
	}
	[bool]HasInRange([int32]$point){
		return (($point -ge $this.start) -and ($point -le $this.end))
	}
	[bool]ContainsFully([RangeSet]$other){
		return ($this.HasInRange($other.start) -and $this.HasInRange($other.end))
	}
	[bool]ContainsPartially([RangeSet]$other){
		return ($this.ContainsFully($other) -or ($other.GetRange().Where({$this.HasInRange([int32]$_)}).length -gt 0))
	}
	[int32[]]GetRange(){
		$range = @()
		for($c = $this.start;$c -le $this.end; $c++){
			$range += $c
		}
		return $range
	}
}

class RangeGroup{
	[RangeSet]$range1
	[RangeSet]$range2

	RangeGroup([string]$descriptor){
		$sets = $descriptor.Split(",")
		$this.range1 = [RangeSet]::new($sets[0])		
		$this.range2 = [RangeSet]::new($sets[1])		
	}

	[bool]OneContainsTheOtherFully(){
		return ($this.range1.ContainsFully($this.range2) -or $this.range2.ContainsFully($this.range1))
	}
	[bool]OneContainsTheOtherPartially(){
		return ($this.range1.ContainsPartially($this.range2) -or $this.range2.ContainsPartially($this.range1))
	}
}

$rangeGroups = @()
foreach($line in Get-Content -Path "day4_input"){
	$rangeGroups += ([RangeGroup]::new($line))
}

$fullyCoveredGroups = 0
$partiallyCoveredGroups = 0
foreach($group in $rangeGroups){
	if($group.OneContainsTheOtherFully()){
		$fullyCoveredGroups++
	} 
	if($group.OneContainsTheOtherPartially()){
		$partiallyCoveredGroups++
	}
}


Write-Output "Fully covered: $fullyCoveredGroups"
Write-Output "Partially covered: $partiallyCoveredGroups"


