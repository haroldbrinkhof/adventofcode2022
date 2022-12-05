function Get-Day1-Content{
	return Get-Content -Path "day1_input"
}

$highest1 = 0
$highest2 = 0
$highest3 = 0

$current = 0
$elf=0

function Put-Highest($number, [REF]$highest1,[REF]$highest2,[REF]$highest3){
	if($number -gt $highest1.Value){
		$highest3.Value = $highest2.Value
		$highest2.Value = $highest1.Value
		$highest1.Value = $number
	} elseif($number -gt $highest2.Value){
		$highest3.Value = $highest2.Value
		$highest2.Value = $number
	} elseif ($number -gt $highest3.Value){
		$highest3.Value = $number
	}
}

foreach($line in Get-Day1-Content){
	if ($line -match '^[0-9]+$'){
		$current = $current + $line 
	} else {
		$elf+=1
		Put-Highest $current ([REF]$highest1) ([REF]$highest2) ([REF]$highest3)
		$current = 0
	}
}


$highest3total = $highest1 + $highest2 + $highest3
Write-Output $highest1
Write-Output $highest3total
