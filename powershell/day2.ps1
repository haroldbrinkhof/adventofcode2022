$outcomes = @{A = @{X = 3; Y = 6; Z = 0}; B = @{X = 0; Y = 3; Z = 6}; C = @{X = 6; Y = 0; Z = 3} }
$values = @{ X=1;Y=2;Z=3}

$score = 0
foreach($line in Get-Content -Path "day2_input"){
	$moves = $line.Split(" ")
	$score += $values["$($moves[1])"]
	$score += $outcomes["$($moves[0])"]["$($moves[1])"]
}

Write-Output $score

#	L[0]	D[3]	W[6]
# R[A]	S(3)	R(1)[4]	P(2)[8]
# P[B]	R(1)	P(2)[5]	S(3)[9]
# S[C]	P(2)	S(3)[6]	R(1)[7]


$outcomes_part2 = @{A = @{X = 3; Y = 4; Z = 8}; B = @{X = 1; Y = 5; Z = 9}; C = @{X = 2; Y = 6; Z = 7} }

$score = 0
foreach($line in Get-Content -Path "day2_input"){
	$moves = $line.Split(" ")
	$score += $outcomes_part2["$($moves[0])"]["$($moves[1])"]
}

Write-Output $score
