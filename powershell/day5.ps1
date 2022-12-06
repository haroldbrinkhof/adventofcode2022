# custom class so we don't use Systems..ArrayList
class Stacks{
	$stacks = "","","","","","","","","",""
	
	[void]AddState([string]$line){
		for(($c = 1),($pos = 1); $c -lt $line.length; ($c += 4), ($pos++)){
			if($line[$c] -match "^ *$"){ # do not allow 'empty' values
				continue	
			}
			$this.Push($pos, $line[$c])
		}
	}
	[void]FinalizeState(){
		for($pos = 1; $pos -lt 10; $pos++){	
			$tmp = $this.stacks[$pos] 
			$this.stacks[$pos] = $tmp[-1..-$tmp.length] -join '' 
	}
	}

	[void]Push([int32]$position,[string]$value){
		if($value -match "^ *$"){ # do not allow 'empty' values
			return
		}
		$this.stacks[$position] = "$value$($this.stacks[$position])"
	}

	[string]Pop([int32]$position){
		return $this.Pop($position, 1)
	}

	[string]Pop([int32]$position, $amount){
		$tmp = $this.stacks[$position].Substring(0, $amount)

		$this.stacks[$position] = $this.stacks[$position].Substring($amount)
		return $tmp
	}
	[String]Peek([int32]$position){
		return $this.stacks[$position][0]
	}
	[String]PeekAllTops(){
		$result = ""
		for($c = 1; $c -lt 10; $c++){
			$result += $this.Peek($c)
		}
		return $result
	}
}

class MoveOrder{
	[int32]$amount = 0
	[int32]$from = 0
	[int32]$to = 0

	MoveOrder([string]$order){
		if($order -match "move ([0-9]+) from ([0-9]+) to ([0-9]+)"){
			$this.amount = [int32]$Matches.1
			$this.from = [int32]$Matches.2
			$this.to = [int32]$Matches.3
		}
	}

	[int32]GetAmount(){
		return $this.amount
	}
	[int32]GetFrom(){
		return $this.from
	}
	[int32]GetTo(){
		return $this.to
	}
}

class CrateMover9000{
	[Stacks]$stacks

	CrateMover9000([Ref]$stacksRef){
		$this.stacks = $stacksRef.Value
	}

	[void]ActOnMoveOrder([MoveOrder]$order){
		for($c = 0; $c -lt $order.GetAmount(); $c++){
			$item = $this.stacks.Pop($order.GetFrom())
			$this.stacks.Push($order.GetTo(), $item)
		}
	}

	[void]CompleteOrders([MoveOrder[]]$orders){
		foreach($order in $orders){
			$this.ActOnMoveOrder($order)
		}
	}
}
	

class CrateMover9001:CrateMover9000{
	[Stacks]$stacks

	CrateMover9001([Ref]$stacksRef):base($stackRef){
		$this.stacks = $stacksRef.Value
	}

	[void]ActOnMoveOrder([MoveOrder]$order){
		$item = $this.stacks.Pop($order.GetFrom(), $order.GetAmount())
		$this.stacks.Push($order.GetTo(), $item)
	}

}


Write-Output "First simulation"
$stacks = [Stacks]::new()
[MoveOrder[]]$moveOrders = @()

foreach($line in Get-Content "day5_input"){
	if($line[0] -eq "m"){
		$moveOrders += [MoveOrder]::new($line)
	} elseif($line[0] -eq "["){
		$stacks.AddState($line)
	}}

$stacks.FinalizeState()

$crane = [CrateMover9000]::new([Ref]$stacks)
$crane.CompleteOrders($moveOrders)

Write-Output $stacks.PeekAllTops()

Write-Output "Revised simulation"
$stacks = [Stacks]::new()
[MoveOrder[]]$moveOrders = @()

foreach($line in Get-Content "day5_input"){
	if($line[0] -eq "m"){
		$moveOrders += [MoveOrder]::new($line)
	} elseif($line[0] -eq "["){
		$stacks.AddState($line)
	}}

$stacks.FinalizeState()

$crane = [CrateMover9001]::new([Ref]$stacks)
$crane.CompleteOrders($moveOrders)

Write-Output $stacks.PeekAllTops()
