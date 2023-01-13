package main
import "fmt"
func main(){
	var num int
	sum:=0	
	fmt.Scanln(&num)
	for i:=0;i<num;i++{
		sum+=i
	}
	fmt.Println(sum)
}