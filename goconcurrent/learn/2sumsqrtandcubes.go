//https://golangbot.com/channels/
package main
import "fmt"
func sqrtc(num int,op chan int){
	sum:=0
	for num!=0{
		digit:=num%10
		sum+=digit*digit
		num/=10
	}
	op<-sum
}
func calcCubes(num int, op chan int){
	sum:=0
	for num!=0{
		digit:=num%10
		sum+=digit*digit*digit
		num/=10
	}
	op<-sum
}
func main(){
	num:=589
	sq:=make(chan int)
	cu:=make(chan int)
	go sqrtc(num,sq)
	go calcCubes(num,cu)
	sqrs,cubs:=<-sq,<-cu
	fmt.Println("out",sqrs+cubs)

}
