//https://golangbot.com/channels/
package main
import "fmt"
func dig(n int,chl chan int){
	for n!=0{
		dig:=n%10
		chl<-dig
		n/=10
	}
	close(chl)
}
func sqrtc(num int,op chan int){
	sum:=0
	dch:=make(chan int)
	go dig(num,dch)
	for dig:=range dch{
		sum+=dig*dig
	}
	op<-sum
}
func calcCubes(num int, op chan int){
	sum:=0
	dch:=make(chan int)
	go dig(num,dch)
	for dig:=range dch{
		sum+=dig*dig*dig
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
