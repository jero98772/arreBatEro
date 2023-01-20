package main
import ("fmt"
	"time"
	)
func multablen(n,times int){
	fmt.Printf("table of :%d\n",n)
	for i:=0;i<times;i++{
		fmt.Printf("%d*%d=%d\n",i,n,i*n)
	}
	time.Sleep(1*time.Millisecond)
}
func main(){
	var n,exit int
	fmt.Scanln(&n)
	for i:=0;i<n;i++{
		go multablen(i,n)
	}
	fmt.Scanln(&exit)
}
