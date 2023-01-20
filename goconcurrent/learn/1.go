//https://golangbot.com/channels/
package main
import "fmt"
func h(done chan bool){
	fmt.Println("gor")
	done<-true
}
func main(){
	done:=make(chan bool)
	go h(done)
	<-done
	fmt.Println("ok")

}
