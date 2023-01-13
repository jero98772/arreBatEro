package main
import "fmt"
func main(){
	var name string
	fmt.Scanln(&name)
	if name=="alice" || name=="bob"{
	fmt.Println(name+" welcome")
	}else{
	fmt.Println(name+" not welcome")
	}
}