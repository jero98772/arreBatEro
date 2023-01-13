package main
import "fmt"
func main(){
	var num,dig1,dig2,dig3 int
	fmt.Scanln(&num)
	dig1=num/100
	dig2=((num/10)%10)*10
	dig3=(num%10)*100	
	fmt.Println(dig1+dig2+dig3)
}

//345