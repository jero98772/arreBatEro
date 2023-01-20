package main
import (
	"bufio"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io"
	"log"
  	"net"
	"net/http"
	"os"
	"fmt"
	"time"
	"github.com/davecgh/go-spew/spew"
	"github.com/gorilla/mux"
	"github.com/joho/godotenv")
type block struct{
	Index int
	Timestamp string
	data string
	Hash string
	PrevHash string
}
type message struct{
	data string
}
var Blockchain []block
var bcServer chan []block
func calculateHash(myblock block) string{
	record:=string(myblock.Index)+myblock.Timestamp+myblock.data+myblock.PrevHash
	h:=sha256.New()
	h.Write([]byte(record))
	hashed:=h.Sum(nil)
	return hex.EncodeToString(hashed)
}
func isBlockValid(newblock,oldblock block) bool{
	if oldblock.Index+1!=newblock.Index{
		return false
	}
	if oldblock.Hash!=newblock.PrevHash{
		return false
	}
	if calculateHash(newblock)!=newblock.Hash{
		return false
	}
	return true
}
func generateBlock(oldblock block,data string)(block,error){
	var newblock block
	newblock.Index=oldblock.Index+1
	newblock.Timestamp=time.Now().String()
	newblock.data=data
	newblock.PrevHash=oldblock.Hash
	newblock.Hash=calculateHash(newblock)
	return newblock,nil
}
func respondWithJSON(w http.ResponseWriter, r *http.Request,code int,payload interface{}){
	response,err:=json.MarshalIndent(payload,""," ")
	if err!=nil{
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("HTTP 500: Internal Server Error"))
		return
	}
	w.WriteHeader(code)
	w.Write(response)
}
func run()error{
	mux:=makeMuxRouter()
	httpAddr:=os.Getenv("PORT")
	fmt.Println(httpAddr)
	log.Println("listening in:",httpAddr)
	s:=&http.Server{Addr:":"+httpAddr,Handler:mux,ReadTimeout:10*time.Second,WriteTimeout:10*time.Second,MaxHeaderBytes:1>>20,}
	if err:=s.ListenAndServe();err!=nil{
		return err
	}
	return nil
}
func handleGetBlockchain(w http.ResponseWriter,r *http.Request){
	bytes,err:=json.MarshalIndent(Blockchain,""," ")
	if err!=nil{
		http.Error(w,err.Error(),http.StatusInternalServerError)
		return
	}
	io.WriteString(w,string(bytes))
}
func replaceChain(newBlock []block){
	if len(newBlock) >len(Blockchain){
		Blockchain=newBlock
	}
}
func handleWriteBlock(w http.ResponseWriter, r *http.Request){
	var m message
	decoder :=json.NewDecoder(r.Body)
	if err:=decoder.Decode(&m);err!= nil{
		respondWithJSON(w,r,http.StatusBadRequest,r.Body)
		return
	}
	defer r.Body.Close()
	newBlock,err:=generateBlock(Blockchain[len(Blockchain)-1],m.data)
	if err != nil {
		respondWithJSON(w, r, http.StatusInternalServerError, m)
		return
	}
	if isBlockValid(newBlock,Blockchain[len(Blockchain)-1]){
		newBlockChain:=append(Blockchain,newBlock)
		replaceChain(newBlockChain)
		spew.Dump(Blockchain)
	}
	respondWithJSON(w,r,http.StatusCreated,newBlock)
}
func makeMuxRouter() http.Handler{
	muxRouter:=mux.NewRouter()
	muxRouter.HandleFunc("/",handleGetBlockchain).Methods("GET")
	muxRouter.HandleFunc("/",handleWriteBlock).Methods("POST")
	return muxRouter
}
func main(){
	err:=godotenv.Load()
	if err != nil {
		log.Fatal(err)
	}
	go func(){
		genesisblock:=block{0,time.Now().String(),"","",""}
		spew.Dump(genesisblock)
		Blockchain= append(Blockchain,genesisblock)
	}()
	log.Fatal(run())
}