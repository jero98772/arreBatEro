//https://jeiwan.net/posts/building-blockchain-in-go-part-1/
package main
import ("fmt"
	"crypto/sha256"
	"strconv"
	"time"
	"bytes"
	"math/big"
	"math"
	"encoding/binary"
	"log"
	"encoding/gob"
)
const targetBits=16
const blocksBucket="block"
type block struct{
	timestamp int64
	data  []byte
	prevBlockHash []byte
	hash []byte
	nonce int
}
type blockChain struct{
	blocks []*block

}
type blockChainIterator struct{
	currentHash []block

}
type proofOfWork struct{
	block *block
	target *big.Int
}
type CLI struct{
	bc *blockChain
}

func IntToHex(num int64) []byte {
	buff := new(bytes.Buffer)
	err := binary.Write(buff, binary.BigEndian, num)
	if err != nil {
		log.Panic(err)
	}

	return buff.Bytes()
}
func (b *block) setHash(){
	timestamp:=[]byte(strconv.FormatInt(b.timestamp,10))
	headers := bytes.Join([][]byte{b.prevBlockHash, b.data, timestamp}, []byte{})

	hash:=sha256.Sum256(headers)
	b.hash=hash[:]
}
func (b* block) serialize() []byte{
	var result bytes.Buffer
	encoder:=gob.NewEncoder(&result)
	err:=encoder.Encode(b)
	if err != nil {
		log.Panic(err)
	}
	return result.Bytes()
}
func newProofOfWork(b *block) *proofOfWork{
	target:=big.NewInt(1)
	target.Lsh(target, uint(256-targetBits))
	pow:=&proofOfWork{b, target}
	return pow
}
func (bc *blockChain) addBlock(data string){
	prevblock:=bc.blocks[len(bc.blocks)-1]
	newBlock:=newBlock(data,prevblock.hash)
	bc.blocks=append(bc.blocks,newBlock)
}
func (pow *proofOfWork) prepareData(nonce int) []byte{
	data:=bytes.Join([][]byte{
			pow.block.prevBlockHash,
			pow.block.data,
			IntToHex(pow.block.timestamp),
			IntToHex(int64(targetBits)),
			IntToHex(int64(nonce)),
		},
		[]byte{},
	)
	return data
}
func (pow *proofOfWork) run()(int,[]byte){
		var hashInt big.Int
		var hash[32]byte
		nonce:=0
		maxNonce:=math.MaxInt64
		fmt.Printf("Mining the block containing \"%s\"\n", pow.block.data)
		for nonce<maxNonce{
			data:= pow.prepareData(nonce)
			hash=sha256.Sum256(data)
			fmt.Println("\r%x",hash)
			hashInt.SetBytes(hash[:])
			if hashInt.Cmp(pow.target)==-1{
				break
			}else {
				nonce++
			}
		}
	fmt.Print("\n\n")

	return nonce, hash[:]
}
func (pow *proofOfWork) validate() bool{
	var hashInt big.Int
	data:= pow.prepareData(pow.block.nonce)
	hash:=sha256.Sum256(data)
	hashInt.SetBytes(hash[:])
	isValid:= hashInt.Cmp(pow.target)==-1
	return isValid
}

func newBlock(data string,prevBlockHash []byte) *block{
	thisBlock:=&block{time.Now().Unix(), []byte(data),prevBlockHash,[]byte{},0}
	pow:=newProofOfWork(thisBlock)
	nonce, hash:=pow.run()
	thisBlock.hash=hash[:]
	thisBlock.nonce=nonce
	//thisBlock.setHash()
	return thisBlock
}
func genesisBlock() *block{
	return newBlock("Genesis block",[]byte{})
}
func newBlockChain() *blockChain{
	return &blockChain{[]*block{genesisBlock()}}
}

func main(){
	blockchain:= newBlockChain()
	for i:=0;i<100;i++{
		blockchain.addBlock(string(i))
	}
	for _,block:= range blockchain.blocks{
		pow:=newProofOfWork(block)
		fmt.Println("Previous block {}",block.prevBlockHash)
		fmt.Println("Data {}",block.data)
		fmt.Println("hash {}",block.hash)
		fmt.Println("hash timestamp",block.timestamp)
		fmt.Println("is valid {}",strconv.FormatBool(pow.validate()))
		fmt.Println("------------")
	}
}