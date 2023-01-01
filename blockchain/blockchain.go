//https://jeiwan.net/posts/building-blockchain-in-go-part-1/
package main
import ("fmt"
	"crypto/sha256"
	"strconv"
	"time"
	"bytes"
	"math/big"
	"flag"
	"math"
	"encoding/binary"
	"log"
	"encoding/gob"
	"github.com/boltdb/bolt"
)
const targetBits=16
const dbFile="blockchain.db"
const blocksBucket="block"
type block struct{
	timestamp int64
	data  []byte
	prevBlockHash []byte
	hash []byte
	nonce int
}
type blockChain struct{
	//blocks []*block
	tip []*block
	db *bolt.DB
}
type blockChainIterator struct{
	currentHash []block
	db *bolt.DB
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
	return result.Bytes()
}
func newProofOfWork(b *block) *proofOfWork{
	target:=big.NewInt(1)
	target.Lsh(target, uint(256-targetBits))
	pow:=&proofOfWork{b, target}
	return pow
}
func (bc *blockChain) addBlock(data string){
	var lasthash []byte
	err:=bc.db.View(func(tx *bolt.Tx) error){
		b:= tx.Bucket([]byte(blocksBucket))
		lasthash=b.get([]byte("l"))
		return nil
	}
	newBlock:=newBlock(data,lasthash)
	err = bc.db.Update(func(tx *bolt.Tx) error {
		b := tx.Bucket([]byte(blocksBucket))
		err := b.Put(newBlock.Hash, newBlock.Serialize())
		err = b.Put([]byte("l"), newBlock.Hash)
		bc.tip = newBlock.Hash

		return nil
	})

	//prevblock:=bc.blocks[len(bc.blocks)-1]
	//newBlock:=newBlock(data,prevblock.hash)
	//bc.blocks=append(bc.blocks,newBlock)
}
func (bc *blockChain) iterator() *blockChainIterator{
	return &blockChainIterator{bc.tip,bc.db}
}
func (i *blockChainIterator) next() *block{
	var thisBlock block
	err := i.db.View(func(tx *bolt.Tx) error {
		b := tx.Bucket([]byte(blocksBucket))
		encodedBlock := b.Get(i.currentHash)
		thisBlock = DeserializeBlock(encodedBlock)

		return nil
	})
	i.currentHash=thisBlock.prevBlockHash
	return block
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
func (cli *CLI) printUsage() {
	fmt.Println("Usage:")
	fmt.Println("  addblock -data BLOCK_DATA - add a block to the blockchain")
	fmt.Println("  printchain - print all the blocks of the blockchain")
}
func (cli *CLI) run(){
	cli.validateArgs()
	addBlockCmd:=flag.NewFlagSet("addBlock",flag.ExitOnError)

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
func deserializeBlock(d []bytes) *block{
	var myblock block
	decoder:=gob.NewDecoder(bytes.NewReader(d))
	err:=decoder.Decode(&myblock)
	return &myblock
}
func genesisBlock() *block{
	return newBlock("Genesis block",[]byte{})
}
func newBlockChain() *blockChain{
	var tip []byte
	db, err :bolt.Open(dbFile, 0600,nil)
	err=db.Update(func(tx *bolt.Tx) error{
		b:=tx.Bucket([]bytes(blocksBucket))
		if b==nil{
			genesis:=genesisBlock()
			b,err:=tx.CreateBucket([]byte(blocksBucket))
			err=b.Put(genesis.hash,genesis.serialize())
			err=b.Put([]byte("l"),genesis.hash)
			tip=genesis.hash
		}else{
			tip=b.Get([]byte("l"))
		}
		return nil
	})
	bc:=blockChain{tip,db}
	return &bc
	//return &blockChain{[]*block{genesisBlock()}}
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