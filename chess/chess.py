import numpy as np
from colorama import Fore, Back, Style

def printmarix(board):
	num=0
	for i in range(len(board)):
		for ii in range(len(board[0])):
			if i%2==num and ii%2==num:
			#if eval("0o"+str(i)+str(ii))%2==num:
				print(board[i][ii].pieceColor(Back.WHITE),end=Style.RESET_ALL+"")
			else:
				print(board[i][ii].pieceColor(Back.BLACK),end=Style.RESET_ALL+"")
		if num:num=0
		else:num=1
		print("")
def fill(mask,board,size=8):#fen notation
	col=0
	row=0
	for i in mask:
		#print(row,col)
		if i=="/":
			row=0
			col+=1
		if i.isupper():
			board[col][row]=piece(i,Fore.MAGENTA+Style.BRIGHT,pos=[col,row])
			row=row+1%size
		if i.islower():
			board[col][row]=piece(i,Fore.GREEN+Style.BRIGHT,pos=[col,row])
			row=row+1%size
	#for i in range(len(board)):
		#for ii in range(len(board[0])):
class piece:
	def __init__(self,typep,color,pos):#pos):
		self.typep=typep
		self.color=color
		self.x=pos[0]
		self.y=pos[1]
		#self.pos
	def __str__(self):
		return self.typep
	def pieceColor(self,c):
		return self.color+c+self.typep

class pawn(piece):
	def __init__(self):
		super().__init__(typep,color,pos)
	#def verifieMove(board):
	#	if board[pos[0]+1][pos[1]+1]:
	#		pass
def makeMove(board,pos1,pos2,newpos1,newpos2):
	board[tmp.x][tmp.y]
def main():
	nullpice=piece("0",Fore.BLUE,[None,None])
	board=np.full((8,8),nullpice,dtype=piece)
	figs={"p":pawn}
	mask="RHBQKBHR/PPPPPPPP/8/8/8/8/pppppppp/rhbqkbhr"
	#printmarix(board)
	fill(mask,board)
	printmarix(board)
	#move=input()
	x=1
	y=1
	tmp=board[x][y]
	board[tmp.x][tmp.y+1]=tmp
	board[tmp.x][tmp.y]=nullpice
	
	print(tmp)
	
	#for i in range(2):
	#print(tmp.x,tmp.y)
	#tmp.y+=1
	#tmp.x+=1
	#board[tmp.x][tmp.y+1]=tmp#board[1][1]
	printmarix(board)
main()
"""
chess as lib

	

class board:


"""