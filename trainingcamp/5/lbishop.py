testcases=int(input())
def checkOnlyRow(row,board):
	tmp=0	
	for i in range(8):	
		if board[row][i]=="#":
			tmp+=1
	if tmp==1: return True
	else:return False
def checkOnlyCol(col,board):
	tmp=0	
	for i in range(8):
		if board[i][col]=="#":
			tmp+=1
	if tmp==1: return True
	else:return False
	
def validpos(i):
	if 0<i<8:
		return True
	else:
		return False

def check(posi,posj,board):
	
	return board[posi+1][posj+1] == '#' and board[posi+1][posj-1] == '#' and board[posi-1][posj+1] == '#' and board[posi-1][posj-1] == '#' 


for _ in range(testcases):
	input()
	canprint=True
	board=[]
	for __ in range(8):
		#board[i]=[0]*8
		#for j in range(0,8):
		board.append(list(input()))
	#print(board)#,j)
	for i in range(1,7):
		for j in range(1,7):
			#Where's?
				#ans = board[i][j] == "#"
			if board[i][j]=="#" and check(i,j,board):
				print(i+1,j+1)
				#i = j = 9
				

"""

#.#.....
.#......
..#.....
...#....
....#...
.....#..
......#.
.......#
"""	