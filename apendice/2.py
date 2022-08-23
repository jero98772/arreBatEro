"""
def pad(x):
	al=False
	count=0
	for i in str(x):
		if i==".":
			al=True
		if al:
			if
cantidad = int(input())
for x in range(cantidad):
    score, chart = map(float, input().split())
    if score >= 10000000:
        modifier = 2
    elif score <= 9800000:
        modifier = (score - 9500000) / 300000
    else:
        modifier = 1 + (score - 9800000) / 200000
    print(round(modifier + chart,7))
    #print('{0:.7f}'.format(modifier + chart))
"""
cantidad = int(input())
for x in range(cantidad):
    score, chart = map(float, input().split())
    if score >= 10000000:
        modifier = 2
    elif score <= 9800000:
        modifier = (score - 9500000) / 300000
    else:
        modifier = 1 + (score - 9800000) / 200000
    ans=modifier + chart
    print('%.7f' %ans)#,end="\n")
    #print('{0:.7f}'.format(modifier + chart))
