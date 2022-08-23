import math

cantidad = int(input())
for i in range(cantidad):
    a, b, c, x = map(int, input().split())
    d = 0
    d += c
    d += math.sqrt(a **2 + (b + c)**  2)
    e = math.sqrt(a**2 + b**2) * (x / 100)
    d += e
    k = math.atan(a / b)
    k = math.degrees(k)
    d += b + c - (math.cos(math.radians(k)) * e)
    d += math.sqrt((a -(math.cos(math.radians(k)) * e))**2 + c**2)
    print(d)