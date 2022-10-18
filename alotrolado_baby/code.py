import folium
import pandas as pd
import networkx as nx

def validData(txt:str,dicts:list)-> bool:
  """
  validData(txt:str,dicts:list)-> bool
  check if data is valid, if character is in dicts is not valid
  """
  tmp=False
  for i in txt:
    if not i in dicts:
      tmp=False
      break
    else:
      tmp=True
  return tmp

class graphX():
    """
    create graph with pandas data
    """
    def __init__(self,data):
        self.graph=nx.Graph()
        for i in range(len(data)):
            node=data["node"][i]
            weight=(data["harassmentRisk"][i]+0.5)*data["length"][i]
            #weight=(data["length"][i])
            self.graph.add_edge(str(data["edges"][i][0]),str(data["edges"][i][1]),weight=weight)
            self.graph.add_node(str(node))

class configMap:
    def __init__(self,data,location=[6.256405968932449, -75.59835591123756],color="green",weight=20):
        self.m=folium.Map(location=location)
        print(data)
        #line=folium.PolyLine([(-19.0821978, -72.7411), (-28.6471948, 76.9531796), (24.2170111233401, 81.0791015625000)]).add_to(self.m)
        line=folium.PolyLine(data).add_to(self.m)
        #self.m.add_child(line)
    def show(self):
        self.m
    def save(self):
        self.m.save("map.html")
class pathsX(graphX):
    def __init__(self,data,source,target,graphtype):
        super().__init__(data)
        self._source=source
        self._target=target
    def dijkstra(self):
        self._nodes=nx.dijkstra_path(self.graph, self._source, self._target, weight='weight')
    def getData(self):
        """
        return data from shorts path algoritms
        """
        #pathdf=pd.DataFrame([{"name":"path","path":[eval(i) for i in self._nodes]}])
        #print(self._nodes)
        nodesNew=[]
        for i in self._nodes:
            iarr=eval(i)
            nodesNew.append([iarr[1],iarr[0]])
        return nodesNew
class configData:
    def __init__(self,file,sep=";",pycript=False):
        self._data=""
        if pycript:
            from pyodide.http import open_url
            self._data=pd.read_json(open_url(file))
        if file[-4:]==".csv":
            self._data = pd.read_csv(file,sep=";")
        if file[-5:]==".json":
            self._data = pd.read_json(file)
    def getData(self):
        return self._data


data=configData("https://raw.githubusercontent.com/jero98772/AlOtroLado/main/core/data/medellin_data.json",pycript=True).getData()
validateTxt="1234567890.,- []'"

source=input("de donde va(ejemplo:[-75.5764695, 6.2011545]):\n")#"[-75.5764695, 6.2011545]"
target=input("a donde va(ejemplo:[-75.5805063, 6.247958]):\n")#"[-75.5805063, 6.247958]"#
if target=="" or source=="" or  not (validData(target,validateTxt) and  validData(source,validateTxt)):
    print("Datos invalidos, se usaran los datos de ejemplo")
    target="[-75.5805063, 6.247958]"
    source="[-75.5764695, 6.2011545]"

newPath=pathsX(data,str(source), str(target),graphX)
newPath.dijkstra()
nodesData=newPath.getData()
#print(nodesData)                
maps=configMap(nodesData)
maps.show()
maps.save()       
