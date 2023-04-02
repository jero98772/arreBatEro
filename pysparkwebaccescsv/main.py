from pyspark.sql import SparkSession
 
import pandas as pd
#sc=sparkContext.addFile("")
url="https://raw.githubusercontent.com/unloquer/pm25_predict/master/aqa_montesori_nivel_calle.csv"
df=pd.read_csv(url)
print(df)
spark = SparkSession.builder.master("local[1]").appName("test").getOrCreate()
sparkDF=spark.createDataFrame(df) 
sparkDF.printSchema()
sparkDF.show()