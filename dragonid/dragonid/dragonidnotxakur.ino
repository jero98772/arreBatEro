#include <Talkie.h>
//#include <TalkieUtils.h>
//#include <Vocab_US_Large.h>
//#include <Vocab_Special.h>

#include "BluetoothSerial.h"


BluetoothSerial SerialBT;
String txt;
void setup() {
  Serial.begin(115200);
  SerialBT.begin("Dragonid");
 }

void loop() {
 if(SerialBT.available()>0){
  SerialBT.readStringUntil('\n');
    txt = SerialBT.readStringUntil('\n');
    Serial.println(txt);
    if(txt == "hola") {
       Serial.println("Info");
    }
}}