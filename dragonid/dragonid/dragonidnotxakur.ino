#include <Talkie.h>
//#include <TalkieUtils.h>
//#include <Vocab_US_Large.h>
//#include <Vocab_Special.h>

#include "BluetoothSerial.h"


BluetoothSerial SerialBT;
Talkie voice;

void setup() {
  Serial.begin(115200);
  SerialBT.begin("Dragonid");
 }

void loop() {
 if(SerialBT.available()>0){
  SerialBT.readStringUntil('\n');
    String txt = SerialBT.readStringUntil('\n');
    Serial.println(txt);
    for(int i=0;i<txt.length();i++){
    voice.say(txt[i]);
    }
  }
}