#include <Arduino.h>
#include "constants_defines.h"



#ifdef blue
BLEServer *pServer;
BLEService *pService;
BLECharacteristic *pCharacteristic;
#endif

void setup() {
  #ifndef tts || bluetooth || dragonid
  Serial.begin(115200); 
  #endif
}

void loop() {
  #ifdef webserver
  WiFiClient client = server.available();
  if (client) {                            
    //Serial.println("New Client.");          
    String currentLine = "";              
    while (client.connected()) {
      if (client.available()) {
        char c = client.read();             
        Serial.write(c);                   
        header += c;
        if (c == '\n') {                  
          if (currentLine.length() == 0){
            client.println("HTTP/1.1 200 OK");
            client.println("Content-type:text/html");
            client.println("Connection: close");
            client.println();

            client.println("<!DOCTYPE html><html>");
            client.println("<head>");
            client.println("<meta charset='UTF-8'><title>identificacion perro</title></head><body>");
            client.println("<p><h1>yo soy  "+dogname+"</h1></p><hr>");
            if (gender == "Macho"){
                client.println("<p>soy un "+dogtype+" "+gender+"</p><br>");
              }
            else {
                client.println("<p>soy una "+dogtype+" "+gender+"</p><br>");
              }
            client.println("<p>mi dueño se llama <b>"+ownername+"</b>, si no lo vez cerca te agradesco que lo contactes , puedo estar perdido</p>");
            client.println("<p>su telefono es <b>"+phone1+"</b>, te agradesco si lo llamas.</p>");
            #ifdef optdata;
              if (!(phone2 == "" && email == "")){
                client.println("<p>si nesitas otros medios de contacto puedes comunicarlo a</p>");
                if(!(phone2 == "")){
                  client.println("<p>otro es telefono es <b>"+phone2+"</b></p>");
                }
                if(!(email== "")){
                  client.println("<p>su email es <b>"+email+"</b></p>");
                }
              }
              if (!(diases == "")){
                client.println("<br><p>si no lo encuentras te agradesco  que me cuiedes un momento por que sufro de "+diases+"</p>");
              }
              if (!(address == "")){+
                client.println("<p>si no me puedes cuidar me puedes acompañar a mi casa , vivo en "+address+".</p>");
                }
              if (!(notes == "")){
                client.println("<br><p>"+notes+"</p>");
                }
          
            #endif 
            break;
          }else{
            currentLine = "";
          }
        } else if (c != '\r') {  
          currentLine += c;
        }
      }
    }
    header = "";
    client.stop();
    //Serial.println("Client disconnected.");
  }
  #endif
}