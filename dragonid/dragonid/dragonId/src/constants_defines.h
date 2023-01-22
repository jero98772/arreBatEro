#ifndef _CONSTANTS_DEFINES_H
#define _CONSTANTS_DEFINES_H

#define DEBUG
#define txkurid
#define dragonid

//#define webserver
//#define tts
//#define bluetooth
//#define wifi 
//#define opdata

#ifdef webserver
#define wifi
#include <WiFi.h>
WiFi.softAP(ssid);
server.begin();
//Serial.print("Setting AP (Access Point)…");
//IPAddress IP = WiFi.softAPIP();
//Serial.print("AP IP address: ");
//Serial.println(IP);
#endif

#ifdef tts
#include <google-tts.h>
#endif

#define String dogname = "Txakur";
#define String dogtype = "husky";
#define String phone1 = "";
#define String ownername = "jero";
//#define String gender = "Macho";
#define String gender = "Hembra";

//opcional data 
#ifdef optdata
#define String phone2 = "";
#define String address = "";
#define String notes = "";
#define String email = "";
#define String diases = "";
#endif

//---debuging
#ifdef  DEBUG
#define DMSG(args...)       Serial.print(args)
#define DMSGf(args...)      Serial.printf(args)
#define DMSG_STR(str)       Serial.println(str)
#else
#define DMSG(args...)
#define DMSGf(args...)
#define DMSG_STR(str)
#endif 
//----

#ifndef tts || bluetooth || dragonid
Serial.begin(115200); 
#endif

#endif
