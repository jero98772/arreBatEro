#include <FastLED.h>
#define LED_PIN 0
#define LED_TYPE WS2812B
#define COLOR_ORDER GRB

#define amarillo CRGB::Yellow
#define negro CRGB::Black
#define rojo CRGB::Red
#define azul CRGB::Blue
#define morado CRGB::Purple
#define naranja CRGB::OrangeRed
#define verde CRGB::Green
#define aguamarina CRGB::Cyan
#define rosado CRGB::Fuchsia
#define verdedos CRGB::LimeGreen
#define raro CRGB::DarkOrchid
#define rarodos CRGB::DeepPink

const uint8_t matrixWidth = 8;
const uint8_t matrixHeight = 8;
#define NUM_LEDS (matrixWidth * matrixHeight)

int BRIGHTNESS = 20;
CRGB leds[matrixWidth * matrixHeight];

int animator = 0;
const int sampleWindow = 50; // Sample window width in mS (50 mS = 20Hz)
unsigned int sample;

int inputMin = -2;
int inputMax = 200;

int rangeMin = 30;

int rangeMiddleMin = 31;
int rangeMiddleMax = 90;

int rangleMax = 91;

void setup() {
  Serial.begin(115200);
  LEDS.addLeds<LED_TYPE, LED_PIN, COLOR_ORDER>(leds, NUM_LEDS);
  FastLED.setBrightness(BRIGHTNESS);
}

#define ESCENAS1 2
CRGB matrix1[ESCENAS1][8][8] = {
  {
    {azul, azul, azul, azul, azul, azul, azul, azul},
    {azul, azul, azul, azul, azul, azul, azul, azul},
    {azul, azul, azul, azul, azul, azul, azul, azul},
    {azul, azul, azul, azul, azul, azul, azul, azul},
    {azul, azul, azul, azul, azul, azul, azul, azul},
    {azul, azul, azul, azul, azul, azul, azul, azul},
    {azul, azul, azul, azul, azul, azul, azul, azul},
    {azul, azul, azul, azul, azul, azul, azul, azul},
  },
  {
    {azul, negro, azul, negro, azul, negro, azul, negro},
    {azul, negro, azul, negro, azul, negro, azul, negro},
    {azul, negro, azul, negro, azul, negro, azul, negro},
    {azul, negro, azul, negro, azul, negro, azul, negro},
    {azul, negro, azul, negro, azul, negro, azul, negro},
    {azul, negro, azul, negro, azul, negro, azul, negro},
    {azul, negro, azul, negro, azul, negro, azul, negro},
    {azul, negro, azul, negro, azul, negro, azul, negro},
  },
};

#define ESCENAS2 2
CRGB matrix2[ESCENAS2][8][8] = {
  {
    {verde, verde, verde, verde, verde, verde, verde, verde},
    {verde, verde, verde, verde, verde, verde, verde, verde},
    {verde, verde, verde, verde, verde, verde, verde, verde},
    {verde, verde, verde, verde, verde, verde, verde, verde},
    {verde, verde, verde, verde, verde, verde, verde, verde},
    {verde, verde, verde, verde, verde, verde, verde, verde},
    {verde, verde, verde, verde, verde, verde, verde, verde},
    {verde, verde, verde, verde, verde, verde, verde, verde},
  },
    {
    {verde, negro, negro, negro, verde, negro, verde, negro},
    {verde, negro, negro, negro, verde, negro, verde, negro},
    {verde, negro, negro, negro, verde, negro, verde, negro},
    {verde, negro, negro, negro, verde, negro, verde, negro},
    {verde, negro, negro, negro, verde, negro, verde, negro},
    {verde, negro, negro, negro, verde, negro, verde, negro},
    {verde, negro, negro, negro, verde, negro, verde, negro},
    {verde, negro, negro, negro, verde, negro, verde, negro},
  },
};

#define ESCENAS3 2
CRGB matrix3[ESCENAS3][8][8] = {
  {
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
    {rojo, rojo, rojo, rojo, rojo, rojo, rojo, rojo},
  },
  {
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
    {negro, negro, rojo, rojo, negro, negro, rojo, negro},
  },
};

#define ESCENAS3 2
CRGB matrix4[ESCENAS3][8][8] = {
  {
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
    {amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo, amarillo},
  },
  {
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
    {negro, negro, amarillo, amarillo, negro, negro, amarillo, negro},
  },
};
void loop() {
  unsigned long startMillis = millis(); // Start of sample window
  unsigned int peakToPeak = 0;

  unsigned int signalMax = 0;
  unsigned int signalMin = 1024;

  // collect data for 50 mS
  while (millis() - startMillis < sampleWindow) {
    sample = analogRead(0);
    if (sample < 1024) {
      if (sample > signalMax) {
        signalMax = sample;
      }
      else if (sample < signalMin) {
        signalMin = sample;
      }
    }
  }

  peakToPeak = signalMax - signalMin;
  int changeAnimation = map(peakToPeak, inputMin, inputMax, 0, 100);

  for (int i = 0; i < matrixHeight; i++) {
    for (int j = 0; j < matrixWidth; j++) {
      // microfono
      // if (changeAnimation < rangeMin) {
      //   leds[i * matrixWidth + j] = matrix1[animator % ESCENAS1][i][j];
      // } else if (changeAnimation > rangeMiddleMin && changeAnimation < rangeMiddleMax) {
      //   leds[i * matrixWidth + j] = matrix2[animator % ESCENAS2][i][j];
      // } else if (changeAnimation > rangleMax) {
      //   leds[i * matrixWidth + j] = matrix3[animator % ESCENAS3][i][j];
      // }
      // contador
      if (animator < rangeMin) {
        leds[i * matrixWidth + j] = matrix2[animator % ESCENAS1][i][j];
         FastLED.setBrightness(30);
      } else if (animator > rangeMiddleMin && animator < rangeMiddleMax) {
        leds[i * matrixWidth + j] = matrix4[animator % ESCENAS2][i][j];
         FastLED.setBrightness(50);
      } else if (animator > rangleMax) {
        leds[i * matrixWidth + j] = matrix3[animator % ESCENAS3][i][j];
         FastLED.setBrightness(100);
      }
      
      // if (changeAnimation < 10) {
      //   FastLED.setBrightness(0);
      //   FastLED.clear();
      // } else {
      //   FastLED.setBrightness(100);
      // }
      FastLED.show();
    }
  }

  animator = (animator + 1) % 100;
  Serial.print("map value: ");
  Serial.println(changeAnimation);
  Serial.print("peak value: ");
  Serial.println(peakToPeak);
}