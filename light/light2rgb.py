def wavelength_to_rgb(wavelength):
    """Convierte longitud de onda (nm) a color RGB aproximado."""
    if wavelength < 380 or wavelength > 750:
        return (0, 0, 0)  # fuera del rango visible

    if wavelength < 440:
        r = -(wavelength - 440) / (440 - 380)
        g = 0.0
        b = 1.0
    elif wavelength < 490:
        r = 0.0
        g = (wavelength - 440) / (490 - 440)
        b = 1.0
    elif wavelength < 510:
        r = 0.0
        g = 1.0
        b = -(wavelength - 510) / (510 - 490)
    elif wavelength < 580:
        r = (wavelength - 510) / (580 - 510)
        g = 1.0
        b = 0.0
    elif wavelength < 645:
        r = 1.0
        g = -(wavelength - 645) / (645 - 580)
        b = 0.0
    else:
        r = 1.0
        g = 0.0
        b = 0.0

    # Ajustar la intensidad
    if wavelength < 420:
        factor = 0.3 + 0.7 * (wavelength - 380) / (420 - 380)
    elif wavelength > 700:
        factor = 0.3 + 0.7 * (750 - wavelength) / (750 - 700)
    else:
        factor = 1.0

    r = int(round(r * factor * 255))
    g = int(round(g * factor * 255))
    b = int(round(b * factor * 255))

    return (r, g, b)

# Ejemplo
for wl in range(380, 751, 10):
    print(f"{wl} nm: {wavelength_to_rgb(wl)}")
