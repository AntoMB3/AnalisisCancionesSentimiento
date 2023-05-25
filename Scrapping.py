from selenium import webdriver
from selenium.webdriver.chrome.service import Service
import re

def InicializarServer() -> webdriver:

    ## Leer el driver de Chrome para poder utilizarlo
    path = "chromedriver_win32\chromedriver.exe"
    service = Service(executable_path=path)

    #Activar el driver
    driver = webdriver.Chrome(service=service)
    return driver


def BuscarPalabra(palabra:str):
    driver = InicializarServer()
    try:
        link = f"https://dle.rae.es/{palabra}?m=form"
        driver.get(link)

        texto = driver.find_element(by="xpath",value="//header[@class = "'"f"'"]").text

        for i,letra in enumerate(texto):
            if letra == ',':
                texto = texto[:i]
                break

        texto_limpiado = re.sub(r"[^a-zA-Záéíóú]", "", texto)
        texto = texto_limpiado
    except:
        print("Palabra no encontrada")
        return
    driver.close()
    return texto

#print(BuscarPalabra("cabrón"))




