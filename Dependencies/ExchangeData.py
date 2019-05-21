# Recupere les données liées aux scams et aux exchanges 

# importing the requests library 
import requests 
import pandas as pd
from bs4 import BeautifulSoup


def remove_html_markup(s):
    tag = False
    quote = False
    out = ""

    for c in s:
            if c == '<' and not quote:
                tag = True
            elif c == '>' and not quote:
                tag = False
            elif (c == '"' or c == "'") and tag:
                quote = not quote
            elif not tag:
                out = out + c

    return out


def recup_nb_pages(start_page="https://etherscan.io/accounts/label/phish-hack?sort=balance&order=desc&ps=100"):
    
    
    source = requests.get(start_page).text
    soup = BeautifulSoup(source,'lxml') 
    nb_page = soup.find_all('strong',class_="font-weight-medium") # on récupère la balise 
    nb = int(remove_html_markup(nb_page[1])) 
    
    return(nb)


def parse_exchange(start_page="https://etherscan.io/accounts/label/exchange/1?ps=100"):
    
    source = requests.get(start_page).text
    soup = BeautifulSoup(source,'lxml') 
    extract_data = soup.find('div',class_="table-responsive mb-2 mb-md-0") # on récupère l'ensemble du tableau
    tableau = extract_data.find('tbody') # on récupère le corps du tableau
    lignes = tableau.find_all('tr')
    addresses = []
    name = []
    balance = []
    txn = []

    for i in lignes:
        add = i.find('td').find('a') # Address
        nam = i.find_all('td')[1] # Nom
        bal = i.find_all('td')[2] # balance
        tx = i.find_all('td')[3] # TxN count
    
        add = remove_html_markup(add)
        nam = remove_html_markup(nam)
        bal = remove_html_markup(bal.text)
        tx = remove_html_markup(tx)
    
        addresses.append(add)
        name.append(nam)
        balance.append(bal)
        txn.append(tx)
    
    return(addresses,name,balance,txn)


def parse_all_exchanges():
    
    addresses = []
    name = []
    balance = []
    txn = []
    
    # Nombre de pages
    nb_pages = recup_nb_pages("https://etherscan.io/accounts/label/exchange/1?ps=100")
    
    nb_pagelist = [i+1 for i in range(nb_pages)]    
    
    for i in nb_pagelist:
        page = "https://etherscan.io/accounts/label/exchange/{}?ps=100".format(i)
        add,nam,bal,tx = parse_exchange(page) 
        addresses += add
        name += nam
        balance += bal
        txn += tx
    

    # Formatage de balance et de txn
    for i in range(len(balance)):
        if "Ether" in balance[i]:
            balance[i] = balance[i].strip("Ether")
            balance[i] = float(balance[i].replace(',', ''))
            
        else:
            balance[i] = balance[i].strip("wei")
            balance[i] = float(balance[i].replace(',', ''))
            balance[i] = balance[i]*10**(-18)
        
        if "," in txn[i]:
            txn[i] = int(txn[i].replace(',', ''))
        else:
            txn[i] = int(txn[i])
    
    
    # tableau pandas 
    data = {"addresses":addresses,"name":name,"balance":balance,"txn":txn}
    df = pd.DataFrame(data=data)
    
    return(df)    