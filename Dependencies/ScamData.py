# Recupere les données liées aux scams et aux exchanges 

# importing the requests library 
import requests 
import pandas as pd
from bs4 import BeautifulSoup
import numpy as np

"""
Etherscam

"""

def get_ethscam_data():

    # adresse de l'api
    URL = "https://etherscamdb.info/api/addresses"
    r = requests.get(url = URL) 
  
    # extraction du json
    data = r.json() 
    
    # import en DF pandas
    df = pd.DataFrame(data=data['result'])
    
    return(df)

"""

Parsing et formatage de ethscam

"""

def parse_all_ethscam():

    
    # On importe d'abord l'ensemble des données non formatées du site 
    scam_data = get_ethscam_data()
    
    # Indices des adresses scams uniquement liées a l'ETH
    ind = scam_data.iloc[2] == "ETH" 
    ind = np.argwhere(ind)[:,0]
    
    # Création du tableau "propre"
    
    df = pd.DataFrame()
    
    # Récupération des addresses 
    addresses = np.array(scam_data.iloc[:,ind].columns)
    
    # Récupération des descriptifs 
    prec1 = np.array(scam_data.iloc[1,ind])
    prec2 = np.array(scam_data.iloc[6,ind])
    prec = []
    for i,j in zip(prec1,prec2):
        prec.append(i+" "+j)
    
    # On remplit le tableau 
    df['addresses'] = addresses
    df['precision'] = prec
    
    # On retire les adresses non ETH 
    non_eth_ind = np.argwhere([i[0:2] != '0x' for i in df.addresses])[:,0]
    df = df.drop(non_eth_ind)
    
    # On retire les transactions 
    non_addresses = np.argwhere([len(i) > 42 for i in df.addresses])[:,0]
    df = df.drop(non_addresses)
    
    """
    On complète le tableau avec les balances et les transactions 
    https://api.etherscan.io/api?module=account&action=balancemulti&address={}&tag=latest&apikey=B16PQFQJ9UPYNBP8R6YXF3631EVRTBHUFZ
    EXTRACTION DES BALANCES 
    """
    
    urls = []
    n = 1
    url = str()
    n_iters = np.ceil(df.shape[0]/20) # Le nombre de requetes nécessaires, on ne peut en faire que par batch de 20
    compteur = 0
    
    for add in df.addresses:
        if n%20 != 0:
            url = url + add +',' 
            n += 1
            
            
            
            
        else:
            url = url + add 
            urls.append(url)
            url = str()
            n = 1
            compteur += 1 
            
    if compteur != n_iters:
        url = str()
        for add in df.addresses[(compteur*20):]:
            url = url + add +',' 
            
        url = url[:-1]    
        urls.append(url)
    
    
    balance = []
    for url in urls:
        
        # adresse de l'api
        current_url = "https://api.etherscan.io/api?module=account&action=balancemulti&address={}&tag=latest&apikey=B16PQFQJ9UPYNBP8R6YXF3631EVRTBHUFZ".format(url)
        r = requests.get(url = current_url) 
  
        # extraction du json
        data = r.json() 
    
        res = data['result']
        df_res = pd.DataFrame(data=res)
        df_res['balance'] = df_res['balance'].astype(float)*10**(-18)
        curr_bal = list(df_res['balance'].values)
        balance += curr_bal
        
    df['balance'] = balance
    
    
    """
    EXTRACTION DU NOMBRE DE  TXNS
    NB: IL Y A UN ECART ENTRE LE NB OBTENU SUR ETHERSCAN ET INFURA, ON PREND LA VALEUR ETHERSCAN LORS DE LA FUSION
    """
    API_ENDPOINT = "https://mainnet.infura.io/v3/c6a6f0e057ec465b90b5b85c5cdb5990"
    
    data_list = []
    for j in df.addresses:
        data_list.append({"jsonrpc":"2.0","method":"eth_getTransactionCount","params": [j,"latest"],"id":1})
    
    res_list = []

    for j in data_list:
        data = j
        headers = {"Content-Type": "application/json"}
    
        # sending post request and saving response as response object 
        r = requests.post(url = API_ENDPOINT, json = data, headers=headers) 
  
        # extracting response text  
        pastebin_url = r.text 

        dict_res = eval(pastebin_url)
        res = dict_res['result']
        res = int(res,16)
        res_list.append(res)
        
    
    df['txn'] = res_list
    
    return(df,res_list)


"""
Etherscan.io

"""

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
    

def parse_one_page(start_page="https://etherscan.io/accounts/label/phish-hack?sort=balance&order=desc&ps=100"):
    
    source = requests.get(start_page).text # On récupère le texte html complet
    soup = BeautifulSoup(source,'lxml')    # On crée une instance de parser 
    
    extract_data = soup.find('div',class_="table-responsive mb-2 mb-md-0") # on récupère l'ensemble du tableau
    tableau = extract_data.find('tbody') # on récupère le corps du tableau
    
    lignes = tableau.find_all('tr') # recuperer la liste de toutes les lignes du tableau avec balise (itérable)
    
    addresses = []
    precision = []
    balance = []
    txn = []

    for i in lignes:
        add = i.find('td').find('a') # Address
        preci = i.find_all('td')[1] # ID
        bal = i.find_all('td')[2] # balance
        tx = i.find_all('td')[3] # TxN count
    
        add = remove_html_markup(add)
        preci = remove_html_markup(preci)
        bal = remove_html_markup(bal.text)
        tx = remove_html_markup(tx)
    
        addresses.append(add)
        precision.append(preci)
        balance.append(bal)
        txn.append(tx)
    

    return(addresses,precision,balance,txn)
    
    
    
    
    


 
def parse_all_ethscan(start_page="https://etherscan.io/accounts/label/phish-hack?sort=balance&order=desc&ps=100"):
    
    # Initialisation des listes
    addresses = []
    precision = []
    balance = []
    txn = []
    
    # Nombre de pages
    nb_pages = recup_nb_pages()
    
    nb_pagelist = [i+1 for i in range(nb_pages)]
    
    
    for i in nb_pagelist:
        page = "https://etherscan.io/accounts/label/phish-hack/{}?ps=100&sort=balance&order=desc&".format(i)
        add,prec,bal,tx = parse_one_page(page) 
        addresses += add
        precision += prec
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
    data = {"addresses":addresses,"precision":precision,"balance":balance,"txn":txn}
    df = pd.DataFrame(data=data)
    
    return(df)

