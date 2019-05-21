# Fonctions d'aggregation, d'importation des csv, de creation des graphes

# Packages
# basic 
import pandas as pd 
import numpy as np

# Files
import glob

# Graphes 

from graph_tool.all import *

# Scikit-Learn 
from sklearn.preprocessing import LabelEncoder

"""
Agregation des csv en un seul csv

"""

def aggregate_csv():
    """
    Concatene les csv présents dans le répertoire courant, 
    Renvoie un pandas DF résultant de la concaténation
    
    """
    extension = 'csv'
    all_filenames = [i for i in glob.glob('*.{}'.format(extension))]
    combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames ])
    combined_csv.to_csv( "combined_csv.csv", index=False, encoding='utf-8-sig')
    return(combined_csv)
    
"""
Formatage des donnees transactionnelles 

"""

def format_as_tgraph(data):
    """
    Formatage d'un pandas DF en un pandas DF adapté
    à un import en graphe de transactions: 
    
    Renvoie un pandas DF dans un format adapté a un import graphique
    
    """
    
    # On transforme les addresses en str pour éviter les ennuis
    data[['from_address', 'to_address']] = data[['from_address', 'to_address']].astype(str)

    
    # On concatene les addresses pour éviter les doublons 
    
    full_addresses = np.append(data['from_address'].values,data['to_address'].values)
    
    
    # Encodage des addresses sur la liste ENTIERE des addresses 
    Encoder = LabelEncoder()
    Encoder.fit(full_addresses) #Apprend sur les addresses 

    new_from = Encoder.transform(data['from_address']) # On transforme
    new_to = Encoder.transform(data['to_address'])     # On transforme
    
    
    # Remplacement des colonnes par les colonnes ainsi encodées
    data['from_address'] = new_from
    data['to_address'] = new_to
    
    # On transforme la colonne value en float
    data[['value']] = data[['value']].astype(float)
    
    
    return(data,Encoder)
    
"""
Creation d'un objet graph_tool a partir des donnees 

"""

def create_graph_obj(data):
    """
    Prend un pandas DF formaté correctement en transaction graph
    Crée et renvoie un objet graph-tool sur lequel on peut travailler
    
    """
    
    # Création du graphe vide
    g = Graph(directed=True) 

    # Ajout des arêtes suivant le np.array
    vmap = g.add_edge_list(data[['from_address','to_address']].values)

    # Ajout des edge weights 
    ew = g.new_edge_property("double")
    ew.a = data['value'].values
    g.ep['edge_weight'] = ew
    
    return(g)

