# Fonctions de detection d outliers

# Packages 

# basic 
import pandas as pd 
import numpy as np

# Files


# Visualisation


# Graphes 

from graph_tool.all import *

# Scikit-Learn 




# Avec l'écart interquartile

"""
Outlier sur la valeur des transactions

"""

def transaction_outlier(graph,coeff_lambda=1.5):
    """
    Prend en arguments le graphe de transactions à étudier, et lambda permettant d'ajuster le seuil 
    Renvoie les outliers selon le montant des transactions/eweights associés au graphe 
    sous forme d'un DF pandas 
    
    """
    g = graph
    ew = g.ep['edge_weight']
    
    # On récupère les poids 
    tweights = np.array(ew.a)
    
    # Ecart interquartile
    twq34 = np.quantile(tweights,3/4) # quantile 3/4
    twq14 = np.quantile(tweights,1/4) # quantile 1/4
    tauiq = twq34 - twq14
    
    # Seuillage
    treshold = twq34 + coeff_lambda*tauiq
    index = np.argwhere(tweights >  treshold) # Index des transactions outliers
    values = tweights[tweights > treshold]   # Montant des transactions outliers 
    
    # Préparation du tableau
    
    d_out_e = pd.DataFrame(index,columns=['e_index']) # tableau des arêtes outliers
    df_edges = pd.DataFrame(g.get_edges(),columns=['from','to','index']) # tableau complet des arêtes    
    
    d_out_e.set_index('e_index',inplace=True) # changement des index pour un inner join
    df_edges.set_index('index',inplace=True) # changement des index pour un inner join    
    
    df_edges_out = d_out_e.merge(df_edges,left_index=True,right_index=True) # jointure interieure
    
    
    # On réencode les adresses
    df_edges_out['from_address'] = Encoder.inverse_transform(df_edges_out['from'])
    df_edges_out['to_address'] = Encoder.inverse_transform(df_edges_out['to'])
    df_edges_out['value'] = values
    return(df_edges_out)       

"""

Outlier sur les centralités d'intermédiarité

"""

def between_outlier(graph,coeff_lambda=1.5,nodes=True):
    """
    Prend en arguments le graphe de transactions à étudier, et lambda permettant d'ajuster le seuil 
    Renvoie les outliers au sens de la centralité d'intermédiarité 
    sous forme d'un DF pandas 
    
    """
    g = graph
    
    # On récupère les centralités d'intermédiarité de chaque noeuds (bv) et chaque arête (be)
    bv, be = betweenness(g)
    
    # On récupère les valeurs dans un array numpy
    arraybv = np.array(bv.a)
    arraybe = np.array(be.a)
    
    if nodes:
        q34 = np.quantile(arraybv,3/4)
        q14 = np.quantile(arraybv,1/4)
        
        # Ecart interquartile
        tauiq = q34 - q14
        
        # Seuillage
        treshold  = q34 + coeff_lambda*tauiq
        outliers = arraybv[arraybv > treshold]
        outliers_index = np.argwhere([arraybv > treshold])[:,1]
        
        
        # On prépare le tableau 
        
        df = pd.DataFrame(outliers_index,columns=['v_index'])
        df['address'] = Encoder.inverse_transform(df['v_index'])
        df['betweeness'] = outliers
        
        return(df)
    
    else:

        q34 = np.quantile(arraybe,3/4)
        q14 = np.quantile(arraybe,1/4)
        
        # Ecart interquartile
        tauiq = q34 - q14
        
        # Seuillage
        treshold  = q34 + coeff_lambda*tauiq
        outliers = arraybe[arraybe > treshold]
        outliers_index = np.argwhere(arraybe > treshold)
        
        
        # On prépare le tableau   
        d_out_e = pd.DataFrame(outliers_index,columns=['e_index']) # tableau des arêtes outliers
        df_edges = pd.DataFrame(g.get_edges(),columns=['from','to','index']) # tableau complet des arêtes
        
        d_out_e.set_index('e_index',inplace=True) # changement des indexs pour un inner join
        df_edges.set_index('index',inplace=True) # changement des indexs pour un inner join
        
        df_edges_out = d_out_e.merge(df_edges,left_index=True,right_index=True) # jointure interieure
        
        # On réencode les adresses
        df_edges_out['from_address'] = Encoder.inverse_transform(df_edges_out['from'])
        df_edges_out['to_address'] = Encoder.inverse_transform(df_edges_out['to'])
        df_edges_out['betweeness'] = outliers
        
        return(df_edges_out)
    

"""

Outlier sur le nombre de transactions paralleles

"""

# La fréquence des transactions parallèles (même source, même destinataires)

def parallel_outlier(graph,coeff_lambda=1.5):
    """
    Prend en arguments le graphe de transactions à étudier, et lambda permettant d'ajuster le seuil 
    Renvoie les outliers au sens des transactions parallèles 
    sous forme d'un DF pandas 
    
    """
    g = graph
    
    # On récupère les arêtes parallèles et on stocke dans un numpy array
    
    para = label_parallel_edges(g)
    nb_para = np.array(para.a)
    
    # Ecart interquartile
    q34 = np.quantile(nb_para,3/4) # quantile 3/4
    q14 = np.quantile(nb_para,1/4) # quantile 1/4
    tauiq = q34 - q14
    
    # Seuillage
    treshold = q34 + coeff_lambda*tauiq
    outliers_index = np.argwhere(nb_para > treshold)
    outliers = nb_para[nb_para > treshold]
    
    
    
    # On prépare le tableau   
    d_out_e = pd.DataFrame(outliers_index,columns=['e_index']) # tableau des arêtes outliers
    df_edges = pd.DataFrame(g.get_edges(),columns=['from','to','index']) # tableau complet des arêtes
        
    d_out_e.set_index('e_index',inplace=True) # changement des indexs pour un inner join
    df_edges.set_index('index',inplace=True) # changement des indexs pour un inner join
        
    df_edges_out = d_out_e.merge(df_edges,left_index=True,right_index=True) # jointure interieure
        
    # On réencode les adresses
    df_edges_out['from_address'] = Encoder.inverse_transform(df_edges_out['from'])
    df_edges_out['to_address'] = Encoder.inverse_transform(df_edges_out['to'])
    df_edges_out['number_parallel_transactions'] = outliers
    
    return(df_edges_out)

# Avec la variance 
    
"""
Pour les transactions

"""

# Le montant des transactions 

def transaction_outlier2(graph,coeff_sigma=3):
    """
    Prend en arguments le graphe de transactions à étudier, et lambda permettant d'ajuster le seuil 
    Renvoie les outliers selon le montant des transactions/eweights associés au graphe 
    sous forme d'un DF pandas 
    
    """
    g = graph
    ew = g.ep['edge_weight']
    
    # On récupère les poids 
    tweights = np.array(ew.a)
    
    # Ecart interquartile
    mean = np.mean(tweights)
    std = np.std(tweights)
    
    # Seuillage
    treshold = mean + coeff_sigma*std
    index = np.argwhere(tweights >  treshold) # Index des transactions outliers
    values = tweights[tweights > treshold]   # Montant des transactions outliers 
    
    # Préparation du tableau
    
    d_out_e = pd.DataFrame(index,columns=['e_index']) # tableau des arêtes outliers
    df_edges = pd.DataFrame(g.get_edges(),columns=['from','to','index']) # tableau complet des arêtes    
    
    d_out_e.set_index('e_index',inplace=True) # changement des index pour un inner join
    df_edges.set_index('index',inplace=True) # changement des index pour un inner join    
    
    df_edges_out = d_out_e.merge(df_edges,left_index=True,right_index=True) # jointure interieure
    
    
    # On réencode les adresses
    df_edges_out['from_address'] = Encoder.inverse_transform(df_edges_out['from'])
    df_edges_out['to_address'] = Encoder.inverse_transform(df_edges_out['to'])
    df_edges_out['value'] = values
    return(df_edges_out)       

"""

Pour les centralités d'intermediarite

"""

def between_outlier2(graph,coeff_sigma=3,nodes=True):
    """
    Prend en arguments le graphe de transactions à étudier, et lambda permettant d'ajuster le seuil 
    Renvoie les outliers au sens de la centralité d'intermédiarité 
    sous forme d'un DF pandas 
    
    """
    g = graph
    
    # On récupère les centralités d'intermédiarité de chaque noeuds (bv) et chaque arête (be)
    bv, be = betweenness(g)
    
    # On récupère les valeurs dans un array numpy
    arraybv = np.array(bv.a)
    arraybe = np.array(be.a)
    

    
    if nodes:
        meanbv = np.mean(arraybv)
        stdbv = np.std(arraybv)
        

        
        # Seuillage
        treshold  = meanbv + coeff_sigma*stdbv
        outliers = arraybv[arraybv > treshold]
        outliers_index = np.argwhere([arraybv > treshold])[:,1]
        
        
        # On prépare le tableau 
        
        df = pd.DataFrame(outliers_index,columns=['v_index'])
        df['address'] = Encoder.inverse_transform(df['v_index'])
        df['betweeness'] = outliers
        
        return(df)
    
    else:

        meanbe = np.mean(arraybe)
        stdbe = np.std(arraybe)
        

        
        # Seuillage
        treshold  = meanbe + coeff_sigma*stdbe
        outliers = arraybe[arraybe > treshold]
        outliers_index = np.argwhere(arraybe > treshold)
        
        
        # On prépare le tableau   
        d_out_e = pd.DataFrame(outliers_index,columns=['e_index']) # tableau des arêtes outliers
        df_edges = pd.DataFrame(g.get_edges(),columns=['from','to','index']) # tableau complet des arêtes
        
        d_out_e.set_index('e_index',inplace=True) # changement des indexs pour un inner join
        df_edges.set_index('index',inplace=True) # changement des indexs pour un inner join
        
        df_edges_out = d_out_e.merge(df_edges,left_index=True,right_index=True) # jointure interieure
        
        # On réencode les adresses
        df_edges_out['from_address'] = Encoder.inverse_transform(df_edges_out['from'])
        df_edges_out['to_address'] = Encoder.inverse_transform(df_edges_out['to'])
        df_edges_out['betweeness'] = outliers
        
        return(df_edges_out)
    

"""
Avec les transactions paralleles

"""
# La fréquence des transactions parallèles (même source, même destinataires)

def parallel_outlier2(graph,coeff_sigma=3):
    """
    Prend en arguments le graphe de transactions à étudier, et lambda permettant d'ajuster le seuil 
    Renvoie les outliers au sens des transactions parallèles 
    sous forme d'un DF pandas 
    
    """
    g = graph
    
    # On récupère les arêtes parallèles et on stocke dans un numpy array
    
    para = label_parallel_edges(g)
    nb_para = np.array(para.a)
    
    meanpara = np.mean(nb_para)
    stdpara = np.std(nb_para)
    
    
    
    
    
    # Seuillage
    treshold = meanpara + coeff_sigma*stdpara
    outliers_index = np.argwhere(nb_para > treshold)
    outliers = nb_para[nb_para > treshold]
    
    
    
    # On prépare le tableau   
    d_out_e = pd.DataFrame(outliers_index,columns=['e_index']) # tableau des arêtes outliers
    df_edges = pd.DataFrame(g.get_edges(),columns=['from','to','index']) # tableau complet des arêtes
        
    d_out_e.set_index('e_index',inplace=True) # changement des indexs pour un inner join
    df_edges.set_index('index',inplace=True) # changement des indexs pour un inner join
        
    df_edges_out = d_out_e.merge(df_edges,left_index=True,right_index=True) # jointure interieure
        
    # On réencode les adresses
    df_edges_out['from_address'] = Encoder.inverse_transform(df_edges_out['from'])
    df_edges_out['to_address'] = Encoder.inverse_transform(df_edges_out['to'])
    df_edges_out['number_parallel_transactions'] = outliers
    
    return(df_edges_out)


