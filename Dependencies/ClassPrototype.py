
"""
Creation du prototype 

On veut creer un objet qui va stocker plusieurs attributs:
     1) Le dataframe
     2) Le graphe associe
     3) L'encoder scikit learn

    
On doit des la creation de l'objet, formater les donnees.

On rajoute des methodes qui créeront les attributs outliers_interquartile et
outliers_sig
    
Par la suite, on creera des methodes qui permettront de fit et predict et qui 
ajouteront les attributs suivants:
    1) les addresses suspects
    2) les contrats suspects

    
    
Enfin, on peut essayer de regrouper les addresses suspectes et les addresses 
"environnantes" en clusters  via une methode et qui cree un attribut clusters
    
"""

import ImportTransactionsGraphs.py 
import ImportContracts.py 

class eth_data:
    def __init__(self,transaction_df,contract_df=None):
        
        
        transaction_df = ImportTransactionsGraphs.format_as_tgraph(transaction_df)
        print("Transaction formatting complete")
        self.transaction_df = transaction_df
        
        if contract_df == None:
            print("No contract data given")
        else:
            contract_df = ImportContracts.add_dist(contract_df)
            print("Contract formatting complete")
            self.contract_df = contract_df
            
        
        self.graph = ImportTransactionsGraphs.create_graph_obj(transaction_df)
        print("Graph object created")
        
        #self.encoder = 
        
        print("Done creating eth_data object")
        
        