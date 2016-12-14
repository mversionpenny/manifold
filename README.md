
Données artificelles en haute dimension (fichiers .txt):
data/swissRoll.txt
data/brokenSwissRoll.txt
data/helix.txt
data/twinpeaks.txt
data/openBox.txt

Données réelles en haute dimension (fichiers .png):
data/real_data_contours/[genrei]/[genrei]-[alpha]-[theta].png
data/real_data_grey/[genrei]/[genrei]-[alpha]-[theta].png
data/real_data_color/[genrei]/[genrei]-[alpha]-[theta].png

par exemple : data/real_data_contours/apple1/apple1-045-270.png

Données réelles en haute dimension (fichiers .RData):
data/real_data_matrix/segmentation.RData
data/real_data_matrix/grey.RData
data/real_data_matrix/color.RData

Code :
generate_data.r : permet de générer les données artificielles. Pour avoir les mêmes données que nous, le lecteur ne doit pas exécuter ce code car les aspects aléatoires rendront un résultat différent du nôtre.
optimize_parameters.r : Ce fichier permet d'optimiser les paramètres des méthodes Local Linear Embedding et Isomap, et d'exécuter Sammon, Isomap et LLE. 
	- pour Isomap : les données en faible dimension seront stockées pour tous les k à tester dans data/test_isomap_[nomDonnee]/k[i].RData (eg : data/test_isomap_helix/k40.RData) . Des plots sont alors crées dans le répertoire plots/test_isomap_[nomDonnee].png (eg:plots/test_isomap_helix). Cela permet de comparer l'inertie des resultats pour les différents. C'est le k avec l'inertie la plus grande qi doit être prise.
	- pour LLE : Les k sont testés avec la fonction calc_k. Pour le jeu de donnée testé, cela crée un fichier dans data/test_lle/[nomDonnee].txt qui associe des , à une valer de rho. C'est le k dont le rho est le plus faible qu'il faut choisir.
trustworthiness_continuity.r : implémente les notions de fiabilité et continuité, décrites dans le rapport. Peut calculer les résultats pour les jeux de données artificiels. 
read_real_data.r : permet de changer les fichiers png de données réeles en fichier RData
plot_results.r : permet de faire un plot des données réelles ou artifcielles, en faible dimension.

Donnée réelles:
https://www.mpi-inf.mpg.de/departments/computer-vision-and-multimodal-computing/research/object-recognition-and-scene-understanding/analyzing-appearance-and-contour-based-methods-for-object-categorization/