
Données artificelles en haute dimension (fichiers .txt):
data/swissRoll.txt
data/brokenSwissRoll.txt
data/helix.txt
data/twinpeaks.txt
data/openBox.txt

Données réelles en haute dimension (fichiers .png):
data/real_data_contours/<genrei>/<genrei>-<alpha>-<theta>.png
data/real_data_grey/<genrei>/<genrei>-<alpha>-<theta>.png
data/real_data_color/<genrei>/<genrei>-<alpha>-<theta>.png

par exemple : data/real_data_contours/apple1/apple1-045-270.png

Données réelles en haute dimension (fichiers .RData):
data/real_data_matrix/segmentation.RData
data/real_data_matrix/grey.RData
data/real_data_matrix/color.RData

Code :
generate_data.r : permet de générer les données artificielles. Pour avoir les mêmes données que nous, le lecteur ne doit pas exécuter ce code car les aspects aléatoires rendront un résultat différent du nôtre.

Donnée réelles:
https://www.mpi-inf.mpg.de/departments/computer-vision-and-multimodal-computing/research/object-recognition-and-scene-understanding/analyzing-appearance-and-contour-based-methods-for-object-categorization/