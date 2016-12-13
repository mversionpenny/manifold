setBatchMode(true);
showMessage("Veuillez choisir le dossier qui contient les images");
path = getDirectory("Choose a Directory");
list_folders = getFileList(path);
for (i = 0; i < list_folders.length; i++){
	folder = path + list_folders[i];
	list_files = getFileList(folder);
	for (j = 0; j < list_files.length; j++){
		file = folder + list_files[j];
		open(file);
		run("8-bit");
		saveAs("PNG", file);
	}
}
