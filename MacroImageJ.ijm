setBatchMode(true);
showMessage("Veuillez choisir le dossier qui contient les images");
image_path = getDirectory("Choose a Directory");
showMessage("Veuillez choisir le dossier où il faut sauvegarder les images");
target_path = getDirectory("Choose a Directory");

list_folders = getFileList(image_path);

/*
for (i = 0; i < list_folders.length; i++){
	folder = image_path + list_folders[i] + "maps/";
	File.makeDirectory(target_path + list_folders[i]);
	list_files = getFileList(folder);
	
	for (j =0; j < list_files.length; j++){
		file = folder + list_files[j];
		new_file = target_path + list_folders[i] + list_files[j];
		open(file);
		run("8-bit");
		saveAs("PNG", new_file);
		close();
	}
}
*/
/*
for (i = 0; i < list_folders.length; i++){
	folder = image_path + list_folders[i];
	File.makeDirectory(target_path + list_folders[i]);
	list_files = getFileList(folder);
	
	for (j =0; j < list_files.length; j++){
		if (list_files[j] != "maps/"){
			file = folder + list_files[j];
			new_file = target_path + list_folders[i] + list_files[j];
			open(file);
			saveAs("PNG", new_file);
			close();
		}
	}
}*/

for (i = 0; i < list_folders.length; i++){
	folder = image_path + list_folders[i];
	File.makeDirectory(target_path + list_folders[i]);
	list_files = getFileList(folder);
	
	for (j =0; j < list_files.length; j++){
			file = folder + list_files[j];
			new_file = target_path + list_folders[i] + list_files[j];
			open(file);
			run("8-bit");
			saveAs("PNG", new_file);
			close();
		}
}
