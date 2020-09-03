#include <oxstd.oxh>

main()
{
	// enter code
	decl iQtdLags, txMatPath, txDbase, iQtdVarDependente, iQtdRegioes;

	iQtdVarDependente = 2; // NAO MUDAR ISSO !!! (SE MUDAR FAZER REVISAO DO CODIGO)
	iQtdLags = 2;
	iQtdRegioes = 5; // Isso devia vir de um arquivo e configuracao!!!

	// Configuracao dos diretorios
	txMatPath = "./mat_files/";
	txDbase = "../Database/GrandeRegiao.in7";

	decl mG0, mG0_inv, mGL, mC, iCurrentLag;


	// Carrega a matrix G0
	mG0 = loadmat(sprint(txMatPath, "G0.mat"));

	mG0_inv = invert(mG0);
	println(mG0);
	println(mG0_inv);

	// Carrega a matrix de constantes
	mC = loadmat(sprint(txMatPath, "C.mat"));

	println(mG0_inv * mC);
	
	for(iCurrentLag = 1; iCurrentLag <= iQtdLags; ++iCurrentLag)
	{
	
		// Carrega a matrix de constantes
		mGL = loadmat(sprint(txMatPath, "G", iCurrentLag, ".mat"));

		println(mG0_inv * mGL);
	}
}
