#include <oxstd.oxh>

main()
{

	println("GVAR Passo 3 Inicializado");
	// enter code
	decl iQtdVarDependente, iQtdLags, iQtdRegioes, txMatPathG_Matrix, txMatPathResult_Matrix;

	iQtdVarDependente = 2; // NAO MUDAR ISSO !!! (SE MUDAR FAZER REVISAO DO CODIGO)
	iQtdLags = 2;
	iQtdRegioes = 5; // Isso devia vir de um arquivo e configuracao!!!

	// Configuracao dos diretorios
	txMatPathG_Matrix = "./mat_files/G_Matrix/";
	txMatPathResult_Matrix = "./mat_files/Result_Matrix/";
	
	decl mG0, mG0_inv, mGL, mC, iCurrentLag, mAlpha_Stacked, mIIS_Stacked, mU_Staked;

	// Carrega a matrix G0
	println("Carregando matriz G0");
	mG0 = loadmat(sprint(txMatPathG_Matrix, "G0.mat"));

	println("Invertendo matriz G0");
	mG0_inv = invert(mG0);
	
	// Carrega a matrix Stackeds
	println("Carregando matrizes stacked (Alpha, IIS, U)");
	mAlpha_Stacked = loadmat(sprint(txMatPathG_Matrix, "Alpha_Stacked.mat"));
	mIIS_Stacked = loadmat(sprint(txMatPathG_Matrix, "IIS_Stacked.mat"));
	mU_Staked = loadmat(sprint(txMatPathG_Matrix, "U_Staked.mat"));

	// Salva as matrizes de resultado
	println("Salvando matrizes stacked (Alpha, IIS, U)");
	savemat(sprint(txMatPathResult_Matrix, "Result_Alpha.mat"), (mG0_inv * mAlpha_Stacked));
	savemat(sprint(txMatPathResult_Matrix, "Result_IIS.mat"), (mG0_inv * mIIS_Stacked));
	savemat(sprint(txMatPathResult_Matrix, "Result_U.mat"), (mG0_inv * mU_Staked));

	for(iCurrentLag = 1; iCurrentLag <= iQtdLags; ++iCurrentLag)
	{
		// Carrega a matrix de constantes
		println("Carregando matrizes stacked (Alpha, IIS, U)");
		mGL = loadmat(sprint(txMatPathG_Matrix, "G", iCurrentLag, ".mat"));
		savemat(sprint(txMatPathResult_Matrix, "Result_G0xG", iCurrentLag, ".mat"), (mG0_inv * mGL));
	}

	println("GVAR Passo 3 Finalizado");
}
