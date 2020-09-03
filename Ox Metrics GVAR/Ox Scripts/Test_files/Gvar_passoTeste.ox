#include <oxstd.oxh>

main()
{
	// Declaracao de variaveis
	decl iQtdVarDependente, iTotalRegions, txMatPath, iCurrentRegion, mLambda, mPhi, mLambda_0, A_0, A_L;

	txMatPath = "../mat_files/";
	//txDbase = "../Database/GrandeRegiao.in7";

	iQtdVarDependente = 2; // NAO MUDAR ISSO !!! (SE MUDAR FAZER REVISAO DO CODIGO)
	iTotalRegions = 5; // ISSO DEVERIA VIR DE UM ARQUIVO DE CONFIGURACAO

	
	iCurrentRegion = 1;
	for(iCurrentRegion = 1; iCurrentRegion <= iTotalRegions; ++iCurrentRegion)
	{

		println("REALIZANDO PROCESSO DA REGIAO ", iCurrentRegion);
	
		// Leitura das matrizes para a regiao especifica
		mLambda = loadmat(sprint(txMatPath, "R", iCurrentRegion, "_Lambda.mat"));
		mLambda_0 = mLambda[][0:iQtdVarDependente-1];
		mLambda = mLambda[][iQtdVarDependente:];
	
		mPhi = loadmat(sprint(txMatPath, "R", iCurrentRegion, "_Phi.mat"));
	
		// Impresao das matrizes no console. (pode ser esligado por questao de performance)
		println(mLambda_0);
		println(mLambda);
		println(mPhi);
	
		// Construção das matrizes A
		decl nCont1;
		for(nCont1 =0; nCont1 <= iQtdVarDependente; ++nCont1)
		{
			if(nCont1 == 0)
			{
				// Gero a matrix A_0
				A_0 = (<1, 0> ** unit(iQtdVarDependente)) +(<0, -1> ** mLambda_0);
				savemat(sprint(txMatPath, "A", iCurrentRegion, "_", nCont1, ".mat"), A_0);
				//println(sprint(txMatPath, "A", iCurrentRegion, "_", nCont1, ".mat"));
				//println(A_0);
			}
			else
			{
				A_L = (<1, 0> ** mPhi[][(iQtdVarDependente*(nCont1-1)):(((iQtdVarDependente*(nCont1-1)) + iQtdVarDependente -1))]) +
				  (<0, 1> ** mLambda[][(iQtdVarDependente*(nCont1-1)):(((iQtdVarDependente*(nCont1-1)) + iQtdVarDependente -1))]);
				savemat(sprint(txMatPath, "A", iCurrentRegion, "_", nCont1, ".mat"), A_L);
				//println(sprint(txMatPath, "A", iCurrentRegion, "_", nCont1, ".mat"));
				//println(A_L);
			}
		}
	
		delete nCont1;
	} // FIM Looping iCurrentRegion
}
