#include <oxstd.oxh>

main()
{
	// enter code
	decl iQtdVarDependente, iQtdRegioes, iQtdLags, txMatPathW_Matrix, txMatPathA_Matrix, txMatPathG_Matrix, txMatPathRaw_Matrix, sVarSufix;

	iQtdVarDependente = 2; // NAO MUDAR ISSO !!! (SE MUDAR FAZER REVISAO DO CODIGO)
	iQtdLags = 2;
	iQtdRegioes = 5; // Isso devia vir de um arquivo e configuracao!!!

	// Configuracao dos diretorios
	txMatPathG_Matrix = "./mat_files/G_Matrix/";
	txMatPathA_Matrix = "./mat_files/A_Matrix/";
	txMatPathW_Matrix = "./mat_files/W_Matrix/";
	txMatPathRaw_Matrix = "./mat_files/RawMatrix/";
	sVarSufix = "D_R";

	decl mWi, mAi_l, GL, mUi, mUStack, mAlphai, aAlphaiStack, mIISi, mIISiStack,  iContRegion, iCurrentLag;

	for(iCurrentLag = 0; iCurrentLag <= iQtdLags; ++iCurrentLag)
	{
	
		for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	    {
			// Carrega a matriz de pesso da regiao.
			mWi = loadmat(sprint(txMatPathW_Matrix, "W", iContRegion,".mat"));
			
			// Carrega a Matriz de coeficientes
			mAi_l = loadmat(sprint(txMatPathA_Matrix, "A", iContRegion, "_", iCurrentLag, ".mat"));
			
			// primeira passagem, inicializo o G0 e GL
			if(iContRegion==1)
			{
				GL =  mAi_l * mWi;
			}
			else
			{
				GL = GL | (mAi_l * mWi);
			}
			//println(GL);
	    }
	
		savemat(sprint(txMatPathG_Matrix, "G", iCurrentLag, ".mat"), GL);
	}

	// PROCESSO DE CONSTRUCAO DA MATRIZ DE CONSTANTES E SEASONAL
	println("Realizando processo de stacking das matrizes U (Cosntantes e Seasons)");
	for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	{
		mUi = loadmat(sprint(txMatPathRaw_Matrix, sVarSufix, iContRegion, "_U.mat"));

		// primeira passagem, inicializo o G0 e GL
		if(iContRegion==1)
		{
			mUStack =  mUi;
		}
		else
		{
			mUStack = mUStack | mUi;
		}

		savemat(sprint(txMatPathG_Matrix, "U_Staked", ".mat"), mUStack);
	}


	// PROCESSO DE CONSTRUCAO DA MATRIZ ALPHA
	println("Realizando processo de stacking das matrizes Alpha");
	for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	{
		mAlphai = loadmat(sprint(txMatPathRaw_Matrix, sVarSufix, iContRegion, "_Alpha.mat"));

		// primeira passagem, inicializo o G0 e GL
		if(iContRegion==1)
		{
			aAlphaiStack =  mAlphai;
		}
		else
		{
			aAlphaiStack = aAlphaiStack | mAlphai;
		}

		savemat(sprint(txMatPathG_Matrix, "Alpha_Stacked", ".mat"), aAlphaiStack);
	}


	// PROCESSO DE CONSTRUCAO DA MATRIZ ALPHA
	println("Realizando processo de stacking das matrizes IIS");
	for(iContRegion=1; iContRegion<=iQtdRegioes; ++iContRegion)
	{
		mIISi = loadmat(sprint(txMatPathRaw_Matrix, sVarSufix, iContRegion, "_IIS.mat"));

		// primeira passagem, inicializo o G0 e GL
		if(iContRegion==1)
		{
			mIISiStack =  mIISi;
		}
		else
		{
			mIISiStack = mIISiStack | mIISi;
		}

		savemat(sprint(txMatPathG_Matrix, "IIS_Stacked", ".mat"), mIISiStack);
	}
	
}
