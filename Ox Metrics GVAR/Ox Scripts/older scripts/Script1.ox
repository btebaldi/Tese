#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

ProcessoPhi(mPhi, const iValue, const sName, const iQtdLags, const iRegDependente)
{
	println("Processo Phi iniciado");

	decl iContRows, iContCols, iContVar, sParamName, asTipo, index;

	asTipo = {"Admitidos","Desligados"};

	for(iContRows=0; iContRows < sizeof(asTipo); ++iContRows)
	{

		// Preenchimento da matrix Lambda
		for(iContCols = 0; iContCols<iQtdLags; ++iContCols)
		{

			for(iContVar = 0; iContVar < sizeof(asTipo); ++iContVar)
			{
				// determina o nome das variaveis
				sParamName = sprint("R", iRegDependente, "_", asTipo[iContVar], "_", iContCols+1, "@R", iRegDependente, "_", asTipo[iContRows]);
	
				index = strfind(sName, sParamName);
				
				if(index >-1)
				{
					mPhi[iContRows][(sizeof(asTipo)*iContCols) + iContVar] = iValue[index];	
				}
			} // Fim: iContVar (looping nos tipos para as colunas (colunas pares e impares))

		} // Fim: iContCols (looping nos lags)

	} // Fim: iContRows (looping nos tipos para as linhas)

	println("Processo Phi finalizado");
	return mPhi;
}

ProcessoLambda(mLambda, const iValue, const sName, const iQtdLags, const iRegDependente)
{
	println("Processo Lambda iniciado");

	decl iContRows, iContCols, iContVar, sParamName, asTipo, index;

	asTipo = {"Admitidos","Desligados"};

	for(iContRows=0; iContRows < sizeof(asTipo); ++iContRows)
	{

		// Preenchimento da matrix Lambda
		for(iContCols = 0; iContCols<=iQtdLags; ++iContCols)
		{

			for(iContVar = 0; iContVar < sizeof(asTipo); ++iContVar)
			{
				// determina o nome das variaveis
				if(iContCols == 0)
				{
					// Determina o nome do parametro sem lag
					sParamName = sprint("star_", asTipo[iContVar], "@R", iRegDependente, "_", asTipo[iContRows]);
				}
				else
				{
					// Determina o nome do parametro COM lag
					sParamName = sprint("star_", asTipo[iContVar], "_", iContCols, "@R", iRegDependente, "_", asTipo[iContRows]);
				}
	
				index = strfind(sName, sParamName);
				
				if(index >-1)
				{
					mLambda[iContRows][(sizeof(asTipo)*iContCols) + iContVar] = iValue[index];	
				}
			} // Fim: iContVar (looping nos tipos para as colunas (colunas pares e impares))

		} // Fim: iContCols (looping nos lags)

	} // Fim: iContRows (looping nos tipos para as linhas)


	println("Processo Lambda finalizado");
	return mLambda;
}



ProcessoU(mU, const iValue, const sName, const iQtdLags, const iRegDependente)
{
	println("Processo U iniciado");

	decl iContRows, iContCols, iContVar, sParamName, asTipo, index, asConstants;

	asTipo = {"Admitidos","Desligados"};

	asConstants = {"Constant","CSeasonal", "CSeasonal_1", "CSeasonal_2", "CSeasonal_3", "CSeasonal_4", "CSeasonal_5", "CSeasonal_6", "CSeasonal_7", "CSeasonal_8", "CSeasonal_9", "CSeasonal_10"};

	for(iContRows=0; iContRows < sizeof(asTipo); ++iContRows)
	{

		// Preenchimento da matrix Lambda
		for(iContCols = 0; iContCols<sizeof(asConstants); ++iContCols)
		{
			// determina o nome das variaveis
			sParamName = sprint(asConstants[iContCols], "@R", iRegDependente, "_", asTipo[iContRows]);

			index = strfind(sName, sParamName);
	
			if(index >-1)
			{
				mU[iContRows][iContCols] = iValue[index];	
			}
		} // Fim: iContCols (looping nos lags)

	} // Fim: iContRows (looping nos tipos para as linhas)

	println("Processo U finalizado");
	return mU;
}



main()
{

	/***************************************************
	 *
	 * Declaração de variaveis de configuracao do script
	 *
	 *************************************************** */

	// Variáveis do programa
	decl iQtdVarDependente, iQtdRegioes, iQtdLags, txMatPath, txDbase;

	iQtdVarDependente = 2; // NAO MUDAR ISSO !!! (SE MUDAR FAZER REVISAO DO CODIGO)
	iQtdLags = 2;
	iQtdRegioes = 5; // Isso devia vir de um arquivo e configuracao!!!
		
	txMatPath = "./mat_files/";
	txDbase = "../Database/GrandeRegiao.in7";
	

	println("Carregando matrix de pessos W");
	decl mW;
	mW = loadmat(sprint(txMatPath, "data.mat"));
	print(mW);

	println("Carregando base de dados");
	decl dbase, mData;
	dbase = new Database();
	dbase.Load(txDbase);
	//dbase.Info();

	println("Carregando dados");
	mData = dbase.GetAll();

	println("Construindo as variaveis estrela");
	//print(mData[][1:5]*mW);
	dbase.Append(mData[][1:iQtdRegioes]*mW, {"star_1", "star_2", "star_3", "star_4", "star_5"});

	//dbase.Info();
	delete dbase;
	

	println("*** Iniciando estimacao dos modelos *** \n");


	decl iCont, iCont2;
	for (iCont = 1; iCont <= iQtdRegioes; ++iCont)
	{
		// print Headder
		println("\n\n*****************************************");	
		println("             Regiao ", iCont);	
		println("*****************************************\n\n");	



		// Inicio um nomo objeto do tipo PcGive
		println("Inicio um nomo objeto do tipo PcGive referente a regiao ", iCont);	
		decl model = new PcGive();

		println("Carregando base de dados para regiao ", iCont);
		model.Load(txDbase);

		println("Adicionando variavel estrela da regiao ", iCont);	
		model.Append(mData[][1:iQtdRegioes]*mW[][iCont-1], {"star_Desligados"});
		model.Append(mData[][(iQtdRegioes+1):(iQtdRegioes*2)]*mW[][iCont-1], {"star_Admitidos"});

		println(model.GetAllNames());
		

		decl Wi_aux1,Wi_aux2, Wi; 
		Wi_aux1 = zeros(1, iQtdRegioes);
		Wi_aux1[][iCont-1] = 1;

		Wi_aux1 = Wi_aux1 ** unit(iQtdVarDependente); 
		
		Wi_aux2 = mW[][iCont-1]';
		Wi_aux2 = Wi_aux2 ** unit(iQtdVarDependente); 
		
		Wi = Wi_aux1 | Wi_aux2;
		//println(">>>>>>>>>>>>>>>>>>>>>>",Wi_aux1 ** unit(iQtdVarDependente));
		println("Wi", Wi);
		//Wi_aux1 = zeros(2, c);

		
		//model.Info();
		model.SetModelClass("SYSTEM");
		

		println("Construindo modelo de estimacao da regiao ", iCont);	
	  	
		// Deseleciona as variaveis
		model.DeSelect();

		model.Deterministic(3);
		//iCseason
		//in: int:
		//	-1: no seasonals;
		//	 0: n Seasonals Season, Season_1, ...;
		//	 1: n centred seasonals CSeason, CSeason_1;
		//	 2: 1 seasonal called Seasonal;
		//	 3: 1 centred seasonal called CSeasonal.
		//Appends constant, trend and seasonals to the database. These variables are named Constant, Trend and Season. Season_1, ..., Season_x, where x is the frequency.
		//
		//Season has a 1 in quarter 1 (for quarterly data), and zeros elsewhere, Season_1 has a 1 in quarter 2, etc.
		//If iCseason is 0, normal seasonals are created. If iCseason is 1, the seasonals are centred (with quarterly observations, for quarter 1: 0.75, -0.25, -0.25, -0.25, ...), in which case the names are CSeason, CSeason_1, ..., CSeason_x. No seasonals are created if iCseason is < 0.

		
		// adiciona variavel dependente
		model.Select("Y", {sprint("R", iCont,"_Admitidos"), 0, 0});
		model.Select("Y", {sprint("R", iCont,"_Desligados"), 0, 0});

		
		// adiciona lag de Y e eventuais endogenos que sejam importantes
		for (iCont2 = 1; iCont2 <= 5; ++iCont2)
		{
			// Adiciona os lags da variavel
			if(iCont2 == iCont)
			{
				// Se houver lags para ser adicionados adiciona os mesmos
				if(iQtdLags > 0)
				{
				// adiciona a variavel dependente (lag da independente)
				model.Select("X", {sprint("R", iCont2,"_Admitidos"), 1, iQtdLags});
				model.Select("X", {sprint("R", iCont2,"_Desligados"), 1, iQtdLags});
				}
			}
			else
			{
			// adiciona a variavel dependente (outras regioes)
			// model.Select("X", {sprint("R", iCont2,"_Admitidos"), 0, 2});
			// model.Select("X", {sprint("R", iCont2,"_Desligados"), 0, 2});
			}

		} // Fim de: iCont2

		// Adiciona a variavel "star"
		model.Select("X", {"star_Desligados", 0, iQtdLags});
		model.Select("X", {"star_Admitidos", 0, iQtdLags});


		// Adiciona variaveis constante e sesonals
		model.Select("U", {"Constant", 0, 0});
		model.Select("U", {"CSeasonal", 0, 10});
		
	 	// determina a janela de tempo do modelo
		model.SetSample(2004,1 ,2016, 12);

		// Liga o autometrics
		model.Autometrics(0.05, "none", 1);

		
		// determina o metodo de estimacao.
		model.SetMethod(M_OLS);

		// Realiza a estimacao do modelo
		model.Estimate();

		//model.TestSummary();

		println("Testando aquisicao de parametros");

		// Declaro as matrizes Phi e Lambda
		// Phi: Matrix de coeficientes do log da dependente
		// Lambda: Matrix de coeficientes das star
		// U: Matrix de coeficientes da constante e das dummies sazonais 
		decl mPhi, mLambda, mLambda_0, mU, iContParam, iTotalParam, asParamNames, vParamValues, nContLags, A_0, A_L;

		// inicia as matizes
		mPhi = zeros(iQtdVarDependente, iQtdVarDependente*iQtdLags);
		mLambda = zeros(iQtdVarDependente, iQtdVarDependente*(iQtdLags+1));
		mU = zeros(iQtdVarDependente, (1+11));	//1: constante - 11:Seasonal

		// inicializa o total de parametros
		iTotalParam = model.GetParCount();

		// inicializa um vetor com o nome dos parametros
		asParamNames = model.GetParNames();

		// inicializa um vetor com o valor dos parametros
		vParamValues = model.GetPar();

		// Completa os valores da matrix Lambda
		mLambda = ProcessoLambda(mLambda, vParamValues, asParamNames, iQtdLags, iCont);
		//savemat(sprint(txMatPath, "R", iCont, "_Lambda.mat"), mLambda);

		mPhi = ProcessoPhi(mPhi, vParamValues, asParamNames, iQtdLags, iCont);
		//savemat(sprint(txMatPath, "R", iCont, "_Phi.mat"), mPhi);

		mU = ProcessoU(mU, vParamValues, asParamNames, iQtdLags, iCont);
		//print(mU);
		//savemat(sprint(txMatPath, "R", iCont, "_U.mat"), mPhi);

		//mLambda = ProcessoPhi(mPhi, vParamValues, asParamNames, iQtdLags, iCont);

		// separo a matriz lambda em matriz lag-0 e demais lags
		mLambda_0 = mLambda[][0:iQtdVarDependente-1];
		mLambda = mLambda[][iQtdVarDependente:];

		// Construção das matrizes A
		for(nContLags =0; nContLags <= iQtdVarDependente; ++nContLags)
		{
			if(nContLags == 0)
			{
				// Gero a matrix A_0
				A_0 = (<1, 0> ** unit(iQtdVarDependente)) +(<0, -1> ** mLambda_0);
				savemat(sprint(txMatPath, "A", iCont, "_", nContLags, ".mat"), A_0);
				//println(sprint(txMatPath, "A", iCont, "_", nContLags, ".mat"));
				//println(A_0);
			}
			else
			{
				A_L = (<1, 0> ** mPhi[][(iQtdVarDependente*(nContLags-1)):(((iQtdVarDependente*(nContLags-1)) + iQtdVarDependente -1))]) +
				   (<0, 1> ** mLambda[][(iQtdVarDependente*(nContLags-1)):(((iQtdVarDependente*(nContLags-1)) + iQtdVarDependente -1))]);
				savemat(sprint(txMatPath, "A", iCont, "_", nContLags, ".mat"), A_L);
				//println(sprint(txMatPath, "A", iCont, "_", nContLags, ".mat"));
				//println(A_L);
			}
		}

		// Apaga variaveis que nao usa mais.
		delete mPhi;
		delete mLambda;
		delete mLambda_0;
		delete mU;
		delete iContParam;
		delete iTotalParam;
		delete asParamNames;
		delete vParamValues;
		delete nContLags;
		delete A_0;
		delete A_L;







		


		
		println("\nApagando o modelo PcGive referente a regiao ", iCont);	
		delete model;
	}
	
	println("*** Fim da estimacao dos modelos *** \n");
}
