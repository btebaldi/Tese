#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

ProcessoH(const asParamNames, const vParamValues, const iQtdLags, const varMacro)
{
	println("Processo H iniciado");
//	print( "%r", asParamNames, "%cf", vParamValues);

	decl mRet, nContRow, nContCol, nContVar, nContTotal, sMacroSufix, sP_Name, index;

	sMacroSufix = "D_";

	//mRet = zeros(rows(varMacro), varMacro(varMacro)*iQtdLags);
	mRet = zeros(rows(varMacro), rows(varMacro)*iQtdLags);
	//println(mRet);


//	decl debug_ColsNames;
	
	for(nContRow=0; nContRow < rows(varMacro); ++nContRow)
	{
		nContTotal = 0;
//		debug_ColsNames = {"Begin"};

		for(nContCol=1; nContCol <= iQtdLags; ++nContCol)
		{
			for(nContVar=0; nContVar < rows(varMacro); ++nContVar)
			{
				if(nContCol == 0)
				{
					sP_Name = sprint(sMacroSufix, varMacro[nContVar], "@", sMacroSufix, varMacro[nContRow]);
				}
				else
				{
					sP_Name = sprint(sMacroSufix, varMacro[nContVar], "_", nContCol, "@", sMacroSufix, varMacro[nContRow]);
				}

				//println(sParamName);
				index = find(asParamNames, sP_Name);
	
				// caso tenha achado o indice atualiza a tabela
				if(index >=0)
				{
					mRet[nContRow][nContTotal]=vParamValues[index];
					//println(sprint(sP_Name, ":", vParamValues[index]));
				}
				else
				{
					//println(sP_Name);
				}

//				debug_ColsNames ~= sprint(sMacroSufix, varMacro[nContVar], "_", nContCol);
				++nContTotal;
			} // Fim da contagem de Variaveis

			
		} // Fim da contagem de lags/colunas
	} // Fim da contagem de linhas

//	debug_ColsNames = debug_ColsNames[1:];
//	print( "%r", varMacro, "%c", debug_ColsNames, "%cf", mRet);

	return mRet;
}


ProcessoB(const asParamNames, const vParamValues, const iQtdLags, const varMacro)
{
	println("Processo B iniciado");

//	print( "%r", asParamNames, "%cf", vParamValues);
	
	decl mRet, nContRow, nContCol, nContVar, nContTotal, sMacroSufix, sP_Name, index;

	sMacroSufix = "D_";

	//mRet = zeros(rows(varMacro), varMacro(varMacro)*iQtdLags);
	mRet = zeros(rows(varMacro), iQtdLags);
	//println(mRet);
	
	for(nContRow=0; nContRow < rows(varMacro); ++nContRow)
	{
		nContTotal = 0;
		for(nContCol=1; nContCol <= iQtdLags; ++nContCol)
		{
			if(nContCol == 0)
			{
				sP_Name = sprint(sMacroSufix, "mX", "@", sMacroSufix, varMacro[nContRow]);
			}
			else
			{
				sP_Name = sprint(sMacroSufix, "mX", "_", nContCol, "@", sMacroSufix, varMacro[nContRow]);
			}
						
			//println(sParamName);
			index = find(asParamNames, sP_Name);

			// caso tenha achado o indice atualiza a tabela
			if(index >=0)
			{
				mRet[nContRow][nContTotal]=vParamValues[index];
				//println(sprint(sP_Name, ":", vParamValues[index]));
			}
			else
			{
				//println(sP_Name);
			}
			
			++nContTotal;
			
		} // Fim da contagem de lags/colunas
	} // Fim da contagem de linhas

//	print( "%r", varMacro, "%cf", mRet);

	return mRet;
}



ProcessoAlphaBeta(const asParamNames, const vParamValues, const varMacro)
{
	println("Processo AlphaBeta iniciado");

//	print( "%r", asParamNames, "%cf", vParamValues);
		
	decl mRet, sMacroSufix, nContRow, sP_Name, index;

	sMacroSufix = "D_";
	mRet = zeros(rows(varMacro), 1);
//	println("mRet:", mRet);
	
	for(nContRow=0; nContRow < rows(varMacro); ++nContRow)
	{

		sP_Name = sprint("betaMacro_1", "@", sMacroSufix, varMacro[nContRow]);
						
		//println(sParamName);
		index = find(asParamNames, sP_Name);

		// caso tenha achado o indice atualiza a tabela
		if(index >=0)
		{
			mRet[nContRow][0]=vParamValues[index];
			//println(sprint(sP_Name, ":", vParamValues[index]));
		}
		else
		{
			//println(sP_Name);
		}
	} // Fim da contagem de linhas

//	print( "%r", varMacro, "%cf", mRet);

	return mRet;
}



//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
ProcessoU(const sName, const iValue, const varMacro)
{
	println("Processo U iniciado");

//	println( "%r", sName, "%cf", iValue);


	decl iContRows, iContCols, iContVar, sParamName, index, asConstants, sVarSufix, mU;

	sVarSufix = "D_";
	asConstants = {"Constant","CSeasonal", "CSeasonal_1", "CSeasonal_2", "CSeasonal_3", "CSeasonal_4", "CSeasonal_5", "CSeasonal_6", "CSeasonal_7", "CSeasonal_8", "CSeasonal_9", "CSeasonal_10"};
	mU = zeros(rows(varMacro), rows(asConstants));

	for(iContRows=0; iContRows < sizeof(varMacro); ++iContRows)
	{
		// Preenchimento da matrix Lambda
		for(iContCols = 0; iContCols<sizeof(asConstants); ++iContCols)
		{
			// determina o nome das variaveis
			sParamName = sprint(asConstants[iContCols], "@", sVarSufix, varMacro[iContRows]);

			index = strfind(sName, sParamName);
	
			if(index >-1)
			{
				mU[iContRows][iContCols] = iValue[index];	
			}
		} // Fim: iContCols (looping nos lags)

	} // Fim: iContRows (looping nos tipos para as linhas)

//	print( "%r", varMacro, "%c", asConstants, "%cf", mU);
	
	println("Processo U finalizado");
	return mU;
}

//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
ProcessoIIS(const sName, const iValue, const varMacro, const anoIni, const anoFim)
{
	//(const iValue, const sName, const iQtdLags, const iRegDependente)
	println("Processo Extracao da matriz de saturacao (IIS)");

//	println( "%r", sName, "%cf", iValue);

	
	decl sVarSufix, mReturn, nContTotal, nCountAno, nCountMes, nContTipo, sParamName, index;

	sVarSufix = "D_";
	
	mReturn = zeros(rows(varMacro), ((anoFim-anoIni)+1)*12);

	// Variavel usada apenas para debug
//	decl debug_IIS_labels;
//	debug_IIS_labels = {"Begin"};	

	//Faz um looping por todas as datas, procura a respectiva dummy e se achar adiciona o valor a tabela.
	nContTotal = 0;
	for(nCountAno = anoIni; nCountAno <= anoFim; ++nCountAno)
	{
		for(nCountMes = 1; nCountMes <= 12; ++nCountMes)
		{
			// usada apenas para debug
//			debug_IIS_labels = debug_IIS_labels | sprint("I:",nCountAno,"(",nCountMes,")");

			for(nContTipo = 0; nContTipo <sizeof(varMacro); ++nContTipo)
			{
			    sParamName = sprint("I:",nCountAno,"(",nCountMes,")@", sVarSufix, varMacro[nContTipo]);

				//println(sParamName);
				
				index = find(sName, sParamName);

				// caso tenha achado o indice atualiza a tabela
				if(index >=0)
				{
					mReturn[nContTipo][nContTotal]=iValue[index];
				}
			}

			++nContTotal;
		}
	}

	// usada apenas para debug
//	debug_IIS_labels = debug_IIS_labels[1:];
//	println( "%r", varMacro, "%c", debug_IIS_labels, "%cf", mReturn);

	return mReturn;
}


main()
{

	#include "./Config.ini"

	//decl iQtdVarDependente, iQtdLags, iQtdRegioes, txDbase, txCoIntMatPath, txMatPathRawMatrix, txDbaseMacroVariables, txMatPathW_Matrix;
		
	decl mX, mX_tilda, mW_w, nCont, sMacroVarNames, beta, database, mMacroData;

	
	println(">>> Inicio da estimacao do modelo de macrovariaveis");
	println("\n\n*****************************************");	
	println("             Macrovariaveis ");	
	println("*****************************************\n\n");	

	println("Carregando dados das variaveis exogenas");

	println("Carregando base de dados com informacoes regionais");
	database = new Database();
	database.Load(txDbase);
	
	println("Construindo variaveis X_Adm e X_Des");
	// CONTRUCAO DAS VARIAVEIS DELTA
	for(nCont=1; nCont <= iQtdRegioes; ++nCont)
	{
		if(nCont==1)
		{
			mX_tilda = database.GetVar(sprint("R", nCont, "_Admitidos"));
			mX_tilda = mX_tilda ~ database.GetVar(sprint("R", nCont, "_Desligados"));
		}
		else
		{
			mX_tilda = mX_tilda ~ database.GetVar(sprint("R", nCont, "_Admitidos"));
			mX_tilda = mX_tilda ~ database.GetVar(sprint("R", nCont, "_Desligados"));
		}

		//println("mX_Des: ", mX_Des);
	}
	
	println("Concluido construcao das variaveis X_Adm e X_Des");

	println("Construindo acumulado Net (X_Adm - X_Des)");
	// mW_w = ones(1, columns(mX_Adm)) ~ -1*ones(1, columns(mX_Des));
	mW_w = ones(1, columns(mX_tilda)/2) ** <1, -1>;
	mX = mX_tilda * mW_w';


	// Salvo a matriz W_w para utilizar depois, aqui contriuda de maneira diferente por questoes de conformidade.
	println("Salvando matriz W_w para ser utilizada depois");
	savemat(sprint(txMatPathW_Matrix, "W_w.mat"), mW_w);
	
	delete database, mX_tilda;
	
	// Inicio um nomo objeto do tipo PcGive
	println("Inicio um nomo objeto do tipo PcGive referente a macrovariaveis");	
	decl model = new PcGive();

	println("Carregando base de dados de macrovariaveis");
	model.Load(txDbaseMacroVariables);
	
	println("Iniciando construcao da variavel Delta para variaveis");
		
	for(nCont=0; nCont < rows(aMacroVarNames); ++nCont)
	{
		println("Adicionado Diff da variavel: ", aMacroVarNames[nCont]);
		model.Append(diff(model.GetVar(aMacroVarNames[nCont])), {sprint("D_", aMacroVarNames[nCont])});
		//println("%c", sprint("D_", aMacroVarNames[nCont]), diff(model.GetVar(aMacroVarNames[nCont])));
   	}
	
	
	println("Adicionando variavel mX");	
	model.Append(mX, {"mX"});

	println("Adicionando variavel D_mX ");	
	model.Append(diff(mX), {"D_mX"});

	if(rows(aMacroVarNames) > 1){
		// ****************
		// Processo comentado pois foi verificado que nao existe cointegracao entre as variaveis externas e as macrovariaveis.
	
		//// Leitura do vetor de cointegracao
		//beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_MacroVar.mat")));
	   	//mMacroData = model.GetVar(aMacroVarNames);
		////println(">>>",mMacroData*beta'); 
		//model.Append(mMacroData*beta', {"betaMacro"});
	}

	model.SetModelClass("SYSTEM");
	  	
	// Deseleciona as variaveis
	model.DeSelect();

	model.Deterministic(3);
	
	// adiciona variavel dependente
	for(nCont=0; nCont < rows(aMacroVarNames); ++nCont)
	{
	model.Select("Y", {sprint("D_", aMacroVarNames[nCont]), 0, 0});
	}
	
	// adiciona a variavel dependente (lag da independente)
	for(nCont=0; nCont < rows(aMacroVarNames); ++nCont)
	{
	model.Select("X", {sprint("D_", aMacroVarNames[nCont]), 1, iQtdLags});
	}

	// Adiciona a variavel "star"
	model.Select("X", {"D_mX", 1, iQtdLags});

// ****************
// Processo comentado pois foi verificado que nao existe cointegracao entre as variaveis externas e as macrovariaveis.
	if(rows(aMacroVarNames) < -1){
	// Adiciona a variavel "betaMacro" com um lag apenas (representação da matriz de longo prazo)
	model.Select("X", {"betaMacro", 1, 1});
	}

	// Adiciona variaveis constante e sesonals
	model.Select("U", {"Constant", 0, 0});
//	model.Select("U", {"CSeasonal", 0, 10});
		
	// determina a janela de tempo do modelo
	//model.SetSample(2004,1 ,2016, 12);
	model.SetSelSampleByIndex(model.GetSelStart(), model.GetSelEnd());
//	println("GetSelStart", model.GetSelStart());
//	println("GetSelEnd", model.GetSelEnd());

	println("Model Sample: ", model.GetSelSample());

	// Liga o autometrics
	//model.Autometrics(0.001, "none", 2);
	model.Autometrics(0.0001, "IIS", 1);
	model.AutometricsSet("print", 0);
		
	// determina o metodo de estimacao.
	model.SetMethod(M_OLS);

	// Realiza a estimacao do modelo
	model.Estimate();

	decl iTotalParam, asParamNames, vParamValues;
	
	// inicializa o total de parametros
	iTotalParam = model.GetParCount();

	// inicializa um vetor com o nome dos parametros
	asParamNames = model.GetParNames();

	// inicializa um vetor com o valor dos parametros
	vParamValues = model.GetPar();

	println( "%r", asParamNames, "%cf", vParamValues);

	// A FAZER:
	//(1) Leitura dos valores de H
	decl mH;
	mH = ProcessoH(asParamNames, vParamValues, iQtdLags, aMacroVarNames);
	savemat(sprint(txMatPathRawMatrix, "H_w.mat"), mH);
	delete mH;
	
	//(2) Leitura dos valores de U + Cesonal
	decl mU;
	mU = ProcessoU(asParamNames, vParamValues, aMacroVarNames);
	savemat(sprint(txMatPathRawMatrix, "U_w.mat"), mU);
	delete mU;
	
	//(3) Leitura dos valores de IIS

	decl mIIS;
	mIIS = ProcessoIIS(asParamNames, vParamValues, aMacroVarNames, model.GetYear1(), model.GetYear2());
	savemat(sprint(txMatPathRawMatrix, "IIS_w.mat"), mIIS);
	delete mIIS;
	
	
	//(4) Leitura dos valores do vetor de cointegracao
	decl mAlphaBeta_w, mAlpha_w;
	mAlpha_w = ProcessoAlphaBeta(asParamNames, vParamValues, aMacroVarNames);


// ****************
// Processo comentado pois foi verificado que nao existe cointegracao entre as variaveis externas e as macrovariaveis.
	if(rows(aMacroVarNames) < -1){
	mAlphaBeta_w = mAlpha_w * beta;
	}else{
	mAlphaBeta_w= reshape(0, rows(aMacroVarNames), rows(aMacroVarNames));
	}
	
	savemat(sprint(txMatPathRawMatrix, "AlphaBeta_w.mat"), mAlphaBeta_w);
	savemat(sprint(txMatPathRawMatrix, "Alpha_w.mat"), mAlpha_w);
	delete mAlphaBeta_w, mAlpha_w;
	
	//(5) Leitura dos valores B
	decl mB;
	mB = ProcessoB(asParamNames, vParamValues, iQtdLags, aMacroVarNames);
	savemat(sprint(txMatPathRawMatrix, "B_w.mat"), mB);
	delete mB;
		
	// const iValue, const sName, const iRegDependente, const sVarSufix, const aMacroVarNames
	//mD_macro = ProcessoMacroVariables(vParamValues, asParamNames, iQtdLags, iCont, sprint(sVarSufix, "R"), sVarSufix, aMacroVarNames);
	//savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_D.mat"), mD_macro);
		

	delete iTotalParam, asParamNames, vParamValues;
	
	delete model;

	println("*** Fim da estimacao dos modelos de macrovariaveis *** \n");
}


