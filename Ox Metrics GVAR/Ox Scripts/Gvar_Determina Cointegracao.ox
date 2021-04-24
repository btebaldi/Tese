#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

#include <.\ClasseCATS_Custom.ox>

// GET WD()  OLHAR PARA PEGAR O DIRETORIO


GetRegionNames(const iQtdRegioes, const sVarPrefix, const sVarPosfix) {
    // println("iQtdRegioes: ", iQtdRegioes);
    // println("sVarPrefix: ", sVarPrefix);
    // println("sVarPosfix: ", sVarPosfix);
    decl nCont, aNames;

    for (nCont = 1; nCont <= iQtdRegioes; ++nCont) {
        if (nCont == 1) {
            aNames = {sprint(sVarPrefix, nCont, sVarPosfix)};
        } else {
            aNames = aNames ~ {sprint(sVarPrefix, nCont, sVarPosfix)};
        }
    }
    return aNames;
}

EstimateRank(const mRankMatrix){
	decl iRank;
    for(decl irow = 0; irow < rows(mRankMatrix); irow++) {
		iRank = mRankMatrix[0][irow][1];
		if(mRankMatrix[0][irow][6] > 0.05){
			break;
		} else {
			iRank = iRank + 1;
		}
	}

	print("%c", {"p-r", "r", "Eig.Value", "Trace", "Trace*", "Crit5%", "p-value", "p-value*"},
//          "%cf",{"%8.4g", " [%4.2f]"}
	"%10.2f",mRankMatrix[0]);


	println("Estimated rank: ", iRank);
    return(iRank);
}


//GetBetaEstimative(const mBeta, const iRank){
//	// println(mBeta);
//    decl ret;
//	if (iRank == 0){
//		ret = zeros(4, 1);
//	} else {
//		ret = mBeta[][0:iRank-1];
//	}
//	return ret;
//}
//
//SaveBetaEstimative(const spath, const mBeta, const iRank){
//	decl mbetaTransp = GetBetaEstimative(mBeta, iRank);
//	savemat(spath, mbetaTransp');
//}


main() {
    // Arquivo de configuracao
    #include "./Config.ini"

    /***************************************************
     *
     * Declaração de variaveis de configuracao do script
     *
     *************************************************** */
    println("Carregando dados de macrovariaveis");
    decl mMacroData;
    decl daBaseMacro = new Database();

    daBaseMacro.Load(txDbaseMacroVariables);
    
    //print( "%c", daBaseMacro.GetAllNames(), "%cf", daBaseMacro.GetAll());
    println(" Carregando dados das colunas: ", aMacroVarNames);
    
    mMacroData = daBaseMacro.GetVar(aMacroVarNames);
    
    //print( "%c", aMacroVarNames, "%cf", mMacroData[0:9][]);
    delete daBaseMacro;
    println("Macrovariaveis carregadas");


    decl mW;

    println("*** Iniciando estimacao dos modelos *** \n");
    // iCont : Contador da regiao atual
    decl iCont;

    for (iCont = 1; iCont <= iQtdRegioes; ++iCont) {

        // FOR DEBUG ONLY

		if( any(<289, 320> .== iCont)){
	    	mW = loadmat(sprint(txMatPathW_Matrix, "data_EqualWeight.mat"));
		    println("Carregando matrix de pessos W (Equal Weight)");
		} else if ( any(<28, 32, 36, 81, 171, 178, 187, 203, 225, 263, 288, 298, 368, 376, 382, 429, 501, 526, 537> .== iCont)) {
		    mW = loadmat(sprint(txMatPathW_Matrix, "data_Pib.mat"));
		    println("Carregando matrix de pessos W (PIB)");
		} else if ( any(<17, 20, 35, 44, 46, 59, 74, 77, 87, 95, 102, 114, 115, 117, 120, 144, 158, 161, 166, 167, 183,185, 201, 219, 221, 227, 232, 237, 247, 305, 306, 372, 463, 466, 489, 545> .== iCont)) {
		    mW = loadmat(sprint(txMatPathW_Matrix, "data_PibPerCapta.mat"));
		    println("Carregando matrix de pessos W (PIB per capta)");
		} else if ( any(<51> .== iCont)) {
		    mW = loadmat(sprint(txMatPathW_Matrix, "data_Populacao.mat"));
			println("Carregando matrix de pessos W (Populacao)");
		} else {
	     	mW = loadmat(sprint(txMatPathW_Matrix, "data_Connections.mat"));
		    println("Carregando matrix de pessos W (Connections)");
		}

		
		// print Headder
        println("\n\n*****************************************");
        println("             Regiao ", iCont);
        println("*****************************************\n\n");
        
        // Inicio um nomo objeto do tipo database
        decl modelDatabase = new Database();
        decl mRankMatrix; // Matriz de selecao dos vetores de cointegracao
        decl iRank; // Rank selecionado


        println("\nCarregando base de dados para regiao ", iCont);
        modelDatabase.Load(txDbase);
//modelDatabase.SaveIn7(sprint("DEBUG_", iCont, "_Fulldatabase"));        
        println("\tPeriodo da base de dados");
        println("\tData inicial: ", modelDatabase.GetYear1(), "-", modelDatabase.GetPeriod1());
        println("\tData final: ", modelDatabase.GetYear2(), "-", modelDatabase.GetPeriod2());

        // As Variaveis Star sao uma combinacao linear das variaveis esternas.
        println("(1) Iniciando construcao das variaveis star para a regiao ", iCont);

        // mData: Matrix com as variaveis
        // beta: vetores de cointegracao.
        decl mData, mBeta;
        mBeta =0;

        println("\tAdicionando variavel star da regiao ", iCont);
        // Star dos Desligados
        mData = modelDatabase.GetVar(GetRegionNames(iQtdRegioes, "R", "_Desligados"));

//println(mData);
//println(mW);
		
		modelDatabase.Append(mData * mW[][iCont - 1], {"star_Desligados"});
        
        // Star dos Admitidos
        mData = modelDatabase.GetVar(GetRegionNames(iQtdRegioes, "R", "_Admitidos"));

		//modelDatabase.Renew(mData, GetRegionNames(iQtdRegioes, "R", "_Admitidos"));
		
        modelDatabase.Append(mData * mW[][iCont - 1], {"star_Admitidos"});
        println("\tConcluido construcao das variaveis star para a regiao ", iCont);

        println("(2) Iniciando construcao da variavel Delta para a regiao ", iCont);
        // CONTRUCAO DAS VARIAVEIS DELTA
        mData =	modelDatabase.GetVar(sprint("R", iCont, "_Admitidos"));
        mData = mData ~ modelDatabase.GetVar(sprint("R", iCont, "_Desligados"));
        mData = mData ~ modelDatabase.GetVar("star_Admitidos");
        mData = mData ~ modelDatabase.GetVar("star_Desligados");
        modelDatabase.Append(diff(mData), {sprint("D_R", iCont, "_Admitidos"), sprint("D_R", iCont, "_Desligados"), "D_star_Admitidos", "D_star_Desligados"});

//println(mData);
        println("\tConcluido construcao da variavel Delta para a regiao ", iCont);
        
        // CONTRUCAO DA MATRIZ DE LONGO PRAZO
        println("(3) Iniciando construcao da variavel beta*Z (Cointegracao) para a regiao ", iCont);

        // IF BETA ESTIMATION
        println("\tIniciando determinacao do vetor de cointegracao (beta) a regiao ", iCont);
        
        // Inicio um objeto do CATS (Cointegration)
    	decl modelCats = new GVAR_CATS();
	
        decl asX = {sprint("R", iCont, "_Admitidos"), sprint("R", iCont, "_Desligados"), "star_Admitidos", "star_Desligados"};
        mData = modelDatabase.GetVar(asX);

        // Adiciona variaveis X = [adm, des] e X*=[adm*, des*] ao banco de dados
        modelCats.Append(mData, asX);

		modelCats.Resample(12, 1995, 1);

//println("DEBUG(1)");
//modelCats.SaveIn7(sprint("R", iCont, "_database"));

    	// Adiciona as variaveis X como exogenas
	    for(decl iqtd = 0; iqtd < columns(asX); iqtd++) {
            println("append: ", asX[iqtd]);
			modelCats.Select("Y", {asX[iqtd], 0, 0});
	    }
//println("DEBUG(2)");
        decl iLags;
        iLags = 13;

        // Lags ( const lags , const lagsExo = - 1 , const lagsDet = - 1 )
	    modelCats.Lags(iLags, iLags, iLags);


		if( any(<13, 17, 20, 21, 26, 30, 31, 32, 34, 35, 36, 37, 41, 43, 44, 46, 49, 50, 57, 58, 59, 60, 62, 63, 64, 66, 67, 68, 69, 71, 73, 74, 75, 77, 78, 81, 83, 86, 87, 91, 101, 103, 108, 109, 113, 114, 115, 117, 118, 119, 120, 121, 122, 123, 126, 127, 128, 130, 131, 133, 134, 135, 137, 140, 142, 143, 144, 150, 151, 153, 155, 159, 160, 161, 162, 163, 165, 168, 172, 176, 179, 183, 185, 186, 187, 190, 192, 202, 217, 219, 221, 222, 223, 224, 227, 233, 234, 235, 236, 237, 242, 263, 277, 282, 287, 288, 323, 324, 326, 327, 334, 335, 339, 343, 348, 358, 369, 390, 394, 417, 420, 421, 423, 426, 429, 430, 432, 445, 447, 467, 468, 485, 487, 501, 502, 505, 531, 539, 544, 550> .== iCont)){
		iRank = 1;
		} else {
		iRank = 2;
		}

	    // Rank inicial (mudar para a quantidade de variaveis.)
	    modelCats.I1Rank(iRank);

        // Tipo de cointegracao CIMEAN: Constante no espaço de cointegracao.
        // mode	string: one of "NONE","CIMEAN","DRIFT","CIDRIFT".
        // Equivalently, use the strings "H_z","H_c","H_lc","H_l", or the predefined constants CATS::NONE, CATS::CIMEAN, CATS::DRIFT, CATS::CIDRIFT.
        modelCats.Trend("DRIFT");

        // Inclui seasonal centradas
        modelCats.Seasonals(1);

        // fixa a amostra
        //model.SetSelSample(1995, 1, 1998, 12);
        
        // tipo de metodo RRR: Reduced Rank Regression
        modelCats.SetMethod("RRR");

        // set print to false
        modelCats.SetPrint(FALSE);

        // Estima o modelo.
        modelCats.Estimate();

        modelCats.Estimate();
		modelCats.SetPrint(TRUE);

		mBeta = modelCats.GetBeta();

		modelCats.SaveBetaEstimative(sprint(txCoIntMatPath, sprint("Weak2_CoInt_R", iCont, ".mat")), mBeta, iRank);

        // Guarda o valor do Beta
        // mBeta = model.GetBeta();

        delete modelCats;
        delete modelDatabase;

        // Apago variaveis que nao serao mais utilizadas
        delete mData, mBeta, asX;
    } // for (iCont = 1; iCont <= iQtdRegioes; ++iCont)

    delete mW;
    println("*** Fim da estimacao dos modelos regionais *** \n");
} // End of main()
