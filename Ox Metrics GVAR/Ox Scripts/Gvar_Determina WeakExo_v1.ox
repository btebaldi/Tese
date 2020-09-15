#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>
#import <packages/CATS/CATS>

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
	//println("Estimated rank: ", iRank);
    return(iRank);
}


GetBetaEstimative(const mBeta, const iRank){
	// println(mBeta);
    decl ret;
	if (iRank == 0){
		ret = zeros(4, 1);
	} else {
		ret = mBeta[][0:iRank-1];
	}
	return ret;
}

SaveBetaEstimative(const spath, const mBeta, const iRank){
	decl mbetaTransp = GetBetaEstimative(mBeta, iRank);
	savemat(spath, mbetaTransp');
}


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

    println("Carregando matrix de pessos W");
    decl mW;
    mW = loadmat(sprint(txMatPathW_Matrix, "data.mat"));
    //println("mW", mW);

    println("*** Iniciando estimacao dos modelos *** \n");
    // iCont : Contador da regiao atual
    decl iCont;

    for (iCont = 1; iCont <= iQtdRegioes; ++iCont) {

        // FOR DEBUG ONLY
        // if(iCont >20){
        //     exit(0);
        // }


        // print Headder
        println("\n\n*****************************************");
        println("             Regiao ", iCont);
        println("*****************************************\n\n");
        
        // Inicio um nomo objeto do tipo database
        decl modelDatabase = new Database();
        decl mRankMatrix; // Matriz de selecao dos vetores de cointegracao
        decl iRank; // Rank selecionado
        // decl iRank;

        println("\nCarregando base de dados para regiao ", iCont);
        modelDatabase.Load(txDbase);
        
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
        modelDatabase.Append(mData * mW[][iCont - 1], {"star_Desligados"});
        
        // Star dos Admitidos
        mData = modelDatabase.GetVar(GetRegionNames(iQtdRegioes, "R", "_Admitidos"));
        modelDatabase.Append(mData * mW[][iCont - 1], {"star_Admitidos"});
        println("\tConcluido construcao das variaveis star para a regiao ", iCont);

        println("(2) Iniciando construcao da variavel Delta para a regiao ", iCont);
        // CONTRUCAO DAS VARIAVEIS DELTA
        mData =	modelDatabase.GetVar(sprint("R", iCont, "_Admitidos"));
        mData = mData ~ modelDatabase.GetVar(sprint("R", iCont, "_Desligados"));
        mData = mData ~ modelDatabase.GetVar("star_Admitidos");
        mData = mData ~ modelDatabase.GetVar("star_Desligados");
        modelDatabase.Append(diff(mData), {sprint("D_R", iCont, "_Admitidos"), sprint("D_R", iCont, "_Desligados"), "D_star_Admitidos", "D_star_Desligados"});
        println("\tConcluido construcao da variavel Delta para a regiao ", iCont);
        
        // CONTRUCAO DA MATRIZ DE LONGO PRAZO
        println("(3) Iniciando construcao da variavel beta*Z (Cointegracao) para a regiao ", iCont);

        // IF BETA ESTIMATION
        println("\tIniciando determinacao do vetor de cointegracao (beta) a regiao ", iCont);
        
        // Inicio um objeto do CATS (Cointegration)
    	decl modelCats = new CATS();

        decl asX = {sprint("R", iCont, "_Admitidos"), sprint("R", iCont, "_Desligados"), "star_Admitidos", "star_Desligados"};
        mData = modelDatabase.GetVar(asX);

        // Adiciona variaveis X = [adm, des] e X*=[adm*, des*] ao banco de dados
        modelCats.Append(mData, asX);

    	// Adiciona as variaveis X como exogenas
	    for(decl iqtd = 0; iqtd < columns(asX); iqtd++) {
            println("append: ", asX[iqtd]);
			modelCats.Select("Y", {asX[iqtd], 0, 0});
	    }

        decl iLags;
        iLags = 2;

        // Lags ( const lags , const lagsExo = - 1 , const lagsDet = - 1 )
	    modelCats.Lags(iLags, iLags, iLags);

	    // Rank inicial (mudar para a quantidade de variaveis.)
	    modelCats.I1Rank(4);

        // Tipo de cointegracao CIMEAN: Constante no espaço de cointegracao.
        // mode	string: one of "NONE","CIMEAN","DRIFT","CIDRIFT".
        // Equivalently, use the strings "H_z","H_c","H_lc","H_l", or the predefined constants CATS::NONE, CATS::CIMEAN, CATS::DRIFT, CATS::CIDRIFT.
        modelCats.Trend("DRIFT");

        // Inclui seasonal centradas
        modelCats.Seasonals(1);

        // fixa a amostra
        // model.SetSelSample(1975, 9, 1998, 12);
        
        // tipo de metodo RRR: Reduced Rank Regression
        modelCats.SetMethod("RRR");

        // set print to false
        modelCats.SetPrint(FALSE);

        // Estima o modelo.
        modelCats.Estimate();

        // Estima vetores do cointegração por bootstrap
        mRankMatrix = modelCats.BootstrapRankTest();

        // Escolhe o Rank 
        iRank = EstimateRank(mRankMatrix);

        mBeta = modelCats.GetBeta();
       
        // println(mRankMatrix);

        // Se o rank for maior que dois Automaticamente teremos de modelar as variaveis no modelo dominante
        if(iRank > 2){
            // salva a estimacao do beta PARA AS REGIOES COM MAIS DE 3VETORES DE COINTEGRACAO
            SaveBetaEstimative(sprint(txCoIntMatPath, sprint("Dominant3_CoInt_R", iCont, ".mat")), mBeta, iRank);
        } else {
            // Restima o modelo com os dados de cointegracao.
            modelCats.I1Rank(iRank);
            modelCats.Estimate();
            modelCats.BootstrapRankTest();

            modelCats.SetPrint(TRUE);
            // Estima a exogeniedade fraca
            decl a = modelCats.TestAlphaCommon("* * 0 0");

            // println("a", a[0]);
            // println("a", a[1]);
            // println("a", a[2]);
            // println("a", a[3]);
            // println("a", modelCats.GetAlpha());

            SaveBetaEstimative(sprint(txCoIntMatPath, sprint("Weak2_CoInt_R", iCont, ".mat")), mBeta, iRank);
        }

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
