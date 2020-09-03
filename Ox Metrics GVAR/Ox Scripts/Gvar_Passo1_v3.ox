#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

//	mPhi: matrix dos coeficientes. Inicialmente uma matriz zerada [iQtdVarDependente x iQtdVarDependente*iQtdLags];
//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iQtdLags: Quantidade de lags
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
ProcessoPhi(mPhi, const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix) {
    println("Processo Phi iniciado");
    decl iContRows, iContCols, iContVar, sParamName, asTipo, index;
    asTipo = {"Admitidos", "Desligados"};

    for (iContRows = 0; iContRows < sizeof(asTipo); ++iContRows) {
        // Preenchimento da matrix Lambda
        for (iContCols = 0; iContCols < iQtdLags; ++iContCols) {
            for (iContVar = 0; iContVar < sizeof(asTipo); ++iContVar) {
                // determina o nome das variaveis
                sParamName = sprint(sVarSufix, iRegDependente, "_", asTipo[iContVar], "_", iContCols + 1, "@", sVarSufix, iRegDependente, "_", asTipo[iContRows]);
                index = strfind(sName, sParamName);

                if (index > -1) {
                    mPhi[iContRows][(sizeof(asTipo)*iContCols) + iContVar] = iValue[index];
                }
            } // Fim: iContVar (looping nos tipos para as colunas (colunas pares e impares))
        } // Fim: iContCols (looping nos lags)
    } // Fim: iContRows (looping nos tipos para as linhas)
    println("Processo Phi finalizado");
//	println(mPhi);
    return mPhi;
}



//	mLambda: matrix dos coeficientes. Inicialmente uma matriz zerada [iQtdVarDependente x iQtdVarDependente*(iQtdLags+1)]
//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iQtdLags: Quantidade de lags
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
ProcessoLambda(mLambda, const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix) {
    println("Processo Lambda iniciado");
    decl iContRows, iContCols, iContVar, sParamName, asTipo, index;
    asTipo = {"Admitidos", "Desligados"};

    for (iContRows = 0; iContRows < sizeof(asTipo); ++iContRows) {
        // Preenchimento da matrix Lambda
        for (iContCols = 0; iContCols <= iQtdLags; ++iContCols) {
            for (iContVar = 0; iContVar < sizeof(asTipo); ++iContVar) {
                // determina o nome das variaveis
                if (iContCols == 0) {
                    // Determina o nome do parametro sem lag
                    sParamName = sprint(sVarSufix, "star_", asTipo[iContVar], "@", sVarSufix, "R", iRegDependente, "_", asTipo[iContRows]);
                } else {
                    // Determina o nome do parametro COM lag
                    sParamName = sprint(sVarSufix, "star_", asTipo[iContVar], "_", iContCols, "@", sVarSufix, "R", iRegDependente, "_", asTipo[iContRows]);
                }

                index = strfind(sName, sParamName);

                if (index > -1) {
                    mLambda[iContRows][(sizeof(asTipo)*iContCols) + iContVar] = iValue[index];
                }
            } // Fim: iContVar (looping nos tipos para as colunas (colunas pares e impares))
        } // Fim: iContCols (looping nos lags)
    } // Fim: iContRows (looping nos tipos para as linhas)
    println("Processo Lambda finalizado");
//	println(mLambda);
    return mLambda;
}


//	mU: matriz de coeficientes para ser preenchida (inicialmente deve ser uma matriz de zeros)
//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iQtdLags: Quantidade de lags
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
ProcessoU(mU, const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix) {
    println("Processo U iniciado");
    decl iContRows, iContCols, iContVar, sParamName, asTipo, index, asConstants;
    asTipo = {"Admitidos", "Desligados"};
    asConstants = {"Constant", "CSeasonal", "CSeasonal_1", "CSeasonal_2", "CSeasonal_3", "CSeasonal_4", "CSeasonal_5", "CSeasonal_6", "CSeasonal_7", "CSeasonal_8", "CSeasonal_9", "CSeasonal_10"};

    for (iContRows = 0; iContRows < sizeof(asTipo); ++iContRows) {
        // Preenchimento da matrix Lambda
        for (iContCols = 0; iContCols < sizeof(asConstants); ++iContCols) {
            // determina o nome das variaveis
            sParamName = sprint(asConstants[iContCols], "@", sVarSufix, iRegDependente, "_", asTipo[iContRows]);
            index = strfind(sName, sParamName);

            if (index > -1) {
                mU[iContRows][iContCols] = iValue[index];
            }
        } // Fim: iContCols (looping nos lags)
    } // Fim: iContRows (looping nos tipos para as linhas)
    println("Processo U finalizado");
    return mU;
}

//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
ProcessoIIS(const iValue, const sName, const iRegDependente, const sVarSufix, const anoIni, const anoFim) {
    //(const iValue, const sName, const iQtdLags, const iRegDependente)
    println("Processo Extracao da matriz de saturacao (IIS)");
    decl nContTipo, nContTotal, iContVar, sParamName, asTipo, nCountAno, nCountMes, mReturn, index;
    mReturn = zeros(2, ((anoFim - anoIni) + 1) * 12);
    asTipo = {"Admitidos", "Desligados"};
    //Faz um looping por todas as datas, procura a respectiva dummy e se achar adiciona o valor a tabela.
    nContTotal = 0;

    for (nCountAno = anoIni; nCountAno <= anoFim; ++nCountAno) {
        for (nCountMes = 1; nCountMes <= 12; ++nCountMes) {
            //println(sprint("I:",nCountAno,"(",nCountMes,")"));
            for (nContTipo = 0; nContTipo < sizeof(asTipo); ++nContTipo) {
                index = find(sName, sprint("I:", nCountAno, "(", nCountMes, ")@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]));

                // caso tenha achado o indice atualiza a tabela
                if (index >= 0) {
                    mReturn[nContTipo][nContTotal] = iValue[index];
                }
            }
            ++nContTotal;
        }
    }
//	println(mReturn);
    return mReturn;
}

//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iRegDependente: Regiao dependente na regressao
//	sVarSufix: Sufixo do nome da variavael "D_R"
ProcessoPILongRun(const iValue, const sName, const iRegDependente, const sVarSufix) {
    //(const iValue, const sName, const iQtdLags, const iRegDependente)
    println("Processo Extracao da matriz de longo prazo");
    decl nContTipo, nContTotal, iContVar, sParamName, asTipo, nCountAno, nCountMes, mReturn, index;
    mReturn = zeros(2, 2);
    asTipo = {"Admitidos", "Desligados"};

    for (nContTipo = 0; nContTipo < sizeof(asTipo); ++nContTipo) {
        index = find(sName, sprint("betaZ_", asTipo[0], "_", 1, "@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]));

        // caso tenha achado o indice atualiza a tabela
        if (index >= 0) {
            mReturn[nContTipo][0] = iValue[index];
        }

        index = find(sName, sprint("betaZ_", asTipo[1], "_", 1, "@", sVarSufix, iRegDependente, "_" , asTipo[nContTipo]));

        // caso tenha achado o indice atualiza a tabela
        if (index >= 0) {
            mReturn[nContTipo][1] = iValue[index];
        }
    }
//	println(mReturn);
    return mReturn;
}

//	iValue: vParamValues - Valor efetivo dos parametros
//	sName: asParamNames - Nome dos parametros
//	iQtdLags: Quantidade de lags
//	iRegDependente: iCont - identificador da variavel dependente
//	sVarSufix: Sufixo do nome da variavael "D_R"
//	sMacroSufix: Sufixo do nome da variavael macro {"D"}
//	aMacoVarNames: Nome d variavel macro
ProcessoMacroVariables(const iValue, const sName, const iQtdLags, const iRegDependente, const sVarSufix, const sMacroSufix, const aMacoVarNames) {
    //vParamValues, asParamNames, iQtdLags, iCont, sVarSufix, aMacoVarNames
    println("Processo Extracao das variaveis macroeconomicas (matrix de longo Prazo inclusive)");
    decl nContTipo, nContLag, nContVar, nContTotal, asTipo, mReturn, sParamName, index;//nContTotal, iContVar, sParamName, , nCountAno, nCountMes, , ;
    //println("sName:", sName);
    mReturn = zeros(2, (rows(aMacoVarNames) * (iQtdLags + 1)));
    asTipo = {"Admitidos", "Desligados"};

    //println("%r", asTipo, "%c", {"D_Selic", "D_IPCA", "D_PIM", "D_Selic_1", "D_IPCA_1", "D_PIM_1", "D_Selic_2", "D_IPCA_2", "D_PIM_2", "betaMacro"},  mReturn);

    for (nContTipo = 0; nContTipo < sizeof(asTipo); ++nContTipo) {
        nContTotal = 0;

        for (nContLag = 0; nContLag <= iQtdLags; ++nContLag) {
            for (nContVar = 0; nContVar < rows(aMacoVarNames); ++nContVar) {
                if (nContLag == 0) {
                    sParamName = sprint(sMacroSufix, aMacoVarNames[nContVar], "@", sVarSufix, iRegDependente, "_", asTipo[nContTipo]);
                    //	D_R5_Desligado
                } else {
                    sParamName = sprint(sMacroSufix, aMacoVarNames[nContVar], "_", nContLag, "@", sVarSufix, iRegDependente, "_", asTipo[nContTipo]);
                }

                //println(sParamName);
                index = find(sName, sParamName);

                // caso tenha achado o indice atualiza a tabela
                if (index >= 0) {
                    mReturn[nContTipo][nContTotal] = iValue[index];
                }

                ++nContTotal;
            } // fim nContVar
        } // fim nContLag
    } // fim nContTipo
//	println("%r", asTipo, "%c", {"D_Selic", "D_IPCA", "D_PIM", "D_Selic_1", "D_IPCA_1", "D_PIM_1", "D_Selic_2", "D_IPCA_2", "D_PIM_2", "betaMacro"},  mReturn);
    //println(mReturn);
    return mReturn;
}



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


main() {
    // Arquivo de configuracao
#include "./Config.ini"

    /***************************************************
     *
     * Declaração de variaveis de configuracao do script
     *
     *************************************************** */
    // Variáveis do programa
    decl i, sVarSufix;
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
    // iCont2: Contador
    decl iCont;

    for (iCont = 1; iCont <= iQtdRegioes; ++iCont) {
        // print Headder
        println("\n\n*****************************************");
        println("             Regiao ", iCont);
        println("*****************************************\n\n");
        // Inicio um nomo objeto do tipo PcGive
        println("Iniciando um nomo objeto do tipo PcGive referente a regiao ", iCont);
        decl model = new PcGive();
        println("\nCarregando base de dados para regiao ", iCont);
        model.Load(txDbase);
        println("\tPeriodo da base de dados");
        println("\tData inicial: ", model.GetYear1(), "-", model.GetPeriod1());
        println("\tData final: ", model.GetYear2(), "-", model.GetPeriod2());
        // As Variaveis Star sao uma combinacao linear das variaveis esternas.
        println("(1) Iniciando construcao das variaveis star para a regiao ", iCont);
        decl mData, beta;
        //mData = model.GetAll();
        println("\tAdicionando variavel star da regiao ", iCont);
        // println(GetRegionNames(iQtdRegioes, "R", "_Desligados"));
        mData = model.GetVar(GetRegionNames(iQtdRegioes, "R", "_Desligados"));
        //println("%c", GetRegionNames(iQtdRegioes, "R", "_Desligados"), mData[0:6][]);
        model.Append(mData * mW[][iCont - 1], {"star_Desligados"});
        mData = model.GetVar(GetRegionNames(iQtdRegioes, "R", "_Admitidos"));
        //println("%c", GetRegionNames(iQtdRegioes, "R", "_Admitidos"), mData[0:6][]);
        model.Append(mData * mW[][iCont - 1], {"star_Admitidos"});
        println("\tConcluido construcao das variaveis star para a regiao ", iCont);
        println("(2) Iniciando construcao da variavel Delta para a regiao ", iCont);
        // CONTRUCAO DAS VARIAVEIS DELTA
        mData =	model.GetVar(sprint("R", iCont, "_Admitidos"));
        mData = mData ~ model.GetVar(sprint("R", iCont, "_Desligados"));
        mData = mData ~ model.GetVar("star_Admitidos");
        mData = mData ~ model.GetVar("star_Desligados");
        model.Append(diff(mData), {sprint("D_R", iCont, "_Admitidos"), sprint("D_R", iCont, "_Desligados"), "D_star_Admitidos", "D_star_Desligados"});
        println("\tConcluido construcao da variavel Delta para a regiao ", iCont);
        // CONTRUCAO DA MATRIZ DE LONGO PRAZO
        println("(3) Iniciando construcao da variavel beta*Z (Cointegracao) para a regiao ", iCont);
        // Leitura do vetor de cointegracao
        // beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_R", iCont, ".mat")));
        //beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_R", iCont, ".mat")));
        beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_R_All.mat")));
        // Duvida devo considerar vetor zero para os desligados???
        // println(beta);
        // println(mData[1:10][]);
        // println(mData*beta');
        model.Append(mData * beta', {"betaZ_Admitidos", "betaZ_Desligados"});
        println("\tConcluido construcao da variavel beta*Z (Cointegracao) para a regiao ", iCont);
        // ADICIONANDO MACROVARIAVEIS
        println("(4) Iniciando adicao de macrovariaveis para a regiao ", iCont);
        // Leitura do vetor de cointegracao
        println("\tAdicao de macrovariaveis em nivel para a regiao ", iCont);
        //println(mMacroData[1:10][]);
        model.Append(mMacroData, aMacroVarNames);
        println("\tAdicao de macrovariaveis em primeira differenca para a regiao ", iCont);

        for (i = 0; i < rows(aMacroVarNames); ++i) {
            model.Append(diff(mMacroData[][i]), sprint("D_", aMacroVarNames[i]));
        }
        if (columns(mMacroData) > 1) {
            println("(5) Adicionando matriz de longo prazo das Macrovariaveis para a regiao ", iCont);
            beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_MacroVar.mat")));
            model.Append(mMacroData * beta', {"betaMacro"});
            println("\tConcluido adicao de macrovariaveis para a regiao ", iCont);
        }

        // Apago variaveis que nao serao mais utilizadas
        delete mData, beta;
        // Neste ponto o banco de dados ja contem todas as variaveis para serem estimadas.
        // Tanto variaveis em niveis quanto em primeira diferenca
        // INICIAMOS A MODELAGEM DO VAR PARA ESTIMACAO
        println("(6) Construindo modelo de estimacao da regiao ", iCont);
        //model.Info();
        model.SetModelClass("SYSTEM");
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
        // Vamos utilizar o sufixo "D_" para o modelo em primeira diferenca
        sVarSufix = "D_";
        //println(model.GetAllNames());
        //println(sprint(sVarSufix, "R", iCont,"_Desligados"));
        // adiciona variavel dependente (Adminitidos e Desligados)
        model.Select("Y", {sprint(sVarSufix, "R", iCont, "_Admitidos"), 0, 0});
        model.Select("Y", {sprint(sVarSufix, "R", iCont, "_Desligados"), 0, 0});

        if (iQtdLags > 0) {
            // adiciona a variavel dependente (lag da independente)
            model.Select("X", {sprint(sVarSufix, "R", iCont, "_Admitidos"), 1, iQtdLags});
            model.Select("X", {sprint(sVarSufix, "R", iCont, "_Desligados"), 1, iQtdLags});
        }

        // Adiciona a variavel "star"
        model.Select("X", {sprint(sVarSufix, "star_Desligados"), 0, iQtdLags});
        model.Select("X", {sprint(sVarSufix, "star_Admitidos"), 0, iQtdLags});
        // Adiciona a variavel "betaZ" com um lag apenas (representação da matriz de longo prazo)
        model.Select("X", {"betaZ_Admitidos", 1, 1});
        model.Select("X", {"betaZ_Desligados", 1, 1});

        // Adiciona a variaveis macroeconomicas com um lag apenas (representação da matriz de longo prazo)
        //decl nContVarMacro;
        for (i = 0; i < rows(aMacroVarNames); ++i) {
            model.Select("X", {sprint(sVarSufix, aMacroVarNames[i]), 0, iQtdLags});
            println("\tAdicionado: ", aMacroVarNames[i]);
        }
        //model.Select("X", {"betaMacro", 1, 1});
        // Adiciona variaveis constante e sesonals
        model.Select("U", {"Constant", 0, 0});
        model.Select("U", {"CSeasonal", 0, 10});
        // determina a janela de tempo do modelo
        //model.SetSample(model.GetYear1(),  model.GetPeriod1(),  model.GetYear2(),  model.GetPeriod2());
        //model.SetSample(1995, 1, 2018, 12);
        //model.SetSelSampleByIndex(1, 951);
        model.SetSelSampleByIndex(model.GetSelStart(), model.GetSelEnd());
        println("Model Sample: ", model.GetSelSample());

        // Liga o autometrics
		// (Mudar flag para TRUE, para estimar todos modelos com IIS)
        if ((iCont == 69) || (iCont == 84) || (iCont == 99) || TRUE) {
            model.Autometrics(0.00001, "IIS", 1);
        } else {
            model.Autometrics(0.01, "none", 1);
        }
		 

		// Desliga a impressao em tela do Autometrics
        model.AutometricsSet("print", 0);
        // determina o metodo de estimacao.
        model.SetMethod(M_OLS);
        // Realiza a estimacao do modelo
        model.Estimate();
//		model.TestSummary();
        println("Fazendo aquisicao de parametros");
        // Declaro as matrizes Phi e Lambda
        // Phi: Matrix de coeficientes do log da dependente
        // Lambda: Matrix de coeficientes das star
        // U: Matrix de coeficientes da constante e das dummies sazonais
        decl mPhi, mLambda, mLambda_0, mU, iContParam, iTotalParam, asParamNames, vParamValues, nContLags, A_0, A_L, mIIS, mAlpha, mD_macro;
        // inicia as matizes
        mPhi = zeros(iQtdVarDependente, iQtdVarDependente * iQtdLags);
        mLambda = zeros(iQtdVarDependente, iQtdVarDependente * (iQtdLags + 1));
        mU = zeros(iQtdVarDependente, (1 + 11));	//1: constante - 11:Seasonal
        // inicializa o total de parametros
        iTotalParam = model.GetParCount();
        // inicializa um vetor com o nome dos parametros
        asParamNames = model.GetParNames();
        // inicializa um vetor com o valor dos parametros
        vParamValues = model.GetPar();

        //Impressao dos parametros em Tela (Mudar flag para TRUE, para imprimir todos os modelos)
        if ((iCont == 69) || (iCont == 84) || (iCont == 99) || TRUE) {
            print("%r", asParamNames,
                  "%c", {"Coef", "Std.Err"}, "%14.4f", vParamValues ~ model.GetStdErr());
        }
        // Salva a matriz de IIS Para o modelo
        mIIS = ProcessoIIS(vParamValues, asParamNames, iCont, sprint(sVarSufix, "R"), model.GetYear1(), model.GetYear2());
        savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_IIS.mat"), mIIS);
        mAlpha = ProcessoPILongRun(vParamValues, asParamNames, iCont, sprint(sVarSufix, "R"));
        savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_Alpha.mat"), mAlpha);
        // Completa os valores da matrix Lambda
        mLambda = ProcessoLambda(mLambda, vParamValues, asParamNames, iQtdLags, iCont, sVarSufix);
        savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_Lambda.mat"), mLambda);
        mPhi = ProcessoPhi(mPhi, vParamValues, asParamNames, iQtdLags, iCont, sprint(sVarSufix, "R"));
        savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_Phi.mat"), mPhi);
        mU = ProcessoU(mU, vParamValues, asParamNames, iQtdLags, iCont, sprint(sVarSufix, "R"));
        //print(mU);
        savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_U.mat"), mU);
        // const iValue, const sName, const iRegDependente, const sVarSufix, const aMacroVarNames
        mD_macro = ProcessoMacroVariables(vParamValues, asParamNames, iQtdLags, iCont, sprint(sVarSufix, "R"), sVarSufix, aMacroVarNames);
        savemat(sprint(txMatPathRawMatrix, sVarSufix, "R", iCont, "_D.mat"), mD_macro);
        //mLambda = ProcessoPhi(mPhi, vParamValues, asParamNames, iQtdLags, iCont);
        // separo a matriz lambda em matriz lag-0 e demais lags
        mLambda_0 = mLambda[][0:iQtdVarDependente - 1];
        mLambda = mLambda[][iQtdVarDependente:];

        // Construção das matrizes A
        for (nContLags = 0; nContLags <= iQtdLags; ++nContLags) {
            if (nContLags == 0) {
                // Gero a matrix A_0
                A_0 = ( < 1, 0 > ** unit(iQtdVarDependente)) + ( < 0, -1 > ** mLambda_0);
                savemat(sprint(txMatPathA_Matrix, "A", iCont, "_", nContLags, ".mat"), A_0);
                //println(sprint(txMatPath, "A", iCont, "_", nContLags, ".mat"));
                //println(A_0);
            } else {
                A_L = ( < 1, 0 > ** mPhi[][(iQtdVarDependente * (nContLags - 1)):(((iQtdVarDependente * (nContLags - 1)) + iQtdVarDependente - 1))]) +
                      ( < 0, 1 > ** mLambda[][(iQtdVarDependente * (nContLags - 1)):(((iQtdVarDependente * (nContLags - 1)) + iQtdVarDependente - 1))]);
                savemat(sprint(txMatPathA_Matrix, "A", iCont, "_", nContLags, ".mat"), A_L);
                //println(sprint(txMatPathA_Matrix, "A", iCont, "_", nContLags, ".mat"));
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
        // Faz os calculos para determinar as matrizes Wi
        decl Wi_aux1, Wi_aux2, Wi;
        Wi_aux1 = zeros(1, iQtdRegioes);
        Wi_aux1[][iCont - 1] = 1;
        Wi_aux1 = Wi_aux1 ** unit(iQtdVarDependente);
        Wi_aux2 = mW[][iCont - 1]';
        Wi_aux2 = Wi_aux2 ** unit(iQtdVarDependente);
        Wi = Wi_aux1 | Wi_aux2;
        //println("Wi", Wi);
        savemat(sprint(txMatPathW_Matrix, "W", iCont, ".mat"), Wi);
        delete Wi_aux1;
        delete Wi_aux2;
        delete Wi;
        println("\nApagando o modelo PcGive referente a regiao ", iCont);
        delete model;
    }
    delete mW, i;
    println("*** Fim da estimacao dos modelos regionais *** \n");
}
