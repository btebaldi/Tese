#include <oxstd.oxh>
#import <packages/CATS/CATS>

// ##################################################################################################################################
//  CLASSES
// ##################################################################################################################################
class Cointegration {
    decl iRank;
	decl iLags;

	decl mX;
	decl asX;
	
	decl mX_star;
	decl asX_star;

	Cointegration();
	// estima rank do modelo
	myCATS();

	// estima estima vetores dado r model.estimateBeta
	// EstimateBeta(const iRank);
	
	// Adiciona as variaveis X
	Add_X(const mData, const asNames);
	
	// Adiciona as variaveis X_star
	Add_X_star(const mData, const asNames);

	// Inicializa a contagem dos ranks
	InitRank();
	Info();
	SetRank();
        // faz estimacao model.estimate
        // estima rank model.estimateRank
        
        
        // save beta 
        // ELSE LE BETA
        // Leitura do vetor de cointegracao
        // beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_R", iCont, ".mat")));
        //beta = loadmat(sprint(txCoIntMatPath, sprint("CoInt_R", iCont, ".mat")));

};
//Constructor class
Cointegration::Cointegration(){
	iLags = 2;
}
// Metodo Add_X
Cointegration::Add_X(const mData, const asNames){
	mX = mData;
	asX = asNames;
}
// Metodo Add_X_Star
Cointegration::Add_X_star(const mData, const asNames){
	mX_star = mData;
	asX_star = asNames;
}
// Metodo inicializa o rank
Cointegration::InitRank(){
	iRank = columns(mX) + columns(mX_star);
}
// Seta o rank para valor especifico
Cointegration::SetRank(const iValue){
	iRank = iValue;
}
// Model Info
Cointegration::Info(){
	println("Rank: ", iRank);
	println("Lags: ", iLags);

	println("X Variables: ", asX);
	println("X* Variables: ", asX_star);
}
// Cointegration::EstimateBeta(const iRank){
//     println("EstimateBeta irank ", iRank);
// 	println("EstimateBeta irank2 ", iRank2);
//     return "OK";
// }
Cointegration::myCATS(){
	
	//--- Ox code for CATS( 8)
	decl model = new CATS();

	// Adiciona variaveis X ao banco de dados
	model.Append(mX, asX);

	// Adiciona variaveis X_star ao banco de dados
	model.Append(mX, asX);

	// Adiciona as variaveis X como exogenas
	for(decl iqtd = 0; iqtd < columns(asX); iqtd++) {
            model.Select("Y", {asX[iqtd], 0, 0});
	}

	for(decl iqtd = 0; iqtd < columns(asX_star); iqtd++) {
            // model.Select("Y", {asX[iqtd], 0, 0});
			model.Select("X", {asX[iqtd], 0, 0});
	}

	// Lags ( const lags , const lagsExo = - 1 , const lagsDet = - 1 )
	model.Lags(iLags, iLags, iLags);
	
	// Rank inicial (mudar para a quantidade de variaveis.)
	model.I1Rank(iRank);
	
	// Tipo de cointegracao CIMEAN: Constante no espaço de cointegracao.
	model.Trend("CIMEAN");
	// Inclui seasonal centradas
	model.Seasonals(1);
	// fixa a amostra
	// model.SetSelSample(1975, 9, 1998, 12);
	// tipo de metodo RRR: Reduced Rank Regression
	model.SetMethod("RRR");

	// Estima o modelo.
	model.Estimate();

	delete model;
}
// Cointegration::EstimateRank(){
// // 	// println("EstimateBeta irank2 ", iRank2);
// //     // return "OK";
// }
main()
{
	//--- Ox code for CATS( 8)
	decl model = new CATS();

	model.Load("C:\\Program Files\\OxMetrics8\\CATS\\German-US-PPP\\PPPuip.in7");
	model.Select("Y", {"dp1c", 0, 0});
	model.Select("Y", {"dp2", 0, 0});
	model.Select("Y", {"b1", 0, 0});
	model.Select("Y", {"b2", 0, 0});
	model.Select("Y", {"ppp", 0, 0});
	model.Select("X", {"C(1991:1)", 0, 0});
//	model.Select("U", {"CSeason", 0, 0});
//	model.Select("U", {"CSeason_1", 0, 0});
//	model.Select("U", {"CSeason_2", 0, 0});
//	model.Select("U", {"CSeason_3", 0, 0});
//	model.Select("U", {"CSeason_4", 0, 0});
//	model.Select("U", {"CSeason_5", 0, 0});
//	model.Select("U", {"CSeason_6", 0, 0});
//	model.Select("U", {"CSeason_7", 0, 0});
//	model.Select("U", {"CSeason_8", 0, 0});
//	model.Select("U", {"CSeason_9", 0, 0});
//	model.Select("U", {"CSeason_10", 0, 0});
//	model.Select("X", {"Constant", 0, 0});

	model.Lags(2,2,2);
	model.I1Rank(5);
	model.Trend("CIMEAN");
	model.Seasonals(1);
	model.SetSelSample(1975, 9, 1998, 12);
	model.SetMethod("RRR");
	model.Estimate();

	println("MY TRACE TEST");
	println(model.GetTraceTest());


	println(model.DoI1RankTable(0.05, TRUE));

	println("GET BETA");
	println(model.GetBeta());
							 println(model.GetBeta());

							 println("RANK ORDER");
							 println(model.m_iR);
							 
	
	delete model;
}
