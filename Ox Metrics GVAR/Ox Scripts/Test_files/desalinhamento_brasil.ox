#include <oxstd.h>
#include <oxdraw.h>
#include <Database.h>
#include <oxdraw.oxh>
#import <packages/PcGive/pcgive_ects>
#import <modelbase>
#import <database>


// ##################################################################################################################################
//  CLASSES
// ##################################################################################################################################
class agregador_de_dados : Modelbase
{
	aggregator(const nome);		
};
agregador_de_dados::aggregator(const nome)
{
	decl  dados=GetAll();
	decl final=thinr(dados[GetFrequency()-GetPeriod1():][], rows(dados)/GetFrequency());
	decl media=aggregatec(dados, GetFrequency())/GetFrequency();
	decl nomes=GetAllNames();		   
	if (GetPeriod1()>1)
	{
	print("BASE DE DADOS NÃO COMEÇA NO INÍCIO DO ANO");
	};
	decl baseanual= new Modelbase();
	baseanual.Create(1, GetYear1(), 1, GetYear2(), 1);
	baseanual.Append(final,nomes,0);
 	baseanual.SaveIn7(sprint(nome,"EOP.in7"));
	delete baseanual;
	decl base= new Modelbase();

	print(GetPeriod2(), " -  " );
	if (GetPeriod2()==12)
	{
	base.Create(1, GetYear1(), 1, GetYear2(), 1);
	base.Append(media,nomes,0);
 	base.SaveIn7(sprint(nome,"AVE.in7"));
	};

	if (GetPeriod2()<12)
	{
	base.Create(1, GetYear1(), 1, GetYear2(), 1);
	media[GetYear2()-GetYear1()][]=(GetFrequency()/GetPeriod2())*media[GetYear2()-GetYear1()][];
//	base.Create(1, GetYear1(), 1, GetYear2()-1, 1);
//	base.Append(media[0:(GetYear2()-GetYear1()-1)][],nomes,0);
	base.Append(media[0:(GetYear2()-GetYear1())][],nomes,0);
 	base.SaveIn7(sprint(nome,"AVE.in7"));
	};
	delete base;
	return "OK";
	}
// ##################################################################################################################################
//  CLASSES
// ##################################################################################################################################
	class Emerson : PcGive
{
 	Emerson();
	GetAlpha();
	GetBeta();
	GetAlphaR();
	GetBetaR();
	GetDados();
	GetTime();
};

Emerson::Emerson()
{
	PcGive();
	print("Comecando com a rotina!");}
Emerson::GetAlpha()
{
	return m_mAlpha;
  }
Emerson::GetBeta()
{
	return m_mBeta;
  }
Emerson::GetAlphaR()
{
	return m_mAlphaRes;
// Restricted alpha
  }
Emerson::GetBetaR()
{
	return m_mBetaRes;
// Restricted  beta matrix
  }
Emerson::GetDados()
{
	return m_mY;
// Matriz de dados
  }
Emerson::GetTime()
{
	return m_cT;
// Matriz de dados
  }
// ##################################################################################################################################
//  ROTINAS AUXILIARES
// ##################################################################################################################################

run_tb_nfa_bspib_tot(const base_dados)
{// MODELO COMPLETO
	decl model = new Emerson();
	model.LoadIn7(sprint(base_dados, "EOP.in7"));
  	model.Deterministic(-1);
	model.Select(Y_VAR, {"LRER", 0, 0});
	model.Select(Y_VAR, {"TB", 0, 0});
	model.Select(Y_VAR, {"NFA", 0, 0});
	model.Select(Y_VAR, {"LBSPIB", 0, 0});
	model.Select(Y_VAR, {"LTOT", 0, 0});
	model.Select(Y_VAR, {"LRER", 1, 1});
	model.Select(Y_VAR, {"TB", 1, 1});
	model.Select(Y_VAR, {"NFA", 1, 1});
	model.Select(Y_VAR, {"LBSPIB", 1, 1});
	model.Select(Y_VAR, {"LTOT", 1, 1});
	model.Select(Y_VAR, {"LRER", 2, 2});
	model.Select(Y_VAR, {"TB", 2, 2});
	model.Select(Y_VAR, {"NFA", 2, 2});
	model.Select(Y_VAR, {"LBSPIB", 2, 2});
	model.Select(Y_VAR, {"LTOT", 2, 2});
	model.Select(X_VAR, {"Constant", 0, 0});
	model.SetModelClass(PcGive::MC_SYSTEM);
	model.SetSelSample(1978, 1, 2016, 1);
	model.SetMethod(M_OLS);
	model.Estimate();
	model.Cointegration();
	model.TestSummary();
	model.SetCointRank(2);
  	model.SetCointRestrict("&1=0; &2=0; &4=0;&6=0; &8=0;&10=1;&11=0;\r\n&16=0;&20=0;&17=60;&18=1;&19=0;&21=0;");
//  	model.SetCointRestrict("&10=1;&11=0;&16=0;&20=0;&18=1;&19=0;&21=0;");
	model.SetMethod(M_COINT);
	model.Estimate();
	model.TestSummary();
// FAZENDO GONZALO E GRANGER
	decl parAlpha = model.GetAlphaR();
	decl parBeta = model.GetBetaR();
	decl ab=(parBeta[0:4][])'*parAlpha;
	decl invab = invert(ab);
	decl Ainvab = parAlpha*invab;
	decl dados = model.GetDados();
    decl Y=dados~ones(model.GetTime(),1);
	decl CI= Y*parBeta;
    decl desalinhamento=(CI)*Ainvab';
	print("Dados:", dados);
	print("Parametros A:", parAlpha);
    print("Parametros B:", parBeta);
	print("Parametros A'*B:", ab);
	print("Inversa de A'*B:", invab);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CI);
	print("Desalinhamento:", desalinhamento);
	decl CF=  exp(dados[][0])~(exp(dados[][0]-desalinhamento[][0]));
	decl desalinhamentoI= ((exp(desalinhamento[][0]))-1)*100;
	model.Append(desalinhamentoI,"Desalinhamento-1-A",11);
	model.Append(CF[][1],"Fundamentals-1-A",11);
    model.SaveIn7(sprint(base_dados, "EOP.in7"));
	delete model;
// DADOS MENSAIS
 	decl modelM = new Emerson();
	modelM.LoadIn7(sprint(base_dados, ".in7"));
	modelM.Deterministic(-1);
	modelM.Select(Y_VAR, {"LRER", 0, 0});
	modelM.Select(Y_VAR, {"TB", 0, 0});
	modelM.Select(Y_VAR, {"NFA", 0, 0});
	modelM.Select(Y_VAR, {"LBSPIB", 0, 0});
	modelM.Select(Y_VAR, {"LTOT", 0, 0});
	modelM.Select(X_VAR, {"Constant", 0, 0});
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetSelSample(1978, 1, modelM.GetYear2(), modelM.GetPeriod2());
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetMethod(M_OLS);
	modelM.Estimate();
	decl dadosM = modelM.GetDados();
    decl YM=dadosM~ones(modelM.GetTime(),1);
	decl CIM= YM*parBeta;
    decl desalinhamentoM=(CIM)*Ainvab';
// ##############################################################################################
// Construindo os gráficos
	decl CFM=  exp(dadosM[][0])~(exp(dadosM[][0]-desalinhamentoM[][0]));
	decl desalinhamentoIM= ((exp(desalinhamentoM[][0]))-1)*100;
	print("Dados:", dadosM);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CIM);
	print("Desalinhamento:", desalinhamentoM);

	modelM.Remove("Desalinhamento-1-A");// teste
	modelM.Remove("Fundamentals-1-A");

	
	modelM.Append(desalinhamentoIM,"Desalinhamento-1-A",96);// teste
	modelM.Append(CFM[][1],"Fundamentals-1-A",96);

	
    modelM.SaveIn7(sprint(base_dados, ".in7"));
	delete modelM;
}


run_nfa_tb_bs_tot(const base_dados)
{
	decl model = new Emerson();
	model.LoadIn7(sprint(base_dados, "EOP.in7"));
	model.Deterministic(-1);
	model.Select(Y_VAR, {"LRER", 0, 0});
	model.Select(Y_VAR, {"TB", 0, 0});
	model.Select(Y_VAR, {"NFA", 0, 0});
	model.Select(Y_VAR, {"LBS", 0, 0});
	model.Select(Y_VAR, {"LTOT", 0, 0});   
	model.Select(Y_VAR, {"LRER", 1, 1});
	model.Select(Y_VAR, {"TB", 1, 1});
	model.Select(Y_VAR, {"NFA", 1, 1});
	model.Select(Y_VAR, {"LBS", 1, 1});
	model.Select(Y_VAR, {"LTOT", 1, 1});
	model.Select(Y_VAR, {"LRER", 2, 2});
	model.Select(Y_VAR, {"TB", 2, 2});
	model.Select(Y_VAR, {"NFA", 2, 2});
	model.Select(Y_VAR, {"LBS", 2, 2});
	model.Select(Y_VAR, {"LTOT", 2, 2});
	model.Select(X_VAR, {"Constant", 0, 0});
	model.SetModelClass(PcGive::MC_SYSTEM);
	model.SetSelSample(1978, 1, 2018, 1);
	model.SetMethod(M_OLS);
	model.Estimate();
	model.Cointegration();
	model.TestSummary();
	model.SetCointRank(4);
//	model.SetCointRestrict("&15=0; &16=1;&17=1.5; &18=0; &19=0; &20=0; &21=1; &22=0; &24=0; &25=0; &27=1; &28=0; &29=0; &30=1; ");

	//&17=1.5;
//	eta', the normalized cointegrating vectors:
//                 LRER_1        TB_1       NFA_1       LBS_1      LTOT_1    Constant
//     CVec(1)          0           1         1.5           0           0           0 
//                                                                                    
//     CVec(2)          1           0        -1.5           0           0        -4.7 
//                                        {  -6.1}                            { -56.5}
//     CVec(3)          1           0           0           1       -2.47        2.37 
//   {t-value}                                                    { -29.5}    {   6.4}

	
	model.SetSelSample(1978, 1, 2018, 1);
	model.SetMethod(M_COINT);
	model.Estimate();
	model.TestSummary();
//		model.Cointegration();
	decl parAlpha = model.GetAlphaR();
	decl parBeta = model.GetBetaR();
	decl ab=(parBeta[0:4][])'*parAlpha;
	decl invab = invert(ab);
	decl Ainvab = parAlpha*invab;
	decl dados = model.GetDados();
    decl Y=dados~ones(model.GetTime(),1);
	decl CI= Y*parBeta;
    decl desalinhamento=(CI)*Ainvab';
	decl f1c1 = (CI[][0]*Ainvab[0][0]);
	decl f2c2 = (CI[][1]*Ainvab[0][1]);
	decl f3c3 = (CI[][2]*Ainvab[0][2]);
	//print("Dados:", dados);
	print("Parametros A:", parAlpha);
    print("Parametros B:", parBeta);
	print("Parametros A'*B:", ab);
	print("Inversa de A'*B:", invab);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CI);
	print("Desalinhamento:", desalinhamento);
	print("DesVetor1",f1c1);
	print("DesVetor2",f2c2);
	print("DesVetor3",f3c3);
	decl CF=  exp(dados[][0])~(exp(dados[][0]-desalinhamento[][0]));
	decl desalinhamentoI= ((exp(desalinhamento[][0]))-1)*100;
	decl des1 = ((exp(f1c1[][0]))-1)*100;
	decl des2 = ((exp(f2c2[][0]))-1)*100;
	decl des3 = ((exp(f3c3[][0]))-1)*100;

	model.Remove("Desalinhamento-1-B");
	model.Remove("Fundamentals-1-B");
	model.Remove("DesVetor1");
	model.Remove("DesVetor2");
		model.Remove("DesVetor3");
    
	
	model.Append(desalinhamentoI,"Desalinhamento-1-B",11);
	model.Append(CF[][1],"Fundamentals-1-B",11);
	model.Append(des1,"DesVetor1",11);
	model.Append(des2,"DesVetor2",11);
	model.Append(des3,"DesVetor3",11);
    model.SaveIn7(sprint(base_dados, "EOP.in7"));
	delete model;

	// DADOS MENSAIS
 	decl modelM = new Emerson();
	modelM.LoadIn7(sprint(base_dados, ".in7"));
	modelM.Deterministic(-1);
	modelM.Select(Y_VAR, {"LRER", 0, 0});
	modelM.Select(Y_VAR, {"TB", 0, 0});
	modelM.Select(Y_VAR, {"NFA", 0, 0});
	modelM.Select(Y_VAR, {"LBS", 0, 0});
	modelM.Select(Y_VAR, {"LTOT", 0, 0});
	modelM.Select(X_VAR, {"Constant", 0, 0});
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetSelSample(1978, 1, modelM.GetYear2(), modelM.GetPeriod2());
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetMethod(M_OLS);
	modelM.Estimate();
	decl dadosM = modelM.GetDados();
    decl YM=dadosM~ones(modelM.GetTime(),1);
	decl CIM= YM*parBeta;
    decl desalinhamentoM=(CIM)*Ainvab';
		decl f1c1M = (CIM[][0]*Ainvab[0][0]);
	decl f2c2M = (CIM[][1]*Ainvab[0][1]);
	decl f3c3M = (CIM[][2]*Ainvab[0][2]);
	
	// ##############################################################################################
	// Construindo os gráficos
	decl CFM=  exp(dadosM[][0])~(exp(dadosM[][0]-desalinhamentoM[][0]));
	decl desalinhamentoIM= ((exp(desalinhamentoM[][0]))-1)*100;
	decl des1M = ((exp(f1c1M[][0]))-1)*100;
	decl des2M = ((exp(f2c2M[][0]))-1)*100;
	decl des3M = ((exp(f3c3M[][0]))-1)*100;
	print("Dados:", dadosM);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CIM);
	print("Desalinhamento:", desalinhamentoM);
	print("DesVetor1",f1c1M);
	print("DesVetor2",f2c2M);
		print("DesVetor2",f3c3M);

	modelM.Remove("Desalinhamento-1-B");
	modelM.Remove("Fundamentals-1-B");
	modelM.Remove("DesVetor1");
	modelM.Remove("DesVetor2");
		modelM.Remove("DesVetor3");

		
	
	modelM.Append(desalinhamentoIM,"Desalinhamento-1-B",96);
	modelM.Append(CFM[][1],"Fundamentals-1-B",96);
	modelM.Append(des1M,"DesVetor1",96);
	modelM.Append(des2M,"DesVetor2",96);
		modelM.Append(des3M,"DesVetor3",96);

	
    modelM.SaveIn7(sprint(base_dados, ".in7"));
    CloseDrawWindow();
	delete modelM;
}




run_nfa_bspib_tot(const base_dados)
{
	decl model = new Emerson();
	model.LoadIn7(sprint(base_dados,"EOP.in7"));
	model.Deterministic(-1);
	model.Select(Y_VAR, {"LRER", 0, 0});
	model.Select(Y_VAR, {"NFA", 0, 0});
	model.Select(Y_VAR, {"LBSPIB", 0, 0});
	model.Select(Y_VAR, {"LTOT", 0, 0});
	model.Select(Y_VAR, {"LRER", 1, 1});
	model.Select(Y_VAR, {"NFA", 1, 1});
	model.Select(Y_VAR, {"LBSPIB", 1, 1});
	model.Select(Y_VAR, {"LTOT", 1, 1});
	model.Select(Y_VAR, {"LRER", 2, 2});
	model.Select(Y_VAR, {"NFA", 2, 2});
	model.Select(Y_VAR, {"LBSPIB", 2, 2});
	model.Select(Y_VAR, {"LTOT", 2, 2});
	model.Select(X_VAR, {"Constant", 0, 0});
	model.SetModelClass(PcGive::MC_SYSTEM);
	model.SetSelSample(1978, 1, 2016, 1);
	model.SetMethod(M_OLS);
	model.Estimate();
	model.Cointegration();
	model.TestSummary();
	model.SetCointRank(1);
	model.SetCointRestrictMat(0, <1;
 0;
 0;
 0 >, <>);
	model.SetSelSample(1978, 1, 2016, 1);
	model.SetMethod(M_COINT);
	model.Estimate();
	model.TestSummary();
	decl parAlpha = model.GetAlphaR();
	decl parBeta = model.GetBetaR();
	decl ab=(parBeta[0:3][])'*parAlpha;
	decl invab = invert(ab);
	decl Ainvab = parAlpha*invab;
	decl dados = model.GetDados();
    decl Y=dados~ones(model.GetTime(),1);
	decl CI= Y*parBeta;
    decl desalinhamento=(CI)*Ainvab';

	print("Dados:", dados);
	print("Parametros A:", parAlpha);
    print("Parametros B:", parBeta);
	print("Parametros A'*B:", ab);
	print("Inversa de A'*B:", invab);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CI);
	print("Desalinhamento:", desalinhamento);  

	decl CF=  exp(dados[][0])~(exp(dados[][0]-desalinhamento[][0]));
	decl desalinhamentoI= ((exp(desalinhamento[][0]))-1)*100;
	model.Append(desalinhamentoI,"Desalinhamento-2-A",0);
	model.Append(CF[][1],"Fundamentals-2-A",0);
    model.SaveIn7(sprint(base_dados, "EOP.in7"));
	delete model;
	
	// DADOS MENSAIS
 	decl modelM = new Emerson();
	modelM.LoadIn7(sprint(base_dados, ".in7"));
	modelM.Deterministic(-1);
	modelM.Select(Y_VAR, {"LRER", 0, 0});  
	modelM.Select(Y_VAR, {"NFA", 0, 0});
	modelM.Select(Y_VAR, {"LBSPIB", 0, 0});
	modelM.Select(Y_VAR, {"LTOT", 0, 0});
	modelM.Select(X_VAR, {"Constant", 0, 0});
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetSelSample(1978, 1, modelM.GetYear2(), modelM.GetPeriod2()); 
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetMethod(M_OLS);
	modelM.Estimate();						
	decl dadosM = modelM.GetDados();
    decl YM=dadosM~ones(modelM.GetTime(),1);
	decl CIM= YM*parBeta;
    decl desalinhamentoM=(CIM)*Ainvab';

	// ##############################################################################################
	// Construindo os gráficos
	
	decl CFM=  exp(dadosM[][0])~(exp(dadosM[][0]-desalinhamentoM[][0]));
	decl desalinhamentoIM= ((exp(desalinhamentoM[][0]))-1)*100;


	modelM.Remove("Desalinhamento-2-A");
	modelM.Remove("Fundamentals-2-A");

	
	modelM.Append(desalinhamentoIM,"Desalinhamento-2-A",96);
	modelM.Append(CFM[][1],"Fundamentals-2-A",96);

	
    modelM.SaveIn7(sprint(base_dados, ".in7"));
	delete modelM;
}

run_nfa_bs_tot(const base_dados)
{
	decl model = new Emerson();
	model.LoadIn7(sprint(base_dados, "EOP.in7"));
	model.Deterministic(-1);
	model.Select(Y_VAR, {"LRER", 0, 0});
	model.Select(Y_VAR, {"NFA", 0, 0});
	model.Select(Y_VAR, {"LBS", 0, 0});
	model.Select(Y_VAR, {"LTOT", 0, 0});	
	model.Select(Y_VAR, {"LRER", 1, 1});
	model.Select(Y_VAR, {"NFA", 1, 1});
	model.Select(Y_VAR, {"LBS", 1, 1});
	model.Select(Y_VAR, {"LTOT", 1, 1});
	model.Select(Y_VAR, {"LRER", 2, 2});
	model.Select(Y_VAR, {"NFA", 2, 2});
	model.Select(Y_VAR, {"LBS", 2, 2});
	model.Select(Y_VAR, {"LTOT", 2, 2});
	model.Select(X_VAR, {"Constant", 0, 0});
	model.SetModelClass(PcGive::MC_SYSTEM);
	model.SetSelSample(1978, 1, 2016, 1);
	model.SetMethod(M_OLS);
	model.Estimate();
	model.Cointegration();
	model.TestSummary();
	model.SetCointRank(2);
 	model.SetCointRestrict("//	&1=0;&2=0;&4=0;&6=0;&8=1;&10=0;	&11=0; &9=-1.1;&13=0;&14=1;");

//	&1=0;&2=0;&4=0;&6=0;&8=1;&10=0;	&11=0; &9=-1;&13=0;&14=1;

// 	model.SetCointRestrict("&1=0; &2=0; &4=0; &6=0; &8=1; &13=1;");



//	model.SetCointRestrictMat(0, <1;
// 0;
// 0;
// 0 >, <>);
	model.SetSelSample(1978, 1, 2018, 1);
	model.SetMethod(M_COINT);
	model.Estimate();
	model.TestSummary();
	decl parAlpha = model.GetAlphaR();
	decl parBeta = model.GetBetaR();
	decl ab=(parBeta[0:3][])'*parAlpha;
	decl invab = invert(ab);
	decl Ainvab = parAlpha*invab;
	decl dados = model.GetDados();
    decl Y=dados~ones(model.GetTime(),1);
	decl CI= Y*parBeta;
    decl desalinhamento=(CI)*Ainvab';

	print("Dados:", dados);
	print("Parametros A:", parAlpha);
    print("Parametros B:", parBeta);
	print("Parametros A'*B:", ab);
	print("Inversa de A'*B:", invab);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CI);
	print("Desalinhamento:", desalinhamento);

	// ##############################################################################################
	// Construindo os gráficos
	decl CF=  exp(dados[][0])~(exp(dados[][0]-desalinhamento[][0]));
	decl desalinhamentoI= ((exp(desalinhamento[][0]))-1)*100;
	model.Append(desalinhamentoI,"Desalinhamento-2-B",11);
	model.Append(CF[][1],"Fundamentals-2-B",11);
    model.SaveIn7(sprint(base_dados, "EOP.in7"));
	delete model;
		// DADOS MENSAIS
 	decl modelM = new Emerson();
	modelM.LoadIn7(sprint(base_dados, ".in7"));
	modelM.Deterministic(-1);
	modelM.Select(Y_VAR, {"LRER", 0, 0});
	modelM.Select(Y_VAR, {"NFA", 0, 0});
	modelM.Select(Y_VAR, {"LBS", 0, 0});
	modelM.Select(Y_VAR, {"LTOT", 0, 0});
	modelM.Select(X_VAR, {"Constant", 0, 0});
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetSelSample(1978, 1, modelM.GetYear2(), modelM.GetPeriod2());
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetMethod(M_OLS);
	modelM.Estimate();
	decl dadosM = modelM.GetDados();
    decl YM=dadosM~ones(modelM.GetTime(),1);
	decl CIM= YM*parBeta;
    decl desalinhamentoM=(CIM)*Ainvab';
	// ##############################################################################################
	// Construindo os gráficos
	decl CFM=  exp(dadosM[][0])~(exp(dadosM[][0]-desalinhamentoM[][0]));
	decl desalinhamentoIM= ((exp(desalinhamentoM[][0]))-1)*100;

	modelM.Remove("Desalinhamento-2-B");
	modelM.Remove("Fundamentals-2-B");

	
	modelM.Append(desalinhamentoIM,"Desalinhamento-2-B",96);
	modelM.Append(CFM[][1],"Fundamentals-2-B",96);

	
    modelM.SaveIn7(sprint(base_dados,".in7"));
	delete modelM;
}

run_nfa_bspib(const base_dados)
{	decl model = new Emerson();
	model.LoadIn7(sprint(base_dados, "EOP.in7"));
	model.Deterministic(-1);
	model.Select(Y_VAR, {"LRER", 0, 0});
	model.Select(Y_VAR, {"NFA", 0, 0});
	model.Select(Y_VAR, {"LBSPIB", 0, 0});
	model.Select(Y_VAR, {"LRER", 1, 1});
	model.Select(Y_VAR, {"NFA", 1, 1});
	model.Select(Y_VAR, {"LBSPIB", 1, 1});
	model.Select(Y_VAR, {"LRER", 2, 2});
	model.Select(Y_VAR, {"NFA", 2, 2});
	model.Select(Y_VAR, {"LBSPIB", 2, 2});
	model.Select(X_VAR, {"Constant", 0, 0});
	model.SetModelClass(PcGive::MC_SYSTEM);
	model.SetSelSample(1972, 1, 2018, 1);
	model.SetMethod(M_OLS);
	model.Estimate();
	model.Cointegration();
	model.TestSummary();
	model.SetCointRank(1);
	model.SetCointRestrictMat(0, <1; 0; 0 >, <>);
	model.SetSelSample(1972, 1, 2018, 1);
	model.SetMethod(M_COINT);
	model.Estimate();
	model.TestSummary();
//		model.Cointegration();
	decl parAlpha = model.GetAlphaR();
	decl parBeta = model.GetBetaR();
	decl ab=(parBeta[0:2][])'*parAlpha;
	decl invab = invert(ab);
	decl Ainvab = parAlpha*invab;
	decl dados = model.GetDados();
    decl Y=dados~ones(model.GetTime(),1);
	decl CI= Y*parBeta;
    decl desalinhamento=(CI)*Ainvab';

	print("Dados:", dados);
	print("Parametros A:", parAlpha);
    print("Parametros B:", parBeta);
	print("Parametros A'*B:", ab);
	print("Inversa de A'*B:", invab);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CI);
	print("Desalinhamento:", desalinhamento);

	// ##############################################################################################
	// Construindo os gráficos
	decl CF=  exp(dados[][0])~(exp(dados[][0]-desalinhamento[][0]));
	decl desalinhamentoI= ((exp(desalinhamento[][0]))-1)*100;
	model.Append(desalinhamentoI,"Desalinhamento-3-A",2);
	model.Append(CF[][1],"Fundamentals-3-A",2);
    model.SaveIn7(sprint(base_dados,"EOP.in7"));
	delete model;

	// DADOS MENSAIS
 	decl modelM = new Emerson();
	modelM.LoadIn7(sprint(base_dados, ".in7"));
	modelM.Deterministic(-1);
	modelM.Select(Y_VAR, {"LRER", 0, 0});
	modelM.Select(Y_VAR, {"NFA", 0, 0});
	modelM.Select(Y_VAR, {"LBSPIB", 0, 0});

	modelM.Select(X_VAR, {"Constant", 0, 0});
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetSelSample(1970, 1, modelM.GetYear2(), modelM.GetPeriod2());
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetMethod(M_OLS);
	modelM.Estimate();
	decl dadosM = modelM.GetDados();
    decl YM=dadosM~ones(modelM.GetTime(),1);
	decl CIM= YM*parBeta;
    decl desalinhamentoM=(CIM)*Ainvab';

	// ##############################################################################################
	// Construindo os gráficos
	
	decl CFM=  exp(dadosM[][0])~(exp(dadosM[][0]-desalinhamentoM[][0]));
	decl desalinhamentoIM= ((exp(desalinhamentoM[][0]))-1)*100;

	modelM.Remove("Desalinhamento-3-A");
	modelM.Remove("Fundamentals-3-A");


	modelM.Append(desalinhamentoIM,"Desalinhamento-3-A",11);
	modelM.Append(CFM[][1],"Fundamentals-3-A",11);

	
    modelM.SaveIn7(sprint(base_dados, ".in7"));
	delete modelM;
	
}


 
run_nfa_bs(const base_dados)
{	decl model = new Emerson();
	model.LoadIn7(sprint(base_dados, "EOP.in7"));
	model.Deterministic(-1);
	model.Select(Y_VAR, {"LRER", 0, 0});
	model.Select(Y_VAR, {"NFA", 0, 0});
	model.Select(Y_VAR, {"LBS", 0, 0});
	model.Select(Y_VAR, {"LRER", 1, 1});
	model.Select(Y_VAR, {"NFA", 1, 1});
	model.Select(Y_VAR, {"LBS", 1, 1});	
	model.Select(Y_VAR, {"LRER", 2, 2});
	model.Select(Y_VAR, {"NFA", 2, 2});
	model.Select(Y_VAR, {"LBS", 2, 2});	
	model.Select(X_VAR, {"Constant", 0, 0});
	model.SetModelClass(PcGive::MC_SYSTEM);
	model.SetSelSample(1972, 1, 2018, 1);
	model.SetMethod(M_OLS);
	model.Estimate();
	model.Cointegration();
	model.TestSummary();
	model.SetCointRank(1);
	model.SetCointRank(1);
	model.SetCointRestrictMat(0, <1;
 0;
 0 >, <>);
	model.SetSelSample(1972, 1, 2018, 1);
	model.SetMethod(M_COINT);
	model.Estimate();
	model.TestSummary();
	decl parAlpha = model.GetAlphaR();
	decl parBeta = model.GetBetaR();
	decl ab=(parBeta[0:2][])'*parAlpha;
	decl invab = invert(ab);
	decl Ainvab = parAlpha*invab;
	decl dados = model.GetDados();
    decl Y=dados~ones(model.GetTime(),1);
	decl CI= Y*parBeta;
    decl desalinhamento=(CI)*Ainvab';
	print("Dados:", dados);
	print("Parametros A:", parAlpha);
    print("Parametros B:", parBeta);
	print("Parametros A'*B:", ab);
	print("Inversa de A'*B:", invab);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CI);
	print("Desalinhamento:", desalinhamento);

	decl CF=  exp(dados[][0])~(exp(dados[][0]-desalinhamento[][0]));
	decl desalinhamentoI= ((exp(desalinhamento[][0]))-1)*100;
	model.Append(desalinhamentoI,"Desalinhamento-3-B",0);
	model.Append(CF[][1],"Fundamentals-3-B",0);
    model.SaveIn7(sprint(base_dados, "EOP.in7"));
	delete model;

	// DADOS MENSAIS
 	decl modelM = new Emerson();
	modelM.LoadIn7(sprint(base_dados, ".in7"));
	modelM.Deterministic(-1);
	modelM.Select(Y_VAR, {"LRER", 0, 0});
	modelM.Select(Y_VAR, {"NFA", 0, 0});
	modelM.Select(Y_VAR, {"LBS", 0, 0});
	modelM.Select(X_VAR, {"Constant", 0, 0});
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetSelSample(1970, 1, modelM.GetYear2(), modelM.GetPeriod2());
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetMethod(M_OLS);
	modelM.Estimate();
	decl dadosM = modelM.GetDados();
    decl YM=dadosM~ones(modelM.GetTime(),1);
	decl CIM= YM*parBeta;
    decl desalinhamentoM=(CIM)*Ainvab';

	// ##############################################################################################
	// Construindo os gráficos
	
	decl CFM=  exp(dadosM[][0])~(exp(dadosM[][0]-desalinhamentoM[][0]));
	decl desalinhamentoIM= ((exp(desalinhamentoM[][0]))-1)*100;

	modelM.Remove("Desalinhamento-3-B");
	modelM.Remove("Fundamentals-3-B");

	
	modelM.Append(desalinhamentoIM,"Desalinhamento-3-B",11);
	modelM.Append(CFM[][1],"Fundamentals-3-B",11);


	modelM.SaveIn7(sprint(base_dados, ".in7"));
	delete modelM;
	
}


run_nfa(const base_dados)
{// This program requires a licenced version of PcGive Professional.
	//--- Ox code for SYS( 1)

	decl model = new Emerson();
			model.LoadIn7(sprint(base_dados, "EOP.in7"));
	model.Deterministic(-1);
	model.Select(Y_VAR, {"LRER", 0, 0});
	model.Select(Y_VAR, {"NFA", 0, 0});
	model.Select(Y_VAR, {"LRER", 1, 1});
	model.Select(Y_VAR, {"NFA", 1, 1});
	model.Select(Y_VAR, {"LRER", 2, 2});
	model.Select(Y_VAR, {"NFA", 2, 2});
	model.Select(X_VAR, {"Constant", 0, 0});
	model.SetModelClass(PcGive::MC_SYSTEM);
  	model.SetSelSample(1972, 1, 2018, 1);
   	model.SetMethod(M_OLS);
	model.Estimate();
	model.Cointegration();
	model.TestSummary();
	model.SetCointRank(1);
	model.SetCointRank(1);
	model.SetCointRestrictMat(0, <1;
 0 >, <>);
	model.SetSelSample(1972, 1, 2017, 3);
	model.SetMethod(M_COINT);
	model.Estimate();
	model.TestSummary();
	decl parAlpha = model.GetAlphaR();
	decl parBeta = model.GetBetaR();
	decl ab=(parBeta[0:1][])'*parAlpha;
	decl invab = invert(ab);
	decl Ainvab = parAlpha*invab;
	decl dados = model.GetDados();
    decl Y=dados~ones(model.GetTime(),1);
	decl CI= Y*parBeta;
    decl desalinhamento=(CI)*Ainvab';
	print("Dados:", dados);
	print("Parametros A:", parAlpha);
    print("Parametros B:", parBeta);
	print("Parametros A'*B:", ab);
	print("Inversa de A'*B:", invab);
	print("Pesos dos vetores:", Ainvab);
	print("Valor dos vetores:", CI);
	print("Desalinhamento:", desalinhamento);

	decl CF=  exp(dados[][0])~(exp(dados[][0]-desalinhamento[][0]));
	decl desalinhamentoI= ((exp(desalinhamento[][0]))-1)*100;
	model.Append(desalinhamentoI,"Desalinhamento-4",2);
	model.Append(CF[][1],"Fundamentals-4",2);
    model.SaveIn7(sprint(base_dados, "EOP.in7"));
	delete model;


			// DADOS MENSAIS
 	decl modelM = new Emerson();
	modelM.LoadIn7(sprint(base_dados, ".in7"));
	modelM.Deterministic(-1);
	modelM.Select(Y_VAR, {"LRER", 0, 0});
	modelM.Select(Y_VAR, {"NFA", 0, 0});
	modelM.Select(Y_VAR, {"LRER", 1, 1});
	modelM.Select(X_VAR, {"Constant", 0, 0});
	modelM.SetModelClass(PcGive::MC_SYSTEM);
	modelM.SetSelSample(1970, 1, modelM.GetYear2(), modelM.GetPeriod2());
   	modelM.SetModelClass(PcGive::MC_SYSTEM);
   	modelM.SetMethod(M_OLS);
	modelM.Estimate();
	decl dadosM = modelM.GetDados();
    decl YM=dadosM~ones(modelM.GetTime(),1);
	decl CIM= YM*parBeta;
    decl desalinhamentoM=(CIM)*Ainvab';
   	// ##############################################################################################
	decl CFM=  exp(dadosM[][0])~(exp(dadosM[][0]-desalinhamentoM[][0]));
	decl desalinhamentoIM= ((exp(desalinhamentoM[][0]))-1)*100;

	modelM.Remove("Desalinhamento-4");
	modelM.Remove("Fundamentals-4");

	
	modelM.Append(desalinhamentoIM,"Desalinhamento-4",11);
	modelM.Append(CFM[][1],"Fundamentals-4",11);

	
    modelM.SaveIn7(sprint(base_dados, ".in7"));
	delete modelM;
	
}

desalinhamento_medio(const base_dados)
{
 decl model = new Database() ;
 model.LoadIn7(sprint(base_dados, ".in7"));
 decl F1=model.GetVar({"Fundamentals-1-A","Fundamentals-2-A","Fundamentals-3-A","Fundamentals-4"});
 decl F2=model.GetVar({"Fundamentals-1-B","Fundamentals-2-B","Fundamentals-3-B","Fundamentals-4"});
 //decl D1=model.GetVar({"Desalinhamento-1-A","Desalinhamento-2-A","Desalinhamento-3-A","Desalinhamento-4"});
 //decl D2=model.GetVar({"Desalinhamento-1-B","Desalinhamento-2-B","Desalinhamento-3-B","Desalinhamento-4"});

 decl rer=model.GetVar({"RER"});
 
 decl FM1=meanr(F1);
 decl FM2=meanr(F2);
 decl DM1=((rer./FM1)-1)*100;
 decl DM2=((rer./FM2)-1)*100;


 model.Remove("Desalinhamento-Medio-1");
 model.Remove("Fundamentos-Medio-1");
 model.Remove("Desalinhamento-Medio-2"); 
 model.Remove("Fundamentos-Medio-2");


 
 model.Append(DM1,"Desalinhamento-Medio-1");
 model.Append(FM1,"Fundamentos-Medio-1");


 model.Append(DM2,"Desalinhamento-Medio-2"); 
 model.Append(FM2,"Fundamentos-Medio-2");

 model.SaveIn7(sprint(base_dados, ".in7"));

 delete model;
 
 return 1 ;
}


graficos(const base_dados, const periodo, const frequencia, const lingua)
{

 decl model = new Database() ;
 	model.LoadIn7(sprint(base_dados, ".in7"));
 	decl variaveis={"RER",
 	"Desalinhamento-1-B","Fundamentals-1-B",
	"Desalinhamento-2-B", "Fundamentals-2-B",
	"Desalinhamento-3-B", "Fundamentals-3-B",
	"Desalinhamento-4","Fundamentals-4",
	"Desalinhamento-Medio-1","Fundamentos-Medio-1",
	"Desalinhamento-Medio-2","Fundamentos-Medio-2",
	"DesVetor1", "DesVetor2","DesVetor3"}	 ;
  
	decl F1=model.GetVar(variaveis);
 	SetDraw (SET_BOX, 0, 1, 1);
	decl x=F1[8*model.GetFrequency():][0]~F1[8*model.GetFrequency():][12]	 ;	// o 12 significa que pega o Medio 2

	if (lingua==0)
	{
	DrawTMatrix(0, x',{"Real Effective Exchange Rate","Fundamentals"},1978,1,frequencia);
	DrawTMatrix(1, F1[8*model.GetFrequency():][11]',{"Average Exchange Rate Misalignment"},1978,1,frequencia,ST_BARS);
	DrawText(0, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Index",0 ,0, -1, -1, TEXT_YLABEL);	
		}
	else
	{
	DrawTMatrix(0, x',{"Taxa de cambio real efetiva","Fundamento m\\'edio"},1978,1,frequencia);
	DrawTMatrix(1, F1[8*model.GetFrequency():][11]',{"Desalinhamento cambial m\\'edio"},1978,1,frequencia,ST_BARS);
	DrawText(0, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Índice",0 ,0, -1, -1, TEXT_YLABEL);	
	}
	
	SetDraw(SET_BOX, 1, 1, 1);
	
	DrawAdjust(ST_BARS,1);
	ShowDrawWindow();	
	
	SaveDrawWindow(sprint(base_dados, "-grafico-desalinhamento-", sprint(periodo),"-",sprint(lingua), ".pdf"));
	CloseDrawWindow();


	//Gráfico 1-B
	decl F3=model.GetVar(variaveis);
 	SetDraw (SET_BOX, 0,1,1);
	decl y=F3[8*model.GetFrequency():][0]~F3[8*model.GetFrequency():][2]	 ;	  ///	 0 é o RER 
	decl des1A=F3[8*model.GetFrequency():][1]	 ;	  ///	   // 1 é o desalinhamento	1-A
	
    if (lingua==0)
	{
	DrawTMatrix(0, y',{"Real Effective Exchange Rate"},1978,1,frequencia);
	DrawTMatrix(1, des1A',{"Exchange Rate Misalignment-RER TB NFA BS TOT"},1978,1,frequencia,ST_BARS);
	DrawText(0, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Index",0 ,0, -1, -1, TEXT_YLABEL);		
	}
	else
	{
	DrawTMatrix(0, y',{"Taxa de c$\^a$mbio real efetiva"},1978,1,frequencia);
	DrawTMatrix(1, des1A',{"Desalinhamento cambial-RER TB NFA BS TOT"},1978,1,frequencia,ST_BARS);
	DrawText(0, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Indice",0 ,0, -1, -1, TEXT_YLABEL);	
	}
	
	SetDraw(SET_BOX, 1, 1,1);
	
	DrawAdjust(ST_BARS,1);
	ShowDrawWindow();	
	
	SaveDrawWindow(sprint(base_dados, "-grafico-desalinhamento1B-", sprint(periodo),"-",sprint(lingua),"-Brasil",".pdf"));
	CloseDrawWindow();


//Gráfico 2-B				 
				 
	decl F5=model.GetVar(variaveis);
 	SetDraw (SET_BOX, 0,1,1);
	decl w=F5[8*model.GetFrequency():][0]~F5[8*model.GetFrequency():][4]	 ;	  ///	 0 é o RER 
	decl des2A=F5[8*model.GetFrequency():][3]	 ;	  ///	   // 3 é o desalinhamento	2-A
	
    if (lingua==0)
	{
	DrawTMatrix(0, w',{"Real Effective Exchange Rate"},1978,1,frequencia);
	DrawTMatrix(1, des2A',{"Exchange Rate Misalignment-RER NFA TOT BS"},1978,1,frequencia,ST_BARS);
	DrawText(0, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Index",0 ,0, -1, -1, TEXT_YLABEL);		
	}
	else
	{
	DrawTMatrix(0, w',{"Taxa de c$\^a$mbio real efetiva"},1978,1,frequencia);
	DrawTMatrix(1, des2A',{"Desalinhamento cambial-RER NFA TOT BS"},1978,1,frequencia,ST_BARS);
	DrawText(0, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Indice",0 ,0, -1, -1, TEXT_YLABEL);	
	}
	
	SetDraw(SET_BOX, 1, 1,1);
	
	DrawAdjust(ST_BARS,1);
	ShowDrawWindow();	
	
	SaveDrawWindow(sprint(base_dados, "-grafico-desalinhamento2B-", sprint(periodo),"-",sprint(lingua),"-Brasil",".pdf"));
	CloseDrawWindow();


//Gráfico 3-B				 
				 
	decl F6=model.GetVar(variaveis);
 	SetDraw (SET_BOX, 0,1,1);
	decl k=F6[model.GetFrequency():][0]~F6[model.GetFrequency():][6]	 ;	  ///	 0 é o RER 
	decl des3A=F6[model.GetFrequency():][5]	 ;	  ///	   // 5 é o desalinhamento	3-A
	
    if (lingua==0)
	{
	DrawTMatrix(0, k',{"Real Effective Exchange Rate"},1970,12,frequencia);
	DrawTMatrix(1, des3A',{"Exchange Rate Misalignment-RER NFA BS"},1970,12,frequencia,ST_BARS);
	DrawText(0, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Index",0 ,0, -1, -1, TEXT_YLABEL);		
	}
	else
	{
	DrawTMatrix(0, k',{"Taxa de c$\^a$mbio real efetiva"},1970,12,frequencia);
	DrawTMatrix(1, des3A',{"Desalinhamento cambial-RER NFA BS"},1970,12,frequencia,ST_BARS);
	DrawText(0, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Índice",0 ,0, -1, -1, TEXT_YLABEL);	
	}
	
	SetDraw(SET_BOX, 1, 1,1);
	
	DrawAdjust(ST_BARS,1);
	ShowDrawWindow();	
	
	SaveDrawWindow(sprint(base_dados, "-grafico-desalinhamento3B-", sprint(periodo),"-",sprint(lingua),"-Brasil",".pdf"));
	CloseDrawWindow();


//Gráfico 4				 
				 
	decl F7=model.GetVar(variaveis);
 	SetDraw (SET_BOX, 0,1,1);
	decl j=F7[model.GetFrequency():][0]~F7[model.GetFrequency():][8]	 ;	  ///	 0 é o RER 
	decl des4=F7[model.GetFrequency():][7]	 ;	  ///	   // 7 é o desalinhamento	4
	
    if (lingua==0)
	{
	DrawTMatrix(0, j',{"Real Effective Exchange Rate"},1970,12,frequencia);
	DrawTMatrix(1, des4',{"Exchange Rate Misalignment-RER NFA"},1970,12,frequencia,ST_BARS);
	DrawText(0, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Index",0 ,0, -1, -1, TEXT_YLABEL);		
	}
	else
	{
	DrawTMatrix(0, j',{"Taxa de c$\^a$mbio real efetiva"},1970,12,frequencia);
	DrawTMatrix(1, des4',{"Desalinhamento cambial-RER NFA"},1970,12,frequencia,ST_BARS);
	DrawText(0, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Índice",0 ,0, -1, -1, TEXT_YLABEL);	
	}
	
	SetDraw(SET_BOX, 1, 1,1);
	
	DrawAdjust(ST_BARS,1);
	ShowDrawWindow();	
	
	SaveDrawWindow(sprint(base_dados, "-grafico-desalinhamento4-", sprint(periodo),"-",sprint(lingua),"-Brasil",".pdf"));
	CloseDrawWindow();


	//Gráfico 1-B separando os vetores				 
				 
	decl Fx=model.GetVar(variaveis);
 	SetDraw (SET_BOX, 0,1,1);
	decl sep_2b1=Fx[8*model.GetFrequency():][13]	 ;	  ///	 13 e 14 são as partes
	decl sep_2b2=Fx[8*model.GetFrequency():][14]	 ;	  ///	 13 e 14 são as partes
	decl sep_2b3=Fx[8*model.GetFrequency():][15]	 ;	  ///	 13 e 14 são as partes 
	decl total_2b=Fx[8*model.GetFrequency():][1]	 ;	  ///	   // 5 é o desalinhamento	1b
	
    if (lingua==0)
	{
	DrawTMatrix(0, total_2b',{"Exchange Rate Misalignment-RER NFA"},1978,12,frequencia,ST_BARS,2);
	DrawTMatrix(0, sep_2b1',{"Fundamentals Contribution"},1978,12,frequencia,ST_LINE,1);
	DrawTMatrix(0, sep_2b2',{"TB-NFA Disequilibrium Contribution"},1978,12,frequencia,ST_LINE,3);
	DrawTMatrix(0, sep_2b3',{"TB-NFA Disequilibrium Contribution-2"},1978,12,frequencia,ST_LINE,3);
	DrawText(0, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Index",0 ,0, -1, -1, TEXT_YLABEL);

	}
	else
	{
	DrawTMatrix(0, total_2b',{"Desalinhamento cambial-RER NFA BS"},1978,12,frequencia,ST_BARS,2);
	DrawTMatrix(0, sep_2b1',{"Taxa de c$\^a$mbio real efetiva"},1978,12,frequencia,ST_LINE,1);
	DrawTMatrix(0, sep_2b2',{"Taxa de c\\^ambio real efetiva"},1978,12,frequencia,ST_LINE,3);
	DrawText(0, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "Índice",0 ,0, -1, -1, TEXT_YLABEL);	
	}
	
	
	SetDraw(SET_BOX,1,1,1);

	DrawAdjust(ST_BARS,1);
	ShowDrawWindow();	
	
	SaveDrawWindow(sprint(base_dados, "-grafico-desalinhamento1B-partes", sprint(periodo),"-",sprint(lingua),"-Brasil",".pdf"));
	CloseDrawWindow();
	
 delete model;

 decl model1 = new Database() ;
 	model1.LoadIn7(sprint(base_dados, ".in7"));
 	decl nomes={"NFA","TB","TOT", "BS"}	 ;
  
	decl F2=model1.GetVar(nomes);
 	SetDraw (SET_BOX, 0, 1, 1);

	if (lingua==0)
	{
	DrawTMatrix(0, F2[][0]',{"Net Foreign Asset Position"},1970,1,frequencia);
	DrawTMatrix(1, F2[][1]',{"Trade Balance of Goods and Services"},1970,1,frequencia);
    DrawTMatrix(2, F2[][2]',{"Relative Terms of Trade"},1970,1,frequencia);
	DrawTMatrix(3, F2[][3]',{"Relative Price of Tradeables and non tradeable"},1970,1,frequencia); 
	DrawText(0, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "GDP Share",0 ,0, -1, -1, TEXT_YLABEL);
	DrawText(1, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(1, "GDP Share",0 ,0, -1, -1, TEXT_YLABEL);
	DrawText(2, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(2, "Index",0 ,0, -1, -1, TEXT_YLABEL);
	DrawText(3, "Year",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(3, "Index",0 ,0, -1, -1, TEXT_YLABEL);
	}
	else
	{DrawTMatrix(0, F2[][0]',{"Posic$\~a$o Internacional de Investimento"},1970,1,frequencia);
	DrawTMatrix(1, F2[][1]',{"Balanço de Bens e Servicos"},1970,1,frequencia);
    DrawTMatrix(2, F2[][2]',{"Termos de Troca"},1970,1,frequencia);
	DrawTMatrix(3, F2[][3]',{"Preço relativo de bens comercializ$\'a$veis e n$\~a$o-comercializ$\'a$veis"},1970,1,frequencia);
	DrawText(0, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(0, "% do PIB",0 ,0, -1, -1, TEXT_YLABEL);
	DrawText(1, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(1, "% do PIB",0 ,0, -1, -1, TEXT_YLABEL);
	DrawText(2, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(2, "Indice",0 ,0, -1, -1, TEXT_YLABEL);
	DrawText(3, "Ano",0 ,0, -1, -1, TEXT_XLABEL);
	DrawText(3, "Indice",0 ,0, -1, -1, TEXT_YLABEL);
	}
	
	ShowDrawWindow();

	SaveDrawWindow(sprint(base_dados, "-grafico-fundamentos-", sprint(periodo),"-", sprint(lingua), ".pdf"));
	CloseDrawWindow();
 delete model1;
 return 1;




 return 1;
}



// ###########################################################################################################################################################
main()
{			  
	decl model = new agregador_de_dados();
	decl x="data_brazil";
	model.LoadIn7(sprint(x,".in7"));
	model.aggregator(x);
	delete model;
	run_tb_nfa_bspib_tot("data_brazil");// 1-A
	run_nfa_tb_bs_tot("data_brazil"); //1-B
	run_nfa_bspib_tot("data_brazil"); // 2-A
	run_nfa_bs_tot("data_brazil");	// 2-B
	run_nfa_bspib("data_brazil"); // 3-A
	run_nfa_bs("data_brazil");	// 3-B
	run_nfa("data_brazil"); // 4 

	desalinhamento_medio("data_brazil");


	decl model2 = new agregador_de_dados();
	model2.LoadIn7(sprint(x,".in7"));
	model2.aggregator(x);
	delete model2;

	desalinhamento_medio("data_brazilAVE");
	
	graficos("data_brazil", "mensal", 12,0);
	graficos("data_brazilAVE", "anual", 1,0);

	
	graficos("data_brazil", "mensal", 12,1);
	graficos("data_brazilAVE", "anual", 1,1);
	
}