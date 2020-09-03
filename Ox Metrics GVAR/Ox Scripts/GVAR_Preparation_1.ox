#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>

concatenate(a, b, const sep)
{
	decl nCont;
	for(nCont=0; nCont<rows(b); ++nCont)
	{
		b[nCont] = sprint(a, sep, b[nCont]);
	}

	return b;
}

main()
{
	// enter code
	//diff(const ma);


	// Variáveis do programa
	decl iQtdVarDependente, iQtdRegioes, iQtdLags, txMatPath, txDbase, txCointegrataionDirPath;

	iQtdVarDependente = 2; // NAO MUDAR ISSO !!! (SE MUDAR FAZER REVISAO DO CODIGO)
	iQtdLags = 2;
	iQtdRegioes = 5; // Isso devia vir de um arquivo e configuracao!!!

	// Configuracao dos diretorios
	txMatPath = "./mat_files/";

	// Diretorio com as matrizes de cointegracao
	
	txCointegrataionDirPath = "./mat_files/Cointegration/";
	txDbase = "../Database/GrandeRegiao.in7";
	

	
	println("Carregando base de dados");
	decl dbase, mData, aVarNames, nCont;
	dbase = new Database();
	dbase.Load(txDbase);
	dbase.Info();

//	println("GetGroupNames: ", dbase.GetGroupNames(1,1));
	
//	println("dbase.GetAllNames():", dbase.GetAllNames());
//	println("IsDated():",dbase.IsDated());
	
	println("Carregando dados");
	aVarNames = dbase.GetAllNames();
	mData = dbase.GetAll();
								
	dbase.Append(diff(mData[][1:]), concatenate("D", aVarNames[1:], "_"));

	/*
	for(nCont=1; nCont <=5; ++nCont){
	dbase.Select(1, {sprint("R",nCont,"_Desligados"), 0, 0});
	}
	
	dbase.SetSelSample(2004, 1, 2008, 12);
	
	println(" GROUPRRRRRRRRRRRRR", dbase.GetGroup(1));
	*/
	//SetSelSample(2004, 1, 2008, 12);
	
	dbase.Info();
	delete dbase;







	  /*



	
	println("Carregando matrix de pessos W");
	decl mW;
	mW = loadmat(sprint(txMatPath, "data.mat"));
	println("mW", mW);

	println("*** Iniciando estimacao dos modelos *** \n");




	
	println("Carregando matrix de pessos W");
	decl mW;
	mW = loadmat(sprint(txMatPath, "data.mat"));
	print(mW);
	*/
	
}
