#include <oxstd.oxh>
#import <packages/PcGive/pcgive_ects>


GetRegionNames(const iQtdRegioes, const sVarPrefix, const sVarPosfix){

	decl nCont, aNames;
	for(nCont=1; nCont<=iQtdRegioes; ++nCont)
	{
		if(nCont == 1){
		aNames = {sprint(sVarPrefix, nCont, sVarPosfix)};
		}
		else
		{
		aNames = aNames ~ {sprint(sVarPrefix, nCont, sVarPosfix)};
		}
	}

	return aNames;
}

main()
{
	// enter code

	decl iQtdRegioes, sVarPrefix, sVarPosfix, mData, iCont;

	iQtdRegioes = 3;
	
	decl dbase = new Database();


	println("Carregando matrix de pessos W");
	decl mW;
	mW = loadmat(sprint("../mat_files/W_Matrix/", "data.mat"));
	//println(mW);

	dbase.Load("../../Database/teste_1_data.in7");

	for (iCont = 1; iCont <= iQtdRegioes; ++iCont)
	{
		mData = dbase.GetVar(GetRegionNames(iQtdRegioes, "R", "_Desligados"));
	 	dbase.Append(mData*mW[][iCont-1], {sprint("R", iCont,"_star_Desligados")});
	}


	for (iCont = 1; iCont <= iQtdRegioes; ++iCont)
	{
		mData = dbase.GetVar(GetRegionNames(iQtdRegioes, "R", "_Admitidos"));
	 	dbase.Append(mData*mW[][iCont-1], {sprint("R", iCont,"_star_Admitidos")});
	}

	mData = dbase.GetVar(GetRegionNames(iQtdRegioes, "R", "_Admitidos"));
	dbase.Append(diff(mData), GetRegionNames(iQtdRegioes, "D_R", "_Admitidos"));
	
	mData = dbase.GetVar(GetRegionNames(iQtdRegioes, "R", "_star_Admitidos"));
	dbase.Append(diff(mData), GetRegionNames(iQtdRegioes, "D_R", "_star_Admitidos"));
	
	mData = dbase.GetVar(GetRegionNames(iQtdRegioes, "R", "_Desligados"));
	dbase.Append(diff(mData), GetRegionNames(iQtdRegioes, "D_R", "_Desligados"));
	
	mData = dbase.GetVar(GetRegionNames(iQtdRegioes, "R", "_star_Desligados"));
	dbase.Append(diff(mData), GetRegionNames(iQtdRegioes, "D_R", "_star_Desligados"));

	
	dbase.Save("../../Database/teste_1adjusted_data.in7");	
	   	// println(GetRegionNames(iQtdRegioes, "R", "_Desligados"));
		
//		mData = model.GetVar(GetRegionNames(iQtdRegioes, "R", "_Desligados"));
////		println(mData);
////		println(mW[][iCont-1]);
////		println(mData*mW[][iCont-1]);
////		break;
//		model.Append(mData*mW[][iCont-1], {"star_Desligados"});
//		mData = model.GetVar(GetRegionNames(iQtdRegioes, "R", "_Admitidos"));
//		model.Append(mData*mW[][iCont-1], {"star_Admitidos"});
//		println("Concluido construcao das variaveis star para a regiao ", iCont);
//
//
//		println("Iniciando construcao da variavel Delta para a regiao ", iCont);
//		// CONTRUCAO DAS VARIAVEIS DELTA
//		mData =	model.GetVar(sprint("R", iCont, "_Admitidos"));
//		mData = mData ~ model.GetVar(sprint("R", iCont, "_Desligados"));
//		mData = mData ~ model.GetVar("star_Admitidos");
//		mData = mData ~ model.GetVar("star_Desligados");
//
//		model.Append(diff(mData), {sprint("D_R", iCont, "_Admitidos"), sprint("D_R", iCont, "_Desligados"), "D_star_Admitidos", "D_star_Desligados"});
//		println("Concluido construcao da variavel Delta para a regiao ", iCont);
			
		
	delete dbase, mW;

}
