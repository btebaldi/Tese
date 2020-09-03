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

	decl txBancoDados, txBancoDados_dataAjusted, txBancoDados_MacroData;
	txBancoDados = "../../Database/teste_3_data.in7";
	txBancoDados_dataAjusted = "../../Database/teste_3adjusted_data.in7";
	txBancoDados_MacroData = "../../Database/teste_3Macrovars_data.in7";
	
	decl dbase = new Database();

	println("Abre banco de dados");
	dbase.Load(txBancoDados);

	//exit(0);

	println("Trocando o nome das colunas Y's");
	dbase.Rename({"R1_Admitidos", "R1_Desligados", "R2_Admitidos", "R2_Desligados", "R3_Admitidos", "R3_Desligados"}, {"Ya", "Yb", "Yc", "Yd", "Ye", "Yf"});

	println("Removendo colunas Z's");
	dbase.Remove({"Za", "Zb", "Zc"});

	println("Salva novo banco de dados");
	dbase.Save(txBancoDados_dataAjusted);

 	println("Abre banco de dados");
	dbase.Load(txBancoDados);

	println("Remove colunas Y's");
	dbase.Remove({"Ya", "Yb", "Yc", "Yd", "Ye", "Yf"});

	println("Trocando o nome das colunas Z's");
	dbase.Rename({"DMacro1", "DMacro2", "DMacro3"}, {"Za", "Zb", "Zc"});

	mData = dbase.GetVar({"DMacro1", "DMacro2", "DMacro3"});
	
	dbase.Append(cumsum(mData, <1>), {"Macro1", "Macro2", "Macro3"});

	dbase.Save(txBancoDados_MacroData);	
		
	delete dbase, mData;

	println("END OF FILE");
}
